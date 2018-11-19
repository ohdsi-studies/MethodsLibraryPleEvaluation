{DEFAULT @cdm_database_schema = 'cdm' } 
{DEFAULT @exposure_ids = '' } 
{DEFAULT @exposure_database_schema = 'cdm' } 
{DEFAULT @exposure_table = 'exposures' } 
{DEFAULT @max_gap = 30 }
{DEFAULT @min_frequency = 11}

-- Find most frequent lengh of exposure (>0) per drug concept ID
SELECT days_exposed,
	drug_concept_id
INTO #most_frequent_length
FROM (
	SELECT days_exposed,
		drug_concept_id,
		ROW_NUMBER() OVER (
			PARTITION BY drug_concept_id ORDER BY frequency DESC
			) AS rn
	FROM (
		SELECT days_exposed,
			drug_concept_id,
			COUNT(*) AS frequency
		FROM (
			SELECT DATEDIFF(DAY, drug_exposure_start_date, drug_exposure_end_date) days_exposed,
				drug_concept_id
			FROM @cdm_database_schema.drug_exposure
{@exposure_ids != ''} ? {
			INNER JOIN @cdm_database_schema.concept_ancestor
				ON drug_concept_id = descendant_concept_id
			WHERE ancestor_concept_id IN (@exposure_ids)
				AND 
} : {
			WHERE
}
			drug_exposure_start_date != drug_exposure_end_date
			) days_per_exposure
		GROUP BY days_exposed,
			drug_concept_id
		HAVING COUNT(*) >= @min_frequency
		) aggregated_frequencies
	) ordered_frequencies
WHERE rn = 1;

-- Create drug exposure table with imputed values
--HINT DISTRIBUTE_ON_KEY(person_id)
SELECT person_id,
	ancestor_concept_id AS concept_id,
	drug_exposure_start_date AS exposure_start_date,
	CASE 
		WHEN drug_exposure_start_date = drug_exposure_end_date
			THEN CASE 
					WHEN most_frequent_length.days_exposed IS NULL
						THEN drug_exposure_end_date
					ELSE DATEADD(DAY, most_frequent_length.days_exposed, drug_exposure_end_date)
					END
		ELSE drug_exposure_end_date
		END AS exposure_end_date
INTO #exposure
FROM @cdm_database_schema.drug_exposure
INNER JOIN @cdm_database_schema.concept_ancestor
	ON drug_concept_id = descendant_concept_id
LEFT JOIN #most_frequent_length most_frequent_length
	ON drug_exposure.drug_concept_id = most_frequent_length.drug_concept_id
{@exposure_ids != ''} ? {
WHERE ancestor_concept_id IN (@exposure_ids)
}
;

-- Create era table
IF OBJECT_ID('@exposure_database_schema.@exposure_table', 'U') IS NOT NULL
	DROP TABLE @exposure_database_schema.@exposure_table;

--HINT DISTRIBUTE_ON_KEY(subject_id)	
SELECT ends.person_id AS subject_id,
	ends.concept_id AS cohort_definition_id,
	MIN(exposure_start_date) AS cohort_start_date,
	ends.era_end_date AS cohort_end_date
INTO @exposure_database_schema.@exposure_table
FROM (
	SELECT exposure.person_id,
		exposure.concept_id,
		exposure.exposure_start_date,
		MIN(events.end_date) AS era_end_date
	FROM #exposure exposure
	JOIN (
		--cteEndDates
		SELECT person_id,
			concept_id,
			DATEADD(DAY, - 1 * @max_gap, event_date) AS end_date -- unpad the end date by @max_gap
		FROM (
			SELECT person_id,
				concept_id,
				event_date,
				event_type,
				MAX(start_ordinal) OVER (
					PARTITION BY person_id,
					concept_id ORDER BY event_date,
						event_type ROWS UNBOUNDED PRECEDING
					) AS start_ordinal,
				ROW_NUMBER() OVER (
					PARTITION BY person_id,
					concept_id ORDER BY event_date,
						event_type
					) AS overall_ord -- this re-numbers the inner UNION so all rows are numbered ordered by the event date
			FROM (
				-- select the start dates, assigning a row number to each
				SELECT person_id,
					concept_id,
					exposure_start_date AS event_date,
					0 AS event_type,
					ROW_NUMBER() OVER (
						PARTITION BY person_id,
						concept_id ORDER BY exposure_start_date
						) AS start_ordinal
				FROM #exposure
				
				UNION ALL
				
				-- add the end dates with NULL as the row number, padding the end dates by @max_gap to allow a grace period for overlapping ranges.
				SELECT person_id,
					concept_id,
					DATEADD(day, @max_gap, exposure_end_date),
					1 AS event_type,
					NULL
				FROM #exposure
				) rawdata
			) events
		WHERE 2 * events.start_ordinal - events.overall_ord = 0
		) events
		ON exposure.person_id = events.person_id
			AND exposure.concept_id = events.concept_id
			AND events.end_date >= exposure.exposure_end_date
	GROUP BY exposure.person_id,
		exposure.concept_id,
		exposure.exposure_start_date
	) ends
GROUP BY ends.person_id,
	concept_id,
	ends.era_end_date;
