# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of MethodsLibraryPleEvaluation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Execute the Study
#'
#' @details
#' This function executes the Study.
#'
#' The \code{createCohorts}, \code{synthesizePositiveControls}, \code{runAnalyses}, and \code{runDiagnostics} arguments
#' are intended to be used to run parts of the full study at a time, but none of the parts are considerd to be optional.
#'
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param exportFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param databaseId           A short string for identifying the database (e.g.
#'                             'Synpuf').
#' @param cdmVersion           Version of the Common Data Model used. Currently only version 5 is supported.
#'
#' @export
createGeneralCharacterization <- function(connectionDetails,
                                          cdmDatabaseSchema,
                                          oracleTempSchema = NULL,
                                          exportFolder,
                                          databaseId,
                                          cdmVersion = "5") {
    start <- Sys.time()
    if (!file.exists(exportFolder)) {
        dir.create(exportFolder, recursive = TRUE)
    }
    conn <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(conn))

    # Population count ------------------------------------------------------------
    ParallelLogger::logInfo("Counting overall population")
    sql <- "
    SELECT COUNT(*) AS subject_count
    FROM @cdm_database_schema.person;
    "
    popCount <- DatabaseConnector::renderTranslateQuerySql(conn,
                                                           sql,
                                                           cdm_database_schema = cdmDatabaseSchema)
    colnames(popCount) <- tolower(colnames(popCount))
    popCount$database_id <- databaseId
    write.csv(popCount, file.path(exportFolder, sprintf("population_count_%s.csv", databaseId)), row.names = FALSE)

    # Observation duration --------------------------------------------------------
    ParallelLogger::logInfo("Counting observation duration")
    sql <- "
    SELECT observation_years,
        COUNT(*) AS subject_count
    FROM (
        SELECT ROUND(SUM(DATEDIFF(DAY, observation_period_start_date, observation_period_end_date)) / 365.25, 0) AS observation_years
        FROM @cdm_database_schema.observation_period
        GROUP BY person_id
    ) tmp
    GROUP BY observation_years
    ORDER BY observation_years;
    "
    obsDuration <- DatabaseConnector::renderTranslateQuerySql(conn,
                                                              sql,
                                                              cdm_database_schema = cdmDatabaseSchema)
    colnames(obsDuration) <- tolower(colnames(obsDuration))
    obsDuration$database_id <- databaseId
    write.csv(obsDuration, file.path(exportFolder, sprintf("observation_duration_%s.csv", databaseId)), row.names = FALSE)

    # Gender ------------------------------------------------------------
    ParallelLogger::logInfo("Counting gender")
    sql <- "
    SELECT concept_name AS gender,
        subject_count
    FROM (
        SELECT gender_concept_id,
            COUNT(*) AS subject_count
        FROM @cdm_database_schema.person
        GROUP BY gender_concept_id
    ) tmp
    INNER JOIN @cdm_database_schema.concept
        ON gender_concept_id = concept_id
    ORDER BY concept_name;
    "
    gender <- DatabaseConnector::renderTranslateQuerySql(conn,
                                                         sql,
                                                         cdm_database_schema = cdmDatabaseSchema)
    colnames(gender) <- tolower(colnames(gender))
    gender$database_id <- databaseId
    write.csv(gender, file.path(exportFolder, sprintf("gender_%s.csv", databaseId)), row.names = FALSE)

    # Visit types -----------------------------------------------------
    ParallelLogger::logInfo("Counting visit types")
    sql <- "
    SELECT concept_name AS visit_type,
        visit_count
    FROM (
        SELECT visit_concept_id,
            COUNT_BIG(*) AS visit_count
        FROM @cdm_database_schema.visit_occurrence
        GROUP BY visit_concept_id
    ) tmp
    INNER JOIN @cdm_database_schema.concept
        ON visit_concept_id = concept_id
    ORDER BY concept_name;
    "
    visitTypes <- DatabaseConnector::renderTranslateQuerySql(conn,
                                                             sql,
                                                             cdm_database_schema = cdmDatabaseSchema)
    colnames(visitTypes) <- tolower(colnames(visitTypes))
    visitTypes$database_id <- databaseId
    write.csv(visitTypes, file.path(exportFolder, sprintf("visit_type_%s.csv", databaseId)), row.names = FALSE)

    # Age observed ----------------------------------------
    ParallelLogger::logInfo("Counting age observed")
    ages <- data.frame(age = 0:120)
    DatabaseConnector::insertTable(connection = conn,
                                   tableName = "#ages",
                                   data = ages,
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)

    sql <- "
    SELECT age,
        COUNT(DISTINCT person.person_id) AS subject_count
    FROM (
        SELECT person_id,
            DATEFROMPARTS(year_of_birth, ISNULL(month_of_birth, 6), ISNULL(day_of_birth, 15)) AS date_of_birth
        FROM @cdm_database_schema.person
    ) person
    CROSS JOIN #ages
    INNER JOIN @cdm_database_schema.observation_period
        ON person.person_id = observation_period.person_id
        AND DATEADD(YEAR, age + 1, date_of_birth) >= observation_period_start_date
        AND DATEADD(YEAR, age, date_of_birth) <= observation_period_end_date
    GROUP BY age
    ORDER BY age;
    "

    age <- DatabaseConnector::renderTranslateQuerySql(conn,
                                                      sql,
                                                      cdm_database_schema = cdmDatabaseSchema)
    colnames(age) <- tolower(colnames(age))
    age$database_id <- databaseId
    write.csv(age, file.path(exportFolder, sprintf("age_observed_%s.csv", databaseId)), row.names = FALSE)
    DatabaseConnector::renderTranslateExecuteSql(conn, "TRUNCATE TABLE #ages; DROP TABLE #ages", progressBar = FALSE, reportOverallTime = FALSE)


    # Year observed ----------------------------------------
    ParallelLogger::logInfo("Counting calendar years observed")
    years <- data.frame(calendar_year = 1900:2100)
    DatabaseConnector::insertTable(connection = conn,
                                   tableName = "#years",
                                   data = years,
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)

    sql <- "
    SELECT calendar_year,
        COUNT(DISTINCT person_id) AS subject_count
    FROM (
        SELECT calendar_year,
            DATEFROMPARTS(calendar_year, 1, 1) AS year_start,
            DATEFROMPARTS(calendar_year, 12, 31) AS year_end
        FROM #years
    ) years
    INNER JOIN @cdm_database_schema.observation_period
        ON year_end >= observation_period_start_date
            AND year_start <= observation_period_end_date
    GROUP BY calendar_year
    ORDER BY calendar_year;
    "

    calenderYears <- DatabaseConnector::renderTranslateQuerySql(conn,
                                                      sql,
                                                      cdm_database_schema = cdmDatabaseSchema)
    colnames(calenderYears) <- tolower(colnames(calenderYears))
    calenderYears$database_id <- databaseId
    write.csv(calenderYears, file.path(exportFolder, sprintf("calendar_year_observed_%s.csv", databaseId)), row.names = FALSE)
    DatabaseConnector::renderTranslateExecuteSql(conn, "TRUNCATE TABLE #years; DROP TABLE #years", progressBar = FALSE, reportOverallTime = FALSE)

    delta <- Sys.time() - start
    writeLines(paste("Characterization took", signif(delta, 3), attr(delta, "units")))
}
