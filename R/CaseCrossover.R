# @file CaseCrossover.R
#
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

#' @export
runCaseCrossover <- function(connectionDetails,
                             cdmDatabaseSchema,
                             oracleTempSchema = NULL,
                             outcomeDatabaseSchema = cdmDatabaseSchema,
                             outcomeTable = "cohort",
                             exposureDatabaseSchema = cdmDatabaseSchema,
                             exposureTable = "drug_era",
                             nestingCohortDatabaseSchema = cdmDatabaseSchema,
                             nestingCohortTable = "condition_era",
                             outputFolder,
                             cdmVersion = "5",
                             maxCores = 4) {
    start <- Sys.time()

    ccrFolder <- file.path(outputFolder, "caseCrossover")
    if (!file.exists(ccrFolder))
        dir.create(ccrFolder)

    ccrSummaryFile <- file.path(outputFolder, "ccrSummary.rds")
    if (!file.exists(ccrSummaryFile)) {
        allControls <- read.csv(file.path(outputFolder , "allControls.csv"))
        allControls <- unique(allControls[, c("targetId", "outcomeId", "nestingId")])
        eonList <- list()
        for (i in 1:nrow(allControls)) {
            eonList[[length(eonList) + 1]] <- CaseCrossover::createExposureOutcomeNestingCohort(exposureId = allControls$targetId[i],
                                                                                                outcomeId = allControls$outcomeId[i],
                                                                                                nestingCohortId = allControls$nestingId[i])
        }
        ccrAnalysisListFile <- system.file("settings", "ccrAnalysisSettings.txt", package = "MethodsLibraryPleEvaluation")
        ccrAnalysisList <- CaseCrossover::loadCcrAnalysisList(ccrAnalysisListFile)
        ccrResult <- CaseCrossover::runCcrAnalyses(connectionDetails = connectionDetails,
                                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                                   oracleTempSchema = oracleTempSchema,
                                                   exposureDatabaseSchema = exposureDatabaseSchema,
                                                   exposureTable = exposureTable,
                                                   outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                   outcomeTable = outcomeTable,
                                                   nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
                                                   nestingCohortTable = nestingCohortTable,
                                                   ccrAnalysisList = ccrAnalysisList,
                                                   exposureOutcomeNestingCohortList = eonList,
                                                   outputFolder = ccrFolder,
                                                   getDbCaseCrossoverDataThreads = min(3, maxCores),
                                                   selectSubjectsToIncludeThreads = min(5, maxCores),
                                                   getExposureStatusThreads = min(5, maxCores),
                                                   fitCaseCrossoverModelThreads = min(5, maxCores))
        ccrSummary <- CaseCrossover::summarizeCcrAnalyses(ccrResult, ccrFolder)
        saveRDS(ccrSummary, ccrSummaryFile)
    }
    delta <- Sys.time() - start
    writeLines(paste("Completed case-crossover analyses in", signif(delta, 3), attr(delta, "units")))
}

#' @export
createCaseCrossoverSettings <- function(fileName) {
    getDbCaseCrossoverDataArgs1 <- CaseCrossover::createGetDbCaseCrossoverDataArgs(useNestingCohort = FALSE,
                                                                                   maxNestingCohortSize = 1e8,
                                                                                   maxCasesPerOutcome = 1e6)

    selectSubjectsToIncludeArgs1 <- CaseCrossover::createSelectSubjectsToIncludeArgs(firstOutcomeOnly = FALSE,
                                                                                     washoutPeriod = 365)

    getExposureStatusArgs1 <- CaseCrossover::createGetExposureStatusArgs(firstExposureOnly = FALSE,
                                                                         riskWindowStart = 0,
                                                                         riskWindowEnd = 0,
                                                                         controlWindowOffsets = -30)

    ccrAnalysis1 <- CaseCrossover::createCcrAnalysis(analysisId = 1,
                                                     description = "Simple case-crossover, -30 days",
                                                     getDbCaseCrossoverDataArgs = getDbCaseCrossoverDataArgs1,
                                                     selectSubjectsToIncludeArgs = selectSubjectsToIncludeArgs1,
                                                     getExposureStatusArgs = getExposureStatusArgs1)

	getExposureStatusArgs2 <- CaseCrossover::createGetExposureStatusArgs(firstExposureOnly = FALSE,
                                                                         riskWindowStart = 0,
                                                                         riskWindowEnd = 0,
                                                                         controlWindowOffsets = -180)

    ccrAnalysis2 <- CaseCrossover::createCcrAnalysis(analysisId = 2,
                                                     description = "Simple case-crossover, -180 days",
                                                     getDbCaseCrossoverDataArgs = getDbCaseCrossoverDataArgs1,
                                                     selectSubjectsToIncludeArgs = selectSubjectsToIncludeArgs1,
                                                     getExposureStatusArgs = getExposureStatusArgs2)

    getDbCaseCrossoverDataArgs2 <- CaseCrossover::createGetDbCaseCrossoverDataArgs(useNestingCohort = TRUE,
                                                                                   getTimeControlData = TRUE,
                                                                                   maxNestingCohortSize = 1e8,
                                                                                   maxCasesPerOutcome = 1e6)

    ccrAnalysis3 <- CaseCrossover::createCcrAnalysis(analysisId = 3,
                                                     description = "Nested case-crossover, -30 days",
                                                     getDbCaseCrossoverDataArgs = getDbCaseCrossoverDataArgs2,
                                                     selectSubjectsToIncludeArgs = selectSubjectsToIncludeArgs1,
                                                     getExposureStatusArgs = getExposureStatusArgs1)

    ccrAnalysis4 <- CaseCrossover::createCcrAnalysis(analysisId = 4,
                                                     description = "Nested case-crossover, -180 days",
                                                     getDbCaseCrossoverDataArgs = getDbCaseCrossoverDataArgs2,
                                                     selectSubjectsToIncludeArgs = selectSubjectsToIncludeArgs1,
                                                     getExposureStatusArgs = getExposureStatusArgs2)

    matchingCriteria1 <- CaseCrossover::createMatchingCriteria(matchOnAge = TRUE,
                                                               ageCaliper = 2,
                                                               matchOnGender = TRUE)

    selectSubjectsToIncludeArgs2 <- CaseCrossover::createSelectSubjectsToIncludeArgs(firstOutcomeOnly = FALSE,
                                                                                     washoutPeriod = 365,
                                                                                     matchingCriteria = matchingCriteria1)

    ccrAnalysis5 <- CaseCrossover::createCcrAnalysis(analysisId = 5,
                                                     description = "Nested case-time-control, -30 days",
                                                     getDbCaseCrossoverDataArgs = getDbCaseCrossoverDataArgs2,
                                                     selectSubjectsToIncludeArgs = selectSubjectsToIncludeArgs2,
                                                     getExposureStatusArgs = getExposureStatusArgs1)

    ccrAnalysis6 <- CaseCrossover::createCcrAnalysis(analysisId = 6,
                                                     description = "Nested case-time-control, -180 days",
                                                     getDbCaseCrossoverDataArgs = getDbCaseCrossoverDataArgs2,
                                                     selectSubjectsToIncludeArgs = selectSubjectsToIncludeArgs2,
                                                     getExposureStatusArgs = getExposureStatusArgs2)

    ccrAnalysisList <- list(ccrAnalysis1, ccrAnalysis2, ccrAnalysis3, ccrAnalysis4, ccrAnalysis5, ccrAnalysis6)

    if (!missing(fileName) && !is.null(fileName)) {
        CaseCrossover::saveCcrAnalysisList(ccrAnalysisList, fileName)
    }
    invisible(ccrAnalysisList)
}
