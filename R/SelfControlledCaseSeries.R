# @file SelfControlledCaseSeries.R
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
runSelfControlledCaseSeries <- function(connectionDetails,
                                        cdmDatabaseSchema,
                                        oracleTempSchema = NULL,
                                        outcomeDatabaseSchema = cdmDatabaseSchema,
                                        outcomeTable = "cohort",
                                        exposureDatabaseSchema = cdmDatabaseSchema,
                                        exposureTable = "drug_era",
                                        outputFolder,
                                        cdmVersion = "5",
                                        maxCores) {
    start <- Sys.time()
    sccsFolder <- file.path(outputFolder, "selfControlledCaseSeries")
    if (!file.exists(sccsFolder))
        dir.create(sccsFolder)

    sccsSummaryFile <- file.path(outputFolder, "sccsSummary.rds")
    if (!file.exists(sccsSummaryFile)) {
        allControls <- read.csv(file.path(outputFolder , "allControls.csv"))
        allControls <- unique(allControls[, c("targetId", "outcomeId")])
        eoList <- list()
        for (i in 1:nrow(allControls)) {
            eoList[[length(eoList) + 1]] <- SelfControlledCaseSeries::createExposureOutcome(exposureId = allControls$targetId[i],
                                                                                            outcomeId = allControls$outcomeId[i])
        }
        sccsAnalysisListFile <- system.file("settings", "sccsAnalysisSettings.txt", package = "MethodsLibraryPleEvaluation")
        sccsAnalysisList <- SelfControlledCaseSeries::loadSccsAnalysisList(sccsAnalysisListFile)
        sccsResult <- SelfControlledCaseSeries::runSccsAnalyses(connectionDetails = connectionDetails,
                                                                cdmDatabaseSchema = cdmDatabaseSchema,
                                                                oracleTempSchema = oracleTempSchema,
                                                                exposureDatabaseSchema = exposureDatabaseSchema,
                                                                exposureTable = exposureTable,
                                                                outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                                outcomeTable = outcomeTable,
                                                                sccsAnalysisList = sccsAnalysisList,
                                                                exposureOutcomeList = eoList,
                                                                cdmVersion = cdmVersion,
                                                                outputFolder = sccsFolder,
                                                                combineDataFetchAcrossOutcomes = TRUE,
                                                                compressSccsEraDataFiles = TRUE,
                                                                getDbSccsDataThreads = min(3, maxCores),
                                                                createSccsEraDataThreads = min(5, maxCores),
                                                                fitSccsModelThreads = min(max(1, floor(maxCores/8)), 4),
                                                                cvThreads =  min(10, maxCores))

        sccsSummary <- SelfControlledCaseSeries::summarizeSccsAnalyses(sccsResult, sccsFolder)
        saveRDS(sccsSummary, sccsSummaryFile)
    }
    delta <- Sys.time() - start
    writeLines(paste("Completed SCCS analyses in", signif(delta, 3), attr(delta, "units")))
}

#' @export
createSccsSettings <- function(fileName) {

    getDbSccsDataArgs1 <- SelfControlledCaseSeries::createGetDbSccsDataArgs(useCustomCovariates = FALSE,
                                                                            deleteCovariatesSmallCount = 100,
                                                                            studyStartDate = "",
                                                                            studyEndDate = "",
                                                                            exposureIds = c(),
                                                                            maxCasesPerOutcome = 250000)

    covarExposureOfInt1 <- SelfControlledCaseSeries::createCovariateSettings(label = "Exposure of interest",
                                                                             includeCovariateIds = "exposureId",
                                                                             start = 1,
                                                                             end = 0,
                                                                             addExposedDaysToEnd = TRUE)

    createSccsEraDataArgs1 <- SelfControlledCaseSeries::createCreateSccsEraDataArgs(naivePeriod = 365,
                                                                                    firstOutcomeOnly = FALSE,
                                                                                    covariateSettings = covarExposureOfInt1)

    fitSccsModelArgs1 <- SelfControlledCaseSeries::createFitSccsModelArgs()

    sccsAnalysis1 <- SelfControlledCaseSeries::createSccsAnalysis(analysisId = 1,
                                                                  description = "Simple SCCS",
                                                                  getDbSccsDataArgs = getDbSccsDataArgs1,
                                                                  createSccsEraDataArgs = createSccsEraDataArgs1,
                                                                  fitSccsModelArgs = fitSccsModelArgs1)

    covarExposureOfInt2 <- SelfControlledCaseSeries::createCovariateSettings(label = "Exposure of interest",
                                                                             includeCovariateIds = "exposureId",
                                                                             start = 0,
                                                                             end = 0,
                                                                             addExposedDaysToEnd = TRUE)

    createSccsEraDataArgs2 <- SelfControlledCaseSeries::createCreateSccsEraDataArgs(naivePeriod = 365,
                                                                                    firstOutcomeOnly = FALSE,
                                                                                    covariateSettings = covarExposureOfInt2)

    sccsAnalysis2 <- SelfControlledCaseSeries::createSccsAnalysis(analysisId = 2,
                                                                  description = "Including day 0",
                                                                  getDbSccsDataArgs = getDbSccsDataArgs1,
                                                                  createSccsEraDataArgs = createSccsEraDataArgs2,
                                                                  fitSccsModelArgs = fitSccsModelArgs1)

    covarPreExposure = SelfControlledCaseSeries::createCovariateSettings(label = "Pre-exposure",
                                                                         includeCovariateIds = "exposureId",
                                                                         start = -60,
                                                                         end = -1,
                                                                         splitPoints = c(-30))

    createSccsEraDataArgs3 <- SelfControlledCaseSeries::createCreateSccsEraDataArgs(naivePeriod = 365,
                                                                                    firstOutcomeOnly = FALSE,
                                                                                    covariateSettings = list(covarExposureOfInt1,
                                                                                                             covarPreExposure))

    sccsAnalysis3 <- SelfControlledCaseSeries::createSccsAnalysis(analysisId = 3,
                                                                  description = "Using pre-exposure window",
                                                                  getDbSccsDataArgs = getDbSccsDataArgs1,
                                                                  createSccsEraDataArgs = createSccsEraDataArgs3,
                                                                  fitSccsModelArgs = fitSccsModelArgs1)

    ageSettings <- SelfControlledCaseSeries::createAgeSettings(includeAge = TRUE, ageKnots = 5, computeConfidenceIntervals = FALSE)

    seasonalitySettings <- SelfControlledCaseSeries::createSeasonalitySettings(includeSeasonality = TRUE, seasonKnots = 5, computeConfidenceIntervals = FALSE)

    createSccsEraDataArgs4 <- SelfControlledCaseSeries::createCreateSccsEraDataArgs(naivePeriod = 365,
                                                                                    firstOutcomeOnly = FALSE,
                                                                                    covariateSettings = covarExposureOfInt1,
                                                                                    ageSettings = ageSettings,
                                                                                    seasonalitySettings = seasonalitySettings,
                                                                                    minCasesForAgeSeason = 10000)

    sccsAnalysis4 <- SelfControlledCaseSeries::createSccsAnalysis(analysisId = 4,
                                                                  description = "Using age and season",
                                                                  getDbSccsDataArgs = getDbSccsDataArgs1,
                                                                  createSccsEraDataArgs = createSccsEraDataArgs4,
                                                                  fitSccsModelArgs = fitSccsModelArgs1)

    createSccsEraDataArgs5 <- SelfControlledCaseSeries::createCreateSccsEraDataArgs(naivePeriod = 365,
                                                                                    firstOutcomeOnly = FALSE,
                                                                                    covariateSettings = covarExposureOfInt1,
                                                                                    eventDependentObservation = TRUE)

    sccsAnalysis5 <- SelfControlledCaseSeries::createSccsAnalysis(analysisId = 5,
                                                                  description = "Using event-dependent observation",
                                                                  getDbSccsDataArgs = getDbSccsDataArgs1,
                                                                  createSccsEraDataArgs = createSccsEraDataArgs5,
                                                                  fitSccsModelArgs = fitSccsModelArgs1)

    covarAllDrugs = SelfControlledCaseSeries::createCovariateSettings(label = "All other exposures",
                                                                      excludeCovariateIds = "exposureId",
                                                                      stratifyById = TRUE,
                                                                      start = 1,
                                                                      end = 0,
                                                                      addExposedDaysToEnd = TRUE,
                                                                      allowRegularization = TRUE)

    createSccsEraDataArgs6 <- SelfControlledCaseSeries::createCreateSccsEraDataArgs(naivePeriod = 365,
                                                                                    firstOutcomeOnly = FALSE,
                                                                                    covariateSettings = list(covarExposureOfInt1,
                                                                                                             covarAllDrugs))
    prior = Cyclops::createPrior("laplace", useCrossValidation = TRUE)
    control = Cyclops::createControl(cvType = "auto",
                                     selectorType = "byPid",
                                     startingVariance = 0.01,
                                     noiseLevel = "quiet",
                                     tolerance = 2e-07)
    fitSccsModelArgs2 <- SelfControlledCaseSeries::createFitSccsModelArgs(prior = prior, control = control)

    sccsAnalysis6 <- SelfControlledCaseSeries::createSccsAnalysis(analysisId = 6,
                                                                  description = "Using all other exposures",
                                                                  getDbSccsDataArgs = getDbSccsDataArgs1,
                                                                  createSccsEraDataArgs = createSccsEraDataArgs6,
                                                                  fitSccsModelArgs = fitSccsModelArgs2)

    sccsAnalysisList <- list(sccsAnalysis1, sccsAnalysis2, sccsAnalysis3, sccsAnalysis4, sccsAnalysis5, sccsAnalysis6)

    if (!missing(fileName) && !is.null(fileName)) {
        SelfControlledCaseSeries::saveSccsAnalysisList(sccsAnalysisList, fileName)
    }
    invisible(sccsAnalysisList)
}
