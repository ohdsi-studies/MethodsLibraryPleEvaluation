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
packageResults <- function(outputFolder,
                           exportFolder = file.path(outputFolder, "export"),
                           databaseName) {
    if (!file.exists(exportFolder)) {
        dir.create(exportFolder, recursive = TRUE)
    }
    controlSummary <- read.csv(file.path(outputFolder, "allControls.csv"))

    # CohortMethod -----------------------------------------------------------------------------------------------
    estimates <- readRDS(file.path(outputFolder, "cmSummary.rds"))

    # Drop controls for picosulfate sodium - lubiprostone - acute pancreatitis. T and C have now been switched:
    outcomeIds <- controlSummary$outcomeId[controlSummary$targetId == 19025115 &
                                               controlSummary$comparatorId == 987366 &
                                               controlSummary$oldOutcomeId == 1]
    estimates <- estimates[!(estimates$targetId == 19025115 &
                                 estimates$comparatorId == 987366 &
                                 estimates$outcomeId %in% outcomeIds), ]

    estimates <- data.frame(analysisId = estimates$analysisId,
                            targetId = estimates$targetId,
                            outcomeId = estimates$outcomeId,
                            logRr = estimates$logRr,
                            seLogRr = estimates$seLogRr,
                            ci95Lb = estimates$ci95lb,
                            ci95Ub = estimates$ci95ub)

    cmAnalysisListFile <- system.file("settings", "cmAnalysisSettings.txt", package = "MethodsLibraryPleEvaluation")
    cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
    analysisId <- unlist(ParallelLogger::selectFromList(cmAnalysisList, "analysisId"))
    description <- unlist(ParallelLogger::selectFromList(cmAnalysisList, "description"))
    details <- sapply(cmAnalysisList, ParallelLogger::convertSettingsToJson)
    analysisRef <- data.frame(method = "CohortMethod",
                              analysisId = analysisId,
                              description = description,
                              details = details,
                              comparative = TRUE,
                              nesting = FALSE,
                              firstExposureOnly = TRUE)

    MethodEvaluation::packageOhdsiBenchmarkResults(estimates = estimates,
                                                   controlSummary = controlSummary,
                                                   analysisRef = analysisRef,
                                                   databaseName = databaseName,
                                                   exportFolder = exportFolder)

    # CaseControl -----------------------------------------------------------------------------------------------
    estimates <- readRDS(file.path(outputFolder, "ccSummary.rds"))

    estimates <- data.frame(analysisId = estimates$analysisId,
                            targetId = estimates$exposureId,
                            outcomeId = estimates$outcomeId,
                            logRr = estimates$logRr,
                            seLogRr = estimates$seLogRr,
                            ci95Lb = estimates$ci95lb,
                            ci95Ub = estimates$ci95ub)

    ccAnalysisListFile <- system.file("settings", "ccAnalysisSettings.txt", package = "MethodsLibraryPleEvaluation")
    ccAnalysisList <- CaseControl::loadCcAnalysisList(ccAnalysisListFile)
    analysisId <- unlist(ParallelLogger::selectFromList(ccAnalysisList, "analysisId"))
    description <- unlist(ParallelLogger::selectFromList(ccAnalysisList, "description"))
    details <- sapply(ccAnalysisList, ParallelLogger::convertSettingsToJson)
    analysisRef <- data.frame(method = "CaseControl",
                              analysisId = analysisId,
                              description = description,
                              details = details,
                              comparative = FALSE,
                              nesting = FALSE,
                              firstExposureOnly = FALSE)
    analysisRef$nesting[grepl("nesting", analysisRef$description)] <- TRUE

    MethodEvaluation::packageOhdsiBenchmarkResults(estimates = estimates,
                                                   controlSummary = controlSummary,
                                                   analysisRef = analysisRef,
                                                   databaseName = databaseName,
                                                   exportFolder = exportFolder)

    # SelfControlledCohort -----------------------------------------------------------------------------------------------
    estimates <- readRDS(file.path(outputFolder, "sccSummary.rds"))

    estimates <- data.frame(analysisId = estimates$analysisId,
                            targetId = estimates$exposureId,
                            outcomeId = estimates$outcomeId,
                            logRr = estimates$logRr,
                            seLogRr = estimates$seLogRr,
                            ci95Lb = estimates$irrLb95,
                            ci95Ub = estimates$irrUb95)

    sccAnalysisListFile <- system.file("settings", "sccAnalysisSettings.txt", package = "MethodsLibraryPleEvaluation")
    sccAnalysisList <- SelfControlledCohort::loadSccAnalysisList(sccAnalysisListFile)
    analysisId <- unlist(ParallelLogger::selectFromList(sccAnalysisList, "analysisId"))
    description <- unlist(ParallelLogger::selectFromList(sccAnalysisList, "description"))
    details <- sapply(sccAnalysisList, ParallelLogger::convertSettingsToJson)
    analysisRef <- data.frame(method = "SelfControlledCohort",
                              analysisId = analysisId,
                              description = description,
                              details = details,
                              comparative = FALSE,
                              nesting = FALSE,
                              firstExposureOnly = FALSE)

    MethodEvaluation::packageOhdsiBenchmarkResults(estimates = estimates,
                                                   controlSummary = controlSummary,
                                                   analysisRef = analysisRef,
                                                   databaseName = databaseName,
                                                   exportFolder = exportFolder)

    # CaseCrossover -----------------------------------------------------------------------------------------------
    estimates <- readRDS(file.path(outputFolder, "ccrSummary.rds"))

    estimates <- data.frame(analysisId = estimates$analysisId,
                            targetId = estimates$exposureId,
                            outcomeId = estimates$outcomeId,
                            logRr = estimates$logRr,
                            seLogRr = estimates$seLogRr,
                            ci95Lb = estimates$ci95lb,
                            ci95Ub = estimates$ci95ub)

    ccrAnalysisListFile <- system.file("settings", "ccrAnalysisSettings.txt", package = "MethodsLibraryPleEvaluation")
    ccrAnalysisList <- CaseControl::loadCcAnalysisList(ccrAnalysisListFile)
    analysisId <- unlist(ParallelLogger::selectFromList(ccrAnalysisList, "analysisId"))
    description <- unlist(ParallelLogger::selectFromList(ccrAnalysisList, "description"))
    details <- sapply(ccrAnalysisList, ParallelLogger::convertSettingsToJson)
    analysisRef <- data.frame(method = "CaseCrossover",
                              analysisId = analysisId,
                              description = description,
                              details = details,
                              comparative = FALSE,
                              nesting = FALSE,
                              firstExposureOnly = FALSE)
    analysisRef$nesting[grepl("nesting", analysisRef$description)] <- TRUE

    MethodEvaluation::packageOhdsiBenchmarkResults(estimates = estimates,
                                                   controlSummary = controlSummary,
                                                   analysisRef = analysisRef,
                                                   databaseName = databaseName,
                                                   exportFolder = exportFolder)


    # SelfControlledCaseSeries -----------------------------------------------------------------------------------------------
    estimates <- readRDS(file.path(outputFolder, "sccsSummary.rds"))

    estimates <- data.frame(analysisId = estimates$analysisId,
                            targetId = estimates$exposureId,
                            outcomeId = estimates$outcomeId,
                            logRr = estimates$`logRr(Exposure of interest)`,
                            seLogRr = estimates$`seLogRr(Exposure of interest)`,
                            ci95Lb = estimates$`ci95lb(Exposure of interest)`,
                            ci95Ub = estimates$`ci95ub(Exposure of interest)`)

    sccsAnalysisListFile <- system.file("settings", "sccsAnalysisSettings.txt", package = "MethodsLibraryPleEvaluation")
    sccsAnalysisList <- SelfControlledCaseSeries::loadSccsAnalysisList(sccsAnalysisListFile)
    analysisId <- unlist(ParallelLogger::selectFromList(sccsAnalysisList, "analysisId"))
    description <- unlist(ParallelLogger::selectFromList(sccsAnalysisList, "description"))
    details <- sapply(sccsAnalysisList, ParallelLogger::convertSettingsToJson)
    analysisRef <- data.frame(method = "SelfControlledCaseSeries",
                              analysisId = analysisId,
                              description = description,
                              details = details,
                              comparative = FALSE,
                              nesting = FALSE,
                              firstExposureOnly = FALSE)

    MethodEvaluation::packageOhdsiBenchmarkResults(estimates = estimates,
                                                   controlSummary = controlSummary,
                                                   analysisRef = analysisRef,
                                                   databaseName = databaseName,
                                                   exportFolder = exportFolder)


}

