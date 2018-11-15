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
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    oracleTempSchema = NULL,
                    outcomeDatabaseSchema,
                    outcomeTable,
                    nestingCohortDatabaseSchema,
                    nestingCohortTable,
                    outputFolder,
                    databaseName,
                    maxCores = 1,
                    cdmVersion = "5",
                    createNegativeControlCohorts = TRUE,
                    synthesizePositiveControls = TRUE,
                    runCohortMethod = TRUE,
                    runSelfControlledCaseSeries = TRUE,
                    runSelfControlledCohort = TRUE,
                    runCaseControl = TRUE,
                    runCaseCrossover = TRUE,
                    packageResults = TRUE) {
    if (!file.exists(outputFolder)) {
        dir.create(outputFolder, recursive = TRUE)
    }

    ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))

    # Create outcome and nesting cohorts for positive and negative controls ----------------------------------

    if (createNegativeControlCohorts) {
        ParallelLogger::logInfo("Creating negative control cohorts")
        MethodEvaluation::createReferenceSetCohorts(connectionDetails = connectionDetails,
                                                    oracleTempSchema = oracleTempSchema,
                                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                                    outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                    outcomeTable = outcomeTable,
                                                    nestingDatabaseSchema = nestingCohortDatabaseSchema,
                                                    nestingTable = nestingCohortTable,
                                                    referenceSet = "ohdsiMethodsBenchmark")
    }

    if (synthesizePositiveControls) {
        ParallelLogger::logInfo("Synthesizing positive controls")
        MethodEvaluation::synthesizePositiveControls(connectionDetails = connectionDetails,
                                                     oracleTempSchema = oracleTempSchema,
                                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                                     outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                     outcomeTable = outcomeTable,
                                                     maxCores = maxCores,
                                                     workFolder = outputFolder,
                                                     summaryFileName = file.path(outputFolder, "allControls.csv"),
                                                     referenceSet = "ohdsiMethodsBenchmark")
    }

    # Run methods on negative and positive controls ----------------------------------------------------------

    if (runCohortMethod) {
        ParallelLogger::logInfo("Running CohortMethod")
        runCohortMethod(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        oracleTempSchema = oracleTempSchema,
                        outcomeDatabaseSchema = outcomeDatabaseSchema,
                        outcomeTable = outcomeTable,
                        outputFolder = outputFolder,
                        cdmVersion = cdmVersion,
                        maxCores = 32)
    }

    if (runSelfControlledCaseSeries) {
        ParallelLogger::logInfo("Running SelfControlledCaseSeries")
        runSelfControlledCaseSeries(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                    oracleTempSchema = oracleTempSchema,
                                    outcomeDatabaseSchema = outcomeDatabaseSchema,
                                    outcomeTable = outcomeTable,
                                    outputFolder = outputFolder,
                                    cdmVersion = cdmVersion)
    }

    if (runSelfControlledCohort) {
        ParallelLogger::logInfo("Running SelfControlledCohort")
        runSelfControlledCohort(connectionDetails = connectionDetails,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                oracleTempSchema = oracleTempSchema,
                                outcomeDatabaseSchema = outcomeDatabaseSchema,
                                outcomeTable = outcomeTable,
                                outputFolder = outputFolder,
                                cdmVersion = cdmVersion)
    }

    if (runCaseControl) {
        ParallelLogger::logInfo("Running CaseControl")
        runCaseControl(connectionDetails = connectionDetails,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       outcomeDatabaseSchema = outcomeDatabaseSchema,
                       outcomeTable = outcomeTable,
                       nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
                       nestingCohortTable = nestingCohortTable,
                       outputFolder = outputFolder,
                       cdmVersion = cdmVersion,
                       maxCores = 20)
    }

    if (runCaseCrossover) {
        ParallelLogger::logInfo("Running CaseCrossover")
        runCaseCrossover(connectionDetails = connectionDetails,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         oracleTempSchema = oracleTempSchema,
                         outcomeDatabaseSchema = outcomeDatabaseSchema,
                         outcomeTable = outcomeTable,
                         nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
                         nestingCohortTable = nestingCohortTable,
                         outputFolder = outputFolder,
                         cdmVersion = cdmVersion,
                         maxCores = 20)
    }

    if (packageResults) {
        ParallelLogger::logInfo("Packaging results")
        packageResults(outputFolder = outputFolder,
                       exportFolder = file.path(outputFolder, "export"),
                       databaseName = databaseName)
    }
}
