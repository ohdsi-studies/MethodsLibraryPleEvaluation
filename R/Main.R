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
#' @param outcomeDatabaseSchema Schema name where outcome data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param outcomeTable          The name of the table that will be created in the outcomeDatabaseSchema.
#'                             This table will hold the outcome cohorts used in this
#'                             study.
#' @param exposureDatabaseSchema For PanTher only: Schema name where exposure data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param exposureTable          For PanTher only: The name of the table that will be created in the exposureDatabaseSchema
#'                             This table will hold the exposure cohorts used in this
#'                             study.
#' @param nestingCohortDatabaseSchema Schema name where nesting cohort data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param nestingCohortTable          The name of the table that will be created in the nestingCohortDatabaseSchema
#'                             This table will hold the nesting cohorts used in this
#'                             study.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param databaseName         A short string for identifying the database (e.g.
#'                             'Synpuf').
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#' @param cdmVersion           Version of the Common Data Model used. Currently only version 5 is supported.
#' @param createNegativeControlCohorts        Create the negative control outcome and nesting cohorts?
#' @param imputeExposureLengthForPanther      For PanTher only: impute exposure length?
#' @param synthesizePositiveControls          Should positive controls be synthesized?
#' @param runCohortMethod                     Perform the cohort method analyses?
#' @param runSelfControlledCaseSeries                     Perform the SCCS analyses?
#' @param runSelfControlledCohort                     Perform the SCC analyses?
#' @param runCaseControl                     Perform the case-control analyses?
#' @param runCaseCrossover       Perform the case-crossover analyses?
#' @param createCharacterization       Create a general characterization of the database population?
#' @param packageResults       Should results be packaged for later sharing and viewing?
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    oracleTempSchema = NULL,
                    outcomeDatabaseSchema,
                    outcomeTable,
                    exposureDatabaseSchema = cdmDatabaseSchema,
                    exposureTable = "drug_era",
                    nestingCohortDatabaseSchema,
                    nestingCohortTable,
                    outputFolder,
                    databaseName,
                    maxCores = 1,
                    cdmVersion = "5",
                    createNegativeControlCohorts = TRUE,
                    imputeExposureLengthForPanther = TRUE,
                    synthesizePositiveControls = TRUE,
                    runCohortMethod = TRUE,
                    runSelfControlledCaseSeries = TRUE,
                    runSelfControlledCohort = TRUE,
                    runCaseControl = TRUE,
                    runCaseCrossover = TRUE,
                    createCharacterization = TRUE,
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

    if (imputeExposureLengthForPanther && tolower(databaseName) ==  "panther") {
        ParallelLogger::logInfo("Imputing exposure lengths for PanTher database")
        imputeExposureLengthForPanther(connectionDetails = connectionDetails,
                                       oracleTempSchema = oracleTempSchema,
                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                       exposureDatabaseSchema = exposureDatabaseSchema,
                                       exposureTable = exposureTable)
    }


    if (synthesizePositiveControls) {
        ParallelLogger::logInfo("Synthesizing positive controls")
        MethodEvaluation::synthesizePositiveControls(connectionDetails = connectionDetails,
                                                     oracleTempSchema = oracleTempSchema,
                                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                                     outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                     outcomeTable = outcomeTable,
                                                     exposureDatabaseSchema = exposureDatabaseSchema,
                                                     exposureTable = exposureTable,
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
                        maxCores = maxCores)
    }

    if (runSelfControlledCaseSeries) {
        ParallelLogger::logInfo("Running SelfControlledCaseSeries")
        runSelfControlledCaseSeries(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                    oracleTempSchema = oracleTempSchema,
                                    outcomeDatabaseSchema = outcomeDatabaseSchema,
                                    outcomeTable = outcomeTable,
                                    outputFolder = outputFolder,
                                    cdmVersion = cdmVersion,
                                    maxCores = maxCores)
    }

    if (runSelfControlledCohort) {
        ParallelLogger::logInfo("Running SelfControlledCohort")
        runSelfControlledCohort(connectionDetails = connectionDetails,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                oracleTempSchema = oracleTempSchema,
                                outcomeDatabaseSchema = outcomeDatabaseSchema,
                                outcomeTable = outcomeTable,
                                outputFolder = outputFolder,
                                cdmVersion = cdmVersion,
                                maxCores = maxCores)
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
                       maxCores = maxCores)
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
                         maxCores = cdmVersion)
    }

    if (createCharacterization) {
        ParallelLogger::logInfo("Creating characterization")
        createGeneralCharacterization(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      oracleTempSchema = oracleTempSchema,
                                      exportFolder = file.path(outputFolder, "export"),
                                      databaseId = databaseName,
                                      cdmVersion = cdmVersion)
    }

    if (packageResults) {
        ParallelLogger::logInfo("Packaging results")
        packageResults(outputFolder = outputFolder,
                       exportFolder = file.path(outputFolder, "export"),
                       databaseName = databaseName)
    }
}
