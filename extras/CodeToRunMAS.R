# Copyright 2020 Observational Health Data Sciences and Informatics
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

library(MethodsLibraryPleEvaluation)
options('fftempdir' = 'd:/fftemp')

dbms <- "pdw"
user <- NULL
pw <- NULL
server <- Sys.getenv("PDW_SERVER")
port <- Sys.getenv("PDW_PORT")
maxCores <- 2
cdmVersion <- "5"
oracleTempSchema <- NULL
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# E-mail settings -----------------------------------------------------------------------------
mailSettings <- list(from = Sys.getenv("mailAddress"),
                     to = c(Sys.getenv("mailAddress")),
                     smtp = list(host.name = "smtp.gmail.com",
                                 port = 465,
                                 user.name = Sys.getenv("mailAddress"),
                                 passwd = Sys.getenv("mailPassword"),
                                 ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE)
ParallelLogger::addDefaultEmailLogger(mailSettings = mailSettings, label = Sys.info()["nodename"])

# CCAE settings --------------------------------------------------------------------------------
cdmDatabaseSchema <- "cdm_truven_ccae_v778.dbo"
databaseName <- "CCAE"
outcomeDatabaseSchema <- "scratch.dbo"
outcomeTable <- "mschuemi_ohdsi_hois_ccae"
nestingCohortDatabaseSchema <- "scratch.dbo"
nestingCohortTable <- "mschuemi_ohdsi_nesting_ccae"
outputFolder <- "d:/Users/msuchard/MethodsLibraryPleEvaluation_ccae"
exposureDatabaseSchema <- cdmDatabaseSchema
exposureTable = "drug_era"
exportFolder <- file.path(outputFolder, "export")

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        oracleTempSchema = oracleTempSchema,
        outcomeDatabaseSchema = outcomeDatabaseSchema,
        outcomeTable = outcomeTable,
        nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
        nestingCohortTable = nestingCohortTable,
        exposureDatabaseSchema = exposureDatabaseSchema,
        exposureTable = exposureTable,
        outputFolder = outputFolder,
        databaseName = databaseName,
        maxCores = maxCores,
        cdmVersion = cdmVersion,
        createNegativeControlCohorts = FALSE,
        imputeExposureLengthForPanther = FALSE,
        synthesizePositiveControls = FALSE,
        runCohortMethod = FALSE,
        runSelfControlledCaseSeries = FALSE,
        runSelfControlledCohort = FALSE,
        runCaseControl = FALSE,
        runCaseCrossover = FALSE,
        createCharacterization = TRUE,
        packageResults = FALSE)


MethodEvaluation::launchMethodEvaluationApp(exportFolder)

metrics <- MethodEvaluation::computeOhdsiBenchmarkMetrics(exportFolder = exportFolder, calibrated = FALSE)
write.csv(metrics, file.path(outputFolder, sprintf("metrics_%s.csv", databaseName)), row.names = FALSE)
metrics <- MethodEvaluation::computeOhdsiBenchmarkMetrics(exportFolder = exportFolder, calibrated = TRUE)
write.csv(metrics, file.path(outputFolder, sprintf("metrics_calibrated_%s.csv", databaseName)), row.names = FALSE)



# Rerun case-control ---------------------------------------------------------------
filesToDelete <- list.files(file.path(outputFolder, "caseControl"), "exposureData_.*", include.dirs = TRUE, full.names = TRUE)
unlink(filesToDelete, recursive = TRUE)

filesToDelete <- list.files(file.path(outputFolder, "caseControl"), "ccd_*", include.dirs = FALSE, full.names = TRUE)
unlink(filesToDelete)

filesToDelete <- list.files(file.path(outputFolder, "caseControl"), "Analysis_*", include.dirs = TRUE, full.names = TRUE)
unlink(filesToDelete, recursive = TRUE)

unlink(file.path(outputFolder, "ccSummary.rds"))


execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        oracleTempSchema = oracleTempSchema,
        outcomeDatabaseSchema = outcomeDatabaseSchema,
        outcomeTable = outcomeTable,
        nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
        nestingCohortTable = nestingCohortTable,
        exposureDatabaseSchema = exposureDatabaseSchema,
        exposureTable = exposureTable,
        outputFolder = outputFolder,
        databaseName = databaseName,
        maxCores = maxCores,
        cdmVersion = cdmVersion,
        createNegativeControlCohorts = FALSE,
        imputeExposureLengthForPanther = FALSE,
        synthesizePositiveControls = FALSE,
        runCohortMethod = FALSE,
        runSelfControlledCaseSeries = FALSE,
        runSelfControlledCohort = FALSE,
        runCaseControl = FALSE,
        runCaseCrossover = FALSE,
        packageResults = TRUE)
