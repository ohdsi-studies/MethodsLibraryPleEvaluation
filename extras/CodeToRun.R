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

library(MethodsLibraryPleEvaluation)
options('fftempdir' = 's:/fftemp')

dbms <- "pdw"
user <- NULL
pw <- NULL
server <- Sys.getenv("PDW_SERVER")
port <- Sys.getenv("PDW_PORT")
maxCores <- 32
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
outputFolder <- "r:/MethodsLibraryPleEvaluation_ccae"
exposureDatabaseSchema <- cdmDatabaseSchema
exposureTable = "drug_era"
exportFolder <- file.path(outputFolder, "export")


# Optum Panther settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_optum_panther_v776.dbo"
databaseName <- "PanTher"
outcomeDatabaseSchema <- "scratch.dbo"
outcomeTable <- "mschuemi_ohdsi_hois_panther"
nestingCohortDatabaseSchema <- "scratch.dbo"
nestingCohortTable <- "mschuemi_ohdsi_nesting_panther"
outputFolder <- "r:/MethodsLibraryPleEvaluation_panther"
exposureDatabaseSchema <- "scratch.dbo"
exposureTable = "mschuemi_ohdsi_exposure_panther"
exportFolder <- file.path(outputFolder, "export")


# MDCR settings --------------------------------------------------------------------------------
cdmDatabaseSchema <- "cdm_truven_mdcr_v779.dbo"
databaseName <- "MDCR"
outcomeDatabaseSchema <- "scratch.dbo"
outcomeTable <- "mschuemi_ohdsi_hois_mdcr"
nestingCohortDatabaseSchema <- "scratch.dbo"
nestingCohortTable <- "mschuemi_ohdsi_nesting_mdcr"
outputFolder <- "r:/MethodsLibraryPleEvaluation_mdcr"
exposureDatabaseSchema <- cdmDatabaseSchema
exposureTable = "drug_era"
exportFolder <- file.path(outputFolder, "export")


# JMDC settings --------------------------------------------------------------------------------
cdmDatabaseSchema <- "cdm_jmdc_v773.dbo"
databaseName <- "JMDC"
outcomeDatabaseSchema <- "scratch.dbo"
outcomeTable <- "mschuemi_ohdsi_hois_jmdc"
nestingCohortDatabaseSchema <- "scratch.dbo"
nestingCohortTable <- "mschuemi_ohdsi_nesting_jmdc"
outputFolder <- "s:/MethodsLibraryPleEvaluation_jmdc"
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
        createNegativeControlCohorts = TRUE,
        synthesizePositiveControls = TRUE,
        runCohortMethod = TRUE,
        runSelfControlledCaseSeries = TRUE,
        runSelfControlledCohort = TRUE,
        runCaseControl = TRUE,
        runCaseCrossover = TRUE,
        packageResults = TRUE)


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
        synthesizePositiveControls = FALSE,
        runCohortMethod = FALSE,
        runSelfControlledCaseSeries = FALSE,
        runSelfControlledCohort = FALSE,
        runCaseControl = TRUE,
        runCaseCrossover = FALSE,
        packageResults = TRUE)
