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
options('fftempdir' = 'r:/fftemp')

dbms <- "pdw"
user <- NULL
pw <- NULL
server <- Sys.getenv("PDW_SERVER")
port <- Sys.getenv("PDW_PORT")
maxCores <- 32
cdmVersion <- "5"
oracleTempSchema <- NULL

# CCAE settings --------------------------------------------------------------------------------
cdmDatabaseSchema <- "cdm_truven_ccae_v778.dbo"
databaseName <- "CCAE"
outcomeDatabaseSchema <- "scratch.dbo"
outcomeTable <- "mschuemi_ohdsi_hois_ccae"
nestingCohortDatabaseSchema <- "scratch.dbo"
nestingCohortTable <- "mschuemi_ohdsi_nesting_ccae"
outputFolder <- "r:/MethodsLibraryPleEvaluation_ccae"


# Optum Panther settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_optum_panther_v776.dbo"
databaseName <- "Panther"
outcomeDatabaseSchema <- "scratch.dbo"
outcomeTable <- "mschuemi_ohdsi_hois_panther"
nestingCohortDatabaseSchema <- "scratch.dbo"
nestingCohortTable <- "mschuemi_ohdsi_nesting_panther"
outputFolder <- "r:/MethodsLibraryPleEvaluation_panther"


connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

execute <- function(connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    oracleTempSchema = oracleTempSchema,
                    outcomeDatabaseSchema = outcomeDatabaseSchema,
                    outcomeTable = outcomeTable,
                    nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
                    nestingCohortTable = nestingCohortTable,
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

exportFolder <- file.path(outputFolder, "export")
MethodEvaluation::launchMethodEvaluationApp(exportFolder)


