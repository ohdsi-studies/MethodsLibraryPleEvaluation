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
cdmDatabaseSchema <- "CDM_Truven_CCAE_V778.dbo"
databaseName <- "CCAE"
oracleTempSchema <- NULL
outcomeDatabaseSchema <- "scratch.dbo"
outcomeTable <- "mschuemi_ohdsi_hois_ccae"
nestingCohortDatabaseSchema <- "scratch.dbo"
nestingCohortTable <- "mschuemi_ohdsi_nesting_ccae"
outputFolder <- "r:/MethodsLibraryPleEvaluation_ccae"
maxCores <- 32
cdmVersion <- "5"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Initiate logger, make sure output folder exists --------------------------------------------------------

if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
}

ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))

# Create outcome and nesting cohorts for positive and negative controls ----------------------------------

MethodEvaluation::createReferenceSetCohorts(connectionDetails = connectionDetails,
                                            oracleTempSchema = oracleTempSchema,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            outcomeDatabaseSchema = outcomeDatabaseSchema,
                                            outcomeTable = outcomeTable,
                                            nestingDatabaseSchema = nestingCohortDatabaseSchema,
                                            nestingTable = nestingCohortTable,
                                            referenceSet = "ohdsiMethodsBenchmark")

MethodEvaluation::synthesizePositiveControls(connectionDetails = connectionDetails,
                                             oracleTempSchema = oracleTempSchema,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             outcomeDatabaseSchema = outcomeDatabaseSchema,
                                             outcomeTable = outcomeTable,
                                             maxCores = maxCores,
                                             workFolder = outputFolder,
                                             summaryFileName = file.path(outputFolder, "allControls.csv"),
                                             referenceSet = "ohdsiMethodsBenchmark")

# Run methods on negative and positive controls ----------------------------------------------------------


runCohortMethod(connectionDetails = connectionDetails,
                cdmDatabaseSchema = cdmDatabaseSchema,
                oracleTempSchema = oracleTempSchema,
                outcomeDatabaseSchema = outcomeDatabaseSchema,
                outcomeTable = outcomeTable,
                outputFolder = outputFolder,
                cdmVersion = cdmVersion,
                maxCores = 32)

runSelfControlledCaseSeries(connectionDetails = connectionDetails,
                            cdmDatabaseSchema = cdmDatabaseSchema,
                            oracleTempSchema = oracleTempSchema,
                            outcomeDatabaseSchema = outcomeDatabaseSchema,
                            outcomeTable = outcomeTable,
                            workFolder = workFolder,
                            cdmVersion = cdmVersion)

runSelfControlledCohort(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        oracleTempSchema = oracleTempSchema,
                        outcomeDatabaseSchema = outcomeDatabaseSchema,
                        outcomeTable = outcomeTable,
                        outputFolder = outputFolder,
                        cdmVersion = cdmVersion)

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

packageResults(connectionDetails = connectionDetails,
               cdmDatabaseSchema = cdmDatabaseSchema,
               outputFolder = outputFolder)

addCalibration(file.path(outputFolder, "export"))


# Merge results from multiple databases

folders <- c("s:/MethodsLibraryPleEvaluation", "r:/MethodsLibraryPleEvaluation_ccae")
calibrated <- data.frame()
for (folder in folders) {
  temp <- read.csv(file.path(folder, "export", "calibrated.csv"), stringsAsFactors = FALSE)
  calibrated <- rbind(calibrated, temp)
}
head(calibrated)
write.csv(calibrated, "r:/calibrated.csv", row.names = FALSE)
