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
imputeExposureLengthForPanther <- function(connectionDetails,
                                           oracleTempSchema,
                                           cdmDatabaseSchema,
                                           exposureDatabaseSchema,
                                           exposureTable) {
    # # Only impute for exposures in gold standard:
    # ohdsiNegativeControls <- readRDS(system.file("ohdsiNegativeControls.rds", package = "MethodEvaluation"))
    # exposureIds <- unique(c(ohdsiNegativeControls$targetId, ohdsiNegativeControls$comparatorId))
    # sql <- SqlRender::loadRenderTranslateSql("ExposureLengthImputation.sql",
    #                                          "MethodsLibraryPleEvaluation",
    #                                          dbms = connectionDetails$dbms,
    #                                          oracleTempSchema = oracleTempSchema,
    #                                          cdm_database_schema = cdmDatabaseSchema,
    #                                          exposure_database_schema = exposureDatabaseSchema,
    #                                          exposure_table = exposureTable,
    #                                          exposure_ids = exposureIds)
    # connection <- DatabaseConnector::connect(connectionDetails)
    # on.exit(DatabaseConnector::disconnect(connection))
    # DatabaseConnector::executeSql(connection, sql)


    # Impute for all exposures (needed for MSCCS):
    # exposureTable <- "mschuemi_ohdsi_exposure_all_panther"
    sql <- SqlRender::loadRenderTranslateSql("ExposureLengthImputation.sql",
                                             "MethodsLibraryPleEvaluation",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             exposure_database_schema = exposureDatabaseSchema,
                                             exposure_table = exposureTable)
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
    DatabaseConnector::executeSql(connection, sql)

    ParallelLogger::logInfo("Creating indices on exposure table")
    sql <- "CREATE INDEX methodEval_temp1 ON @exposure_database_schema.@exposure_table (subject_id);
    CREATE INDEX methodEval_temp2 ON @exposure_database_schema.@exposure_table (cohort_definition_id);
    CREATE INDEX methodEval_temp3 ON @exposure_database_schema.@exposure_table (cohort_start_date);"
    sql <- SqlRender::renderSql(sql,
                              exposure_database_schema = exposureDatabaseSchema,
                              exposure_table = exposureTable)$sql
    sql <- SqlRender::translateSql(sql,
                                   targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(connection, sql)

#
#
#    sql <- "SELECT COUNT(*) zero_length_count, cohort_definition_id FROM @exposure_database_schema.@exposure_table WHERE cohort_start_date = cohort_end_date AND cohort_definition_id IN (@exposure_ids) GROUP BY cohort_definition_id;"
#     sql <- SqlRender::renderSql(sql,
#                                 exposure_database_schema = exposureDatabaseSchema,
#                                 exposure_table = exposureTable,
#                                 exposure_ids = exposureIds)$sql
#     sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
#     x <- querySql(connection, sql)
#
#     sql <- "SELECT COUNT(*) overall_count, cohort_definition_id FROM @exposure_database_schema.@exposure_table WHERE cohort_definition_id IN (@exposure_ids) GROUP BY cohort_definition_id;"
#     sql <- SqlRender::renderSql(sql,
#                                 exposure_database_schema = exposureDatabaseSchema,
#                                 exposure_table = exposureTable,
#                                 exposure_ids = exposureIds)$sql
#     sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
#     x2 <- querySql(connection, sql)
#     y <- merge(x, x2)
#     y$fractionZeroLength <- y$ZERO_LENGTH_COUNT / y$OVERALL_COUNT
}
