omr <- readRDS(file.path(outputFolder, "selfControlledCaseSeries", "outcomeModelReference.rds"))
result <- omr[, c("exposureId", "outcomeId", "analysisId")]
pb <- txtProgressBar(style = 3)
for (i in 1:nrow(result)) {
    sccsEraDataFolder <- file.path(outputFolder, "selfControlledCaseSeries", omr$sccsEraDataFolder[i])
    sccsEraData <- SelfControlledCaseSeries::loadSccsEraData(sccsEraDataFolder)
    covariateName <- ff::as.ram(sccsEraData$covariateRef$covariateName)
    idx <- which(grepl("Exposure of interest", covariateName))
    covariateId <- sccsEraData$covariateRef$covariateId[idx]
    exposedRowIds <- sccsEraData$covariates$rowId[ff::as.ram(sccsEraData$covariates$covariateId) == covariateId]
    result$cases[i] <- ffbase::sum.ff(sccsEraData$outcomes$y)
    result$exposedOutcomes[i] <- ffbase::sum.ff(sccsEraData$outcomes$y[ffbase::`%in%`(sccsEraData$outcomes$rowId, exposedRowIds)])
    result$exposedSubjects[i] <- length(unique(sccsEraData$outcomes$stratumId[ffbase::`%in%`(sccsEraData$outcomes$rowId, exposedRowIds)]))
    setTxtProgressBar(pb, i/nrow(result))
}
close(pb)
write.csv(result, file.path(outputFolder, "sccsSummaryStats.csv"), row.names = FALSE)
