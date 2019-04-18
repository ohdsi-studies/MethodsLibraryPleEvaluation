# Some code to check whether the true effect size of the positive controls holds for the case-control design:

allControls <- read.csv(file.path(outputFolder, "allControls.csv"))

i <- 244
nesting <- FALSE

allControls$trueOddsRatio <- NA
allControls$trueRelativeRisk <- NA


allControls[i, ]
if (!nesting) {
    caseData <- CaseControl::loadCaseData(file.path(outputFolder, "caseControl", "caseData_cd1"))
} else {
    loadedData <- ""
    allControls <- allControls[order(allControls$nestingId), ]
}

for (i in 1:nrow(allControls)) {
    print(i)
    if (allControls$targetEffectSize[i] == 1) {
        allControls$trueOddsRatio[i] <- 1.0
        allControls$trueRelativeRisk[i] <- 1.0
    } else {
        if (nesting) {
            fileName <- file.path(outputFolder, "caseControl", sprintf("caseData_cd2_n%s", allControls$nestingId[i]))
            if (loadedData != fileName) {
                caseData <- CaseControl::loadCaseData(fileName)
                loadedData <- fileName
            }
        }
        caseControlsNc <- CaseControl::selectControls(caseData = caseData,
                                                      outcomeId = allControls$oldOutcomeId[i],
                                                      firstOutcomeOnly = TRUE,
                                                      washoutPeriod = 365,
                                                      controlsPerCase = 0,
                                                      removedUnmatchedCases = FALSE)
        caseControlsPc <- CaseControl::selectControls(caseData = caseData,
                                                      outcomeId = allControls$outcomeId[i],
                                                      firstOutcomeOnly = TRUE,
                                                      washoutPeriod = 365,
                                                      controlsPerCase = 0,
                                                      removedUnmatchedCases = FALSE)

        exposureDataNc <- CaseControl::getDbExposureData(caseControlsNc, exposureIds = allControls$targetId[i], caseData = caseData)

        exposureDataPc <- CaseControl::getDbExposureData(caseControlsPc, exposureIds = allControls$targetId[i], caseData = caseData)

        ccdNc <- CaseControl::createCaseControlData(caseControlsExposure = exposureDataNc, exposureId = allControls$targetId[i], firstExposureOnly = FALSE, riskWindowStart = 0, riskWindowEnd = 0, exposureWashoutPeriod = 365)
        ccdPc <- CaseControl::createCaseControlData(caseControlsExposure = exposureDataPc, exposureId = allControls$targetId[i], firstExposureOnly = FALSE, riskWindowStart = 0, riskWindowEnd = 0, exposureWashoutPeriod = 365)

        # exposures <- caseData$exposures[caseData$exposures$exposureId == allControls$targetId[i], ]
        # exposures <- merge(exposures, caseData$nestingCohorts)
        # exposures <- ff::as.ram(exposures)
        # exposures$startDate <- exposures$startDate + 365
        # exposures <- exposures[exposures$exposureEndDate >= exposures$startDate, ]
        # exposures$exposureStartDate[exposures$exposureStartDate < exposures$startDate] <- exposures$startDate[exposures$exposureStartDate < exposures$startDate]
        # exposureDays <- as.integer(sum(exposures$exposureEndDate - exposures$exposureStartDate))
        # or <- (sum(ccdPc$exposed) / (exposureDays - sum(ccdPc$exposed))) / (sum(ccdNc$exposed) / (exposureDays - sum(ccdNc$exposed)))
        # allControls$trueOddsRatio[i] <- or
        rr <- sum(ccdPc$exposed) / sum(ccdNc$exposed)
        allControls$trueRelativeRisk[i] <- rr
    }
}


ggplot2::ggplot(allControls, ggplot2::aes(x = trueEffectSize, y = trueRelativeRisk)) + ggplot2::geom_point(alpha = 0.25)

allControls$trueEffectSize[i]
allControls$targetEffectSize[i]
allControls$trueEffectSizeFirstExposure[i]


ccd <- rbind(ccdNc[ccdNc$exposed, ], ccdPc[ccdPc$exposed, ])
ccd$isCase <- c(rep(0, sum(ccdNc$exposed)), rep(1, sum(ccdPc$exposed)))

or <- (sum(ccd$isCase & ccd$exposed) / sum(!ccd$isCase & ccd$exposed)) / (sum(ccd$isCase & !ccd$exposed) / sum(!ccd$isCase & !ccd$exposed))
or


injectionSummary <- readRDS(file.path(outputFolder, "injectionSummary.rds"))

injectionSummary[injectionSummary$newOutcomeId == allControls$outcomeId[i], ]

exposures <- caseData$exposures[caseData$exposures$exposureId == allControls$targetId[i], ]
exposures <- merge(exposures, caseData$nestingCohorts)
exposures <- ff::as.ram(exposures)
exposures$startDate <- exposures$startDate + 365
# exposures <- exposures[exposures$exposureEndDate >= exposures$startDate & exposures$exposureStartDate < exposures$endDate, ]
exposures <- exposures[exposures$exposureStartDate >= exposures$startDate & exposures$exposureStartDate < exposures$endDate, ]
exposures$exposureStartDate[exposures$exposureStartDate < exposures$startDate] <- exposures$startDate[exposures$exposureStartDate < exposures$startDate]
nrow(exposures)

outcomes <- caseData$cases[caseData$cases$outcomeId == allControls$oldOutcomeId[i], ]
outcomes <- ff::as.ram(outcomes)
outcomes <- merge(outcomes, exposures)
exposedOutcomes <- outcomes[outcomes$indexDate >= outcomes$exposureStartDate & outcomes$indexDate <= outcomes$exposureEndDate, ]
nrow(exposedOutcomes)
names(outcomes)
