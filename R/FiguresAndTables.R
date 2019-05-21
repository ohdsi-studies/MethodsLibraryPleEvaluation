# @file FiguresAndTables.R
#
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
createFiguresAndTables <- function(exportFolder) {
    files <- list.files(exportFolder, "estimates.*csv", full.names = TRUE)
    estimates <- lapply(files, read.csv)
    estimates <- do.call("rbind", estimates)
    estimates <- estimates[estimates$mdrrTarget <= 1.25, ]
    estimates <- estimates[!is.na(estimates$outcomeId), ]
    estimates <- estimates[estimates$outcomeId < 10000 | estimates$outcomeId > 11000, ]
    estimates$to <- paste(estimates$targetId, estimates$outcomeId)
    estimates$analysis <- paste(as.character(estimates$method), estimates$analysisId)
    d <- estimates[, c("analysis", "to", "logRr")]
    d <- tidyr::spread(d, to, logRr)
    # d <- estimates[, c("analysis", "to", "calLogRr")]
    # d <- tidyr::spread(d, to, calLogRr)
    row.names(d) <- d$analysis
    d$analysis <- NULL
    corrMat <- cor(t(d), use = "na.or.complete")
    corrMat[is.nan(corrMat)] <- 0

    # corrMatMelt <- reshape2::melt(corrMat) # A lot easier with reshape2. Here's using tidyr:
    corrMatMelt <- tidyr::gather(dplyr::mutate(as.data.frame(corrMat),
                                               Var1 = factor(row.names(corrMat),
                                                             levels = row.names(corrMat))),
                                 key = Var2,
                                 value = value,
                                 -Var1,
                                 na.rm = TRUE,
                                 factor_key = TRUE)


    labels <- unique(estimates[, c("analysis", "method", "analysisId")])
    files <- list.files(exportFolder, "analysisRef.*csv", full.names = TRUE)
    analysisRef <- lapply(files, read.csv)
    analysisRef <- do.call("rbind", analysisRef)
    labels <- merge(labels, analysisRef[, c( "method", "analysisId", "description")])
    labels$methodOrder[labels$method == "CohortMethod"] <- 1
    labels$methodOrder[labels$method == "SelfControlledCohort"] <- 2
    labels$methodOrder[labels$method == "CaseControl"] <- 3
    labels$methodOrder[labels$method == "CaseCrossover"] <- 4
    labels$methodOrder[labels$method == "SelfControlledCaseSeries"] <- 5

    labels$shortForm[labels$method == "CohortMethod"] <- "CM"
    labels$shortForm[labels$method == "SelfControlledCohort"] <- "SCC"
    labels$shortForm[labels$method == "CaseControl"] <- "CC"
    labels$shortForm[labels$method == "CaseCrossover"] <- "CCR"
    labels$shortForm[labels$method == "SelfControlledCaseSeries"] <- "SCCS"
    labels$shortForm <- paste(labels$shortForm, labels$analysisId, sep = "-")
    labels$longForm <- sprintf("%s (%s)", labels$description, labels$shortForm)
    labels$displayOrder <- order(labels$methodOrder, labels$analysisId)
    labels <- labels[order(labels$methodOrder, labels$analysisId), ]
    corrMatMelt <- merge(corrMatMelt, data.frame(Var1 = labels$analysis,
                                                 longForm = labels$longForm))
    corrMatMelt <- merge(corrMatMelt, data.frame(Var2 = labels$analysis,
                                                 shortForm = labels$shortForm))
    corrMatMelt$longForm <- factor(corrMatMelt$longForm, levels = rev(labels$longForm))
    corrMatMelt$shortForm <- factor(corrMatMelt$shortForm, levels = labels$shortForm)
    plot <- ggplot2::ggplot(data = corrMatMelt, ggplot2::aes(x = shortForm, y = longForm, fill = value)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2() +
        ggplot2::labs(fill = "Pearson\ncorrelation") +
        ggplot2::scale_x_discrete(position = "top") +
        ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0),
                       axis.title = ggplot2::element_blank(),
                       plot.background = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       legend.text = ggplot2::element_text())
    ggplot2::ggsave(filename = file.path(outputFolder, "correlation.png"), plot = plot, width = 8, height = 5, dpi = 400)
}

#' @export
showExampleControls <- function(outputFolder, exportFolder) {
    files <- list.files(exportFolder, "estimates.*csv", full.names = TRUE)
    estimates <- lapply(files, read.csv)
    estimates <- do.call("rbind", estimates)
    # Pick one control: diclofenac - celecoxib - ingrown nail
    estimates <- estimates[estimates$targetId == 1124300 &
                               estimates$comparatorId == 1118084 &
                               estimates$oldOutcomeId == 139099, ]


    files <- list.files(exportFolder, "analysisRef.*csv", full.names = TRUE)
    analysisRef <- lapply(files, read.csv)
    analysisRef <- do.call("rbind", analysisRef)
    estimates <- merge(estimates, analysisRef[, c( "method", "analysisId", "description")])

    d <- estimates[estimates$targetEffectSize == 1, ]

    d <- rbind(data.frame(method = d$method,
                          analysisId = d$analysisId,
                          description = d$description,
                          rr = exp(d$logRr),
                          ci95Lb = d$ci95Lb,
                          ci95Ub = d$ci95Ub,
                          type = "Uncalibrated"),
               data.frame(method = d$method,
                          analysisId = d$analysisId,
                          description = d$description,
                          rr = exp(d$calLogRr),
                          ci95Lb = d$calCi95Lb,
                          ci95Ub = d$calCi95Ub,
                          type = "Calibrated"))
    labels <- unique(d[, c("method", "analysisId" ,"description")])
    labels$methodOrder[labels$method == "CohortMethod"] <- 1
    labels$methodOrder[labels$method == "SelfControlledCohort"] <- 2
    labels$methodOrder[labels$method == "CaseControl"] <- 3
    labels$methodOrder[labels$method == "CaseCrossover"] <- 4
    labels$methodOrder[labels$method == "SelfControlledCaseSeries"] <- 5
    labels <- labels[order(labels$methodOrder, labels$analysisId), ]
    d$description <- factor(d$description, levels = rev(labels$description))

    # One plot with both calibrated and uncalibrated
    breaks <- c(0.25, 0.5, 1, 2, 3, 4)
    plot <- ggplot2::ggplot(d, ggplot2::aes(x = rr, y = description, xmin = ci95Lb, xmax = ci95Ub)) +
        ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
        ggplot2::geom_vline(xintercept = 1,size = 0.5) +
        ggplot2::geom_errorbarh(height = 0.15) +
        ggplot2::geom_point(size = 3, shape = 18) +
        ggplot2::scale_x_continuous("Effect size estimate",  trans = "log10", breaks = breaks, labels = breaks) +
        ggplot2::coord_cartesian(xlim = c(0.33, 3)) +
        ggplot2::facet_grid(~type) +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
                       legend.position = "none", panel.border = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       strip.background =  ggplot2::element_blank(),
                       plot.margin = grid::unit(c(0, 0, 0.1, 0), "lines"))
    ggplot2::ggsave(filename = file.path(outputFolder, "exampleIncCalibrated.png"), plot = plot, width = 8, height = 5, dpi = 400)


    # Plot showing only uncalibrated
    d <- d[d$type == "Uncalibrated", ]
    d$methodOrder <- labels$methodOrder[match(d$method, labels$method)]
    d <- d[order(d$methodOrder, d$analysisId), ]
    methods <- unique(d$method)
    table <- data.frame(firstCol = "Analysis choices")
    for (i in 1:length(methods)) {
        firstRow <- data.frame(firstCol = methods[i])
        table <- dplyr::bind_rows(table, firstRow)
        table <- dplyr::bind_rows(table, d[d$method ==  methods[i], ])
    }
    table$rrLabel <- paste0(formatC(table$rr, digits = 2, format = "f"),
                            " (", formatC(table$ci95Lb, digits = 2, format = "f"),
                            "-", formatC(table$ci95Ub, digits = 2, format = "f"),
                            ")")
    table$rrLabel[is.na(table$rr)] <- ""
    table$rrLabel[1] <- "Effect size (95% CI)"
    table$y <- nrow(table):1
    table$firstCol[table$firstCol == "CohortMethod"] <- "Cohort method"
    table$firstCol[table$firstCol == "SelfControlledCohort"] <- "Self-controlled cohort"
    table$firstCol[table$firstCol == "CaseControl"] <- "Case-control"
    table$firstCol[table$firstCol == "CaseCrossover"] <- "Case-crossover"
    table$firstCol[table$firstCol == "SelfControlledCaseSeries"] <- "Self-controlled case series"

    breaks <- c(0.25, 0.5, 1, 2, 3, 4)
    rightPlot <- ggplot2::ggplot(table, ggplot2::aes(x = rr, y = y, xmin = ci95Lb, xmax = ci95Ub)) +
        ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
        ggplot2::geom_vline(xintercept = 1,size = 0.5) +
        ggplot2::geom_errorbarh(height = 0.15, color = rgb(0, 0, 0.8), alpha = 0.8) +
        ggplot2::geom_point(size = 3, shape = 18, color = rgb(0, 0, 0.8), alpha = 0.8) +
        ggplot2::scale_x_continuous("Estimated effect size",  trans = "log10", breaks = breaks, labels = breaks) +
        ggplot2::coord_cartesian(xlim = c(0.33, 3)) +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
                       legend.position = "none", panel.border = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       plot.margin = grid::unit(c(0, 0, 0.1, 0), "lines"))


    left <- data.frame(y = rep(table$y, 3),
                       x = rep(c(1, 1.1, 3.2), each = nrow(table)),
                       text = c(as.character(table$firstCol), as.character(table$description), as.character(table$rrLabel)),
                       stringsAsFactors = FALSE)

    leftPlot <- ggplot2::ggplot(left, ggplot2::aes(x = x, y = y, label = text)) +
        ggplot2::geom_text(size = 4, hjust = 0, vjust = 0.5) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(table) - 0.5)) +
        ggplot2::labs(x = "", y = "") +
        ggplot2::coord_cartesian(xlim = c(1,4)) +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       legend.position = "none",
                       panel.border = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(colour = "white"),
                       axis.text.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_line(colour = "white"),
                       plot.margin = grid::unit(c(0, 0, 0.1, 0), "lines"))

    plot <- gridExtra::grid.arrange(leftPlot, rightPlot, ncol = 2, widths = c(2,1))
    ggplot2::ggsave(filename = file.path(outputFolder, "example.png"), plot = plot, width = 9, height = 7, dpi = 400)

    print("CohortMethod numbers:")
    cmFolder <- file.path(outputFolder, "cohortMethod")
    omr <- readRDS(file.path(cmFolder, "outcomeModelReference.rds"))
    omr <- omr[omr$targetId == 1124300 &
                   omr$comparatorId == 1118084 &
                   omr$outcomeId == 139099, ]
    studyPop <- readRDS(file.path(cmFolder, omr$studyPopFile[1]))
    print(CohortMethod::getAttritionTable(studyPop))
    summary <- readRDS(file.path(outputFolder, "cmSummary.rds"))
    cmData <- CohortMethod::loadCohortMethodData(file.path(cmFolder, omr$cohortMethodDataFolder[1]))
    s <- summary(cmData)
    print(paste("Number of covariates:", s$covariateCount))
    strataPop <- readRDS(file.path(cmFolder, omr$strataFile[omr$analysisId == 2]))
    CohortMethod::getAttritionTable(strataPop)
    print(paste("Outcomes in T:", sum(strataPop$outcomeCount[strataPop$treatment == 0] != 0)))
    print(paste("Outcomes in C:", sum(strataPop$outcomeCount[strataPop$treatment == 1] != 0)))

    print("CaseControl numbers:")
    ccFolder <- file.path(outputFolder, "caseControl")
    omr <- readRDS(file.path(ccFolder, "outcomeModelReference.rds"))
    omr <- omr[omr$exposureId == 1124300 &
                   omr$outcomeId == 139099, ]
    cd <- CaseControl::loadCaseData(file.path(ccFolder, omr$caseDataFolder[omr$analysisId == 4]))
    print(paste("Nesting cohort size:", nrow(cd$nestingCohorts)))
    print(paste("Cases before matching:", ffbase::sum.ff(cd$cases$outcomeId == 139099)))
    ccd <- readRDS(file.path(ccFolder, omr$caseControlDataFile[omr$analysisId == 4]))
    print(paste("Cases after matching:", sum(ccd$isCase)))
    print(paste("Controls after matching:", sum(!ccd$isCase)))
    print(paste("Exposed cases:", sum(ccd$exposed[ccd$isCase])))
    print(paste("Exposed controls:", sum(ccd$exposed[!ccd$isCase])))
}

#' @export
getControlStats <- function(outputFolder, exportFolder) {
    # CohortMethod
    estimates <- readRDS(file.path(outputFolder, "cmSummary.rds"))
    agg <- function(subset) {
        result <- data.frame(analysisId = subset$analysisId[1],
                             medianTargetSubjects = median(subset$target),
                             maxTargetSubjects = max(subset$target),
                             medianComparatorSubjects = median(subset$comparator),
                             maxComparatorSubjects = max(subset$comparator),
                             medianTargetOutcomes = median(subset$eventsTarget),
                             maxTargetOutcomes = max(subset$eventsTarget),
                             medianComparatorOutcomes = median(subset$eventsComparator),
                             maxComparatorOutcomes = max(subset$eventsComparator))

        return(result)
    }
    statsCm <- lapply(split(estimates, estimates$analysisId), agg)
    statsCm <- do.call("rbind", statsCm)
    analysisRef <- read.csv(file.path(outputFolder, "export", "analysisRef_CohortMethod.csv"))
    statsCm <- merge(analysisRef[, c("method", "analysisId", "description")], statsCm)

    # SelfControlledCohort
    estimates <- readRDS(file.path(outputFolder, "sccSummary.rds"))
    agg <- function(subset) {
        result <- data.frame(analysisId = subset$analysisId[1],
                             medianSubjects = median(subset$numPersons),
                             maxSubjects = max(subset$numPersons),
                             medianExposures = median(subset$numExposures),
                             maxExposures = max(subset$numExposures),
                             medianExposedOutcomes = median(subset$numOutcomesExposed),
                             maxExposedOutcomes = max(subset$numOutcomesExposed),
                             medianUnexposedOutcomes = median(subset$numOutcomesUnexposed),
                             maxUnexposedOutcomes = max(subset$numOutcomesUnexposed))

        return(result)
    }
    statsScc <- lapply(split(estimates, estimates$analysisId), agg)
    statsScc <- do.call("rbind", statsScc)
    analysisRef <- read.csv(file.path(outputFolder, "export", "analysisRef_SelfControlledCohort.csv"))
    statsScc <- merge(analysisRef[, c("method", "analysisId", "description")], statsScc)

    # SelfControlledCohort
    estimates <- readRDS(file.path(outputFolder, "sccSummary.rds"))
    agg <- function(subset) {
        result <- data.frame(analysisId = subset$analysisId[1],
                             medianSubjects = median(subset$numPersons),
                             maxSubjects = max(subset$numPersons),
                             medianExposures = median(subset$numExposures),
                             maxExposures = max(subset$numExposures),
                             medianExposedOutcomes = median(subset$numOutcomesExposed),
                             maxExposedOutcomes = max(subset$numOutcomesExposed),
                             medianUnexposedOutcomes = median(subset$numOutcomesUnexposed),
                             maxUnexposedOutcomes = max(subset$numOutcomesUnexposed))

        return(result)
    }
    statsScc <- lapply(split(estimates, estimates$analysisId), agg)
    statsScc <- do.call("rbind", statsScc)
    analysisRef <- read.csv(file.path(outputFolder, "export", "analysisRef_SelfControlledCohort.csv"))
    statsScc <- merge(analysisRef[, c("method", "analysisId", "description")], statsScc)

    # Case-control
    estimates <- readRDS(file.path(outputFolder, "ccSummary.rds"))
    agg <- function(subset) {
        result <- data.frame(analysisId = subset$analysisId[1],
                             medianCases = median(subset$cases),
                             maxCases = max(subset$cases),
                             medianControls = median(subset$controls),
                             maxControls = max(subset$controls),
                             medianExposedCases = median(subset$exposedCases),
                             maxExposedCases = max(subset$exposedCases),
                             medianExposedControls = median(subset$exposedControls),
                             maxExposedControls = max(subset$exposedControls))

        return(result)
    }
    statsCc <- lapply(split(estimates, estimates$analysisId), agg)
    statsCc <- do.call("rbind", statsCc)
    analysisRef <- read.csv(file.path(outputFolder, "export", "analysisRef_CaseControl.csv"))
    statsCc <- merge(analysisRef[, c("method", "analysisId", "description")], statsCc)

    # Case-crossover
    estimates <- readRDS(file.path(outputFolder, "ccrSummary.rds"))
    agg <- function(subset) {
        result <- data.frame(analysisId = subset$analysisId[1],
                             medianCases = median(subset$cases),
                             maxCases = max(subset$cases),
                             medianExposedOutcomes = median(subset$exposedCasesCaseWindow),
                             maxExposedOutcomes = max(subset$exposedCasesCaseWindow),
                             medianUnexposedOutcomes = median(subset$exposedCasesControlWindow),
                             maxUnexposedOutcomes = max(subset$exposedCasesControlWindow))

        return(result)
    }
    statsCcr <- lapply(split(estimates, estimates$analysisId), agg)
    statsCcr <- do.call("rbind", statsCcr)
    analysisRef <- read.csv(file.path(outputFolder, "export", "analysisRef_CaseCrossover.csv"))
    statsCcr <- merge(analysisRef[, c("method", "analysisId", "description")], statsCcr)

    # Self-controlled case series
    estimates <- read.csv(file.path(outputFolder, "sccsSummaryStats.csv"))
    agg <- function(subset) {
        result <- data.frame(analysisId = subset$analysisId[1],
                             medianCases = median(subset$cases),
                             maxCases = max(subset$cases),
                             medianExposedSubjects = median(subset$exposedSubjects),
                             maxExposedSubjects = max(subset$exposedSubjects),
                             medianExposedOutcomes = median(subset$exposedOutcomes),
                             maxExposedOutcomes = max(subset$exposedOutcomes))

        return(result)
    }
    statsSccs <- lapply(split(estimates, estimates$analysisId), agg)
    statsSccs <- do.call("rbind", statsSccs)
    analysisRef <- read.csv(file.path(outputFolder, "export", "analysisRef_SelfControlledCaseSeries.csv"))
    statsSccs <- merge(analysisRef[, c("method", "analysisId", "description")], statsSccs)


    # Combine into one table:
    table <- NULL
    # stats <- statsCm
    addStatsToTable <- function(table, stats) {
        header <- SqlRender::camelCaseToSnakeCase(colnames(stats))
        header <- gsub("_", " ", header)
        header <- as.data.frame(t(header), stringsAsFactors = FALSE)
        colnames(stats) <- colnames(header)
        for (i in 1:ncol(stats)) {
           stats[, i] <- as.character(stats[, i])
        }
        emptyRow <- header
        emptyRow[1, ] <- ""
        temp <- dplyr::bind_rows(header, stats, emptyRow)
        if (is.null(table)) {
            table <- temp
        } else {
            table <- dplyr::bind_rows(table, temp)
        }
        return(table)
    }
    table <- addStatsToTable(table, statsCm)
    table <- addStatsToTable(table, statsScc)
    table <- addStatsToTable(table, statsCc)
    table <- addStatsToTable(table, statsCcr)
    table <- addStatsToTable(table, statsSccs)
    table <- table[, -1]
    write.csv(table, file.path(outputFolder, "methodCounts.csv"), row.names = FALSE, na = "")
}


biasPlots <- function(outputFolder, exportFolder) {
    files <- list.files(exportFolder, "estimates.*csv", full.names = TRUE)
    estimates <- lapply(files, read.csv)
    estimates <- do.call("rbind", estimates)
    estimates <- estimates[estimates$targetEffectSize == 1, ]

    files <- list.files(exportFolder, "analysisRef.*csv", full.names = TRUE)
    analysisRef <- lapply(files, read.csv)
    analysisRef <- do.call("rbind", analysisRef)
    for (i in 1:nrow(analysisRef)) {
        fileName <- file.path(outputFolder, sprintf("Bias_%s_%s.png", analysisRef$method[i],  analysisRef$analysisId[i]))
        subset <- estimates[estimates$method == analysisRef$method[i] &
                                estimates$analysisId == analysisRef$analysisId[i], ]
        # EmpiricalCalibration::plotForest(logRr = subset$logRr,
        #                                  seLogRr = subset$seLogRr,
        #                                  names = paste(subset$targetName, subset$outcomeName, sep = " - "))
        EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = subset$logRr,
                                                    seLogRrNegatives = subset$seLogRr,
                                                    fileName = fileName)
    }
}

scatterPlots <- function(exportFolder) {
    files <- list.files(exportFolder, "estimates.*csv", full.names = TRUE)
    estimates <- lapply(files, read.csv)
    estimates <- do.call("rbind", estimates)
    # estimates <- estimates[estimates$targetEffectSize == 1, ]
    estimates <- estimates[(estimates$method == "CaseControl" & estimates$analysisId == 4) |
                               (estimates$method == "CohortMethod" & estimates$analysisId == 2), ]

    d <- estimates
    d$Group <- paste("True effect size =", d$targetEffectSize)
    d$method <- as.character(d$method)
    d$method[d$method == "CaseControl"] <- "Case-control"
    d$method[d$method == "CohortMethod"] <- "Cohort method"
    d$method <- factor(d$method, levels = c("Cohort method", "Case-control"))
    d$Significant <- d$ci95Lb > d$targetEffectSize | d$ci95Ub < d$targetEffectSize

    temp1 <- aggregate(Significant ~ Group + method, data = d, length)
    temp2 <- aggregate(Significant ~ Group + method, data = d, mean)

    temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
    temp1$Significant <- NULL

    temp2$meanLabel <- paste0(formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
                              "% of CIs includes ",
                              substr(as.character(temp2$Group), start = 20, stop = nchar(as.character(temp2$Group))))
    temp2$Significant <- NULL
    dd <- merge(temp1, temp2)
    # print(substr(as.character(dd$Group), start = 20, stop = nchar(as.character(dd$Group))))
    dd$tes <- as.numeric(substr(as.character(dd$Group), start = 20, stop = nchar(as.character(dd$Group))))
    breaks <- c(0.25, 0.5, 1, 2, 4, 6, 8, 10)


    theme <- ggplot2::element_text(colour = "#000000", size = 14)
    themeRA <- ggplot2::element_text(colour = "#000000", size = 14, hjust = 1)
    themeLA <- ggplot2::element_text(colour = "#000000", size = 14, hjust = 0)

    alpha <- 1 - min(0.95*(nrow(d)/nrow(dd)/50000)^0.1, 0.95)
    # All true effect sizes
    plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y= seLogRr), environment = environment()) +
        ggplot2::geom_vline(xintercept = log(breaks), colour = "#CCCCCC", lty = 1, size = 0.5) +
        ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.025), slope = 1/qnorm(0.025)), colour = rgb(0.8, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
        ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.975), slope = 1/qnorm(0.975)), colour = rgb(0.8, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
        ggplot2::geom_point(size = 2, color = rgb(0, 0, 0, alpha = 0.05), alpha = alpha, shape = 16) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::geom_label(x = log(0.26), y = 0.95, alpha = 1, hjust = "left", ggplot2::aes(label = nLabel), size = 5, data = dd) +
        ggplot2::geom_label(x = log(0.26), y = 0.8, alpha = 1, hjust = "left", ggplot2::aes(label = meanLabel), size = 5, data = dd) +
        ggplot2::scale_x_continuous("Estimated effect size", limits = log(c(0.25, 10)), breaks = log(breaks), labels = breaks) +
        ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
        ggplot2::facet_grid(method ~ Group) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       axis.text.y = themeRA,
                       axis.text.x = theme,
                       axis.title = theme,
                       legend.key = ggplot2::element_blank(),
                       strip.text.x = theme,
                       strip.text.y = theme,
                       strip.background = ggplot2::element_blank(),
                       legend.position = "none")
    ggplot2::ggsave(file.path(outputFolder, "exampleEffects.png"), plot, width = 12, height = 5, dpi = 400)

    d <- d[d$targetEffectSize == 1, ]
    dd <- dd[dd$tes == 1, ]
    plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y= seLogRr), environment = environment()) +
        ggplot2::geom_vline(xintercept = log(breaks), colour = "#CCCCCC", lty = 1, size = 0.5) +
        ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.025), slope = 1/qnorm(0.025)), colour = rgb(0.8, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
        ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.975), slope = 1/qnorm(0.975)), colour = rgb(0.8, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
        ggplot2::geom_point(size = 2, color = rgb(0, 0, 0, alpha = 0.05), alpha = alpha, shape = 16) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::geom_label(x = log(0.26), y = 0.95, alpha = 1, hjust = "left", ggplot2::aes(label = nLabel), size = 5, data = dd) +
        ggplot2::geom_label(x = log(0.26), y = 0.8, alpha = 1, hjust = "left", ggplot2::aes(label = meanLabel), size = 5, data = dd) +
        ggplot2::scale_x_continuous("Estimated effect size", limits = log(c(0.25, 10)), breaks = log(breaks), labels = breaks) +
        ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
        ggplot2::facet_grid(. ~ method) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       axis.text.y = themeRA,
                       axis.text.x = theme,
                       axis.title = theme,
                       legend.key = ggplot2::element_blank(),
                       strip.text.x = theme,
                       strip.text.y = theme,
                       strip.background = ggplot2::element_blank(),
                       legend.position = "none")
    ggplot2::ggsave(file.path(outputFolder, "exampleEffectsNcs.png"), plot, width = 8, height = 4, dpi = 300)
}

orderMetricsTables <- function(rootOutputFolderName = "MethodsLibraryPleEvaluation_", rootFolder = "s:") {
    outputFolders <- list.files(rootFolder, rootOutputFolderName)

    reorder <- function(metrics) {
        methods <- c("CohortMethod", "SelfControlledCohort", "CaseControl", "CaseCrossover", "SelfControlledCaseSeries")
        dummy <- metrics[1:5, ]
        dummy[, 4:ncol(dummy)] <- NA
        dummy$method <- methods
        dummy$analysisId <- 0
        metrics <- rbind(metrics, dummy)
        methodOrder <- data.frame(method = methods,
                                  methodOrder = 1:5)
        metrics <- merge(metrics, methodOrder)
        metrics <- metrics[order(metrics$methodOrder, metrics$analysisId), ]
        metrics$methodOrder <- NULL
        return(metrics)
    }
    for (outputFolder in outputFolders) {
        database <- gsub(rootOutputFolderName, "", outputFolder)
        metrics <- read.csv(file.path(rootFolder, outputFolder, sprintf("metrics_%s.csv", database)))
        metrics <- reorder(metrics)
        write.csv(metrics, file.path(rootFolder, outputFolder, sprintf("metrics_reordered__%s.csv", database)), row.names = FALSE, na = "")

        metrics <- read.csv(file.path(rootFolder, outputFolder, sprintf("metrics_calibrated_%s.csv", database)))
        metrics <- reorder(metrics)
        write.csv(metrics, file.path(rootFolder, outputFolder, sprintf("metrics_calibrated_reordered__%s.csv", database)), row.names = FALSE, na = "")
    }
}

createTableWithAllEstimates <- function(rootOutputFolderName = "MethodsLibraryPleEvaluation_", rootFolder = "s:") {
    outputFolders <- list.files(rootFolder, rootOutputFolderName)
    loadEstimates <- function(outputFolder) {
        exportFolder <- file.path(rootFolder, outputFolder, "export")
        files <- list.files(exportFolder, "estimates.*csv", full.names = TRUE)
        estimates <- lapply(files, read.csv)
        estimates <- do.call("rbind", estimates)
        return(estimates)
    }
    estimates <- lapply(outputFolders, loadEstimates)
    estimates <- do.call("rbind", estimates)
    z <- estimates$logRr/estimates$seLogRr
    estimates$p <- 2 * pmin(pnorm(z), 1 - pnorm(z))
    files <- list.files(file.path(rootFolder, outputFolders[1], "export"), "analysisRef.*csv", full.names = TRUE)
    analysisRef <- lapply(files, read.csv)
    analysisRef <- do.call("rbind", analysisRef)
    estimates <- merge(estimates, analysisRef[, c("method", "analysisId", "description")])
    estimates <- estimates[, c("database",
                               "method",
                               "description",
                               "comparative",
                               "firstExposureOnly",
                               "targetName",
                               "comparatorName",
                               "nestingName",
                               "outcomeName",
                               "targetEffectSize",
                               "trueEffectSize",
                               "trueEffectSizeFirstExposure",
                               "mdrrTarget",
                               "mdrrComparator",
                               "stratum",
                               "logRr",
                               "seLogRr",
                               "ci95Lb",
                               "ci95Ub",
                               "p",
                               "calLogRr",
                               "calSeLogRr",
                               "calCi95Lb",
                               "calCi95Ub",
                               "calP")]
    write.csv(estimates, file.path(rootFolder, outputFolders[1], "allEstimates.csv"), row.names = FALSE)
}

perStrataMetrics <- function(exportFolder) {
    # # Big table (not popular amongst co-authors:
    # library(MethodEvaluation)
    # data(ohdsiNegativeControls)
    # strata <- c(unique(ohdsiNegativeControls$outcomeName[ohdsiNegativeControls$type == "Exposure control"]),
    #             unique(ohdsiNegativeControls$targetName[ohdsiNegativeControls$type == "Outcome control"]),
    #             "All")
    # reference <- MethodEvaluation::computeOhdsiBenchmarkMetrics(exportFolder = exportFolder, calibrated = FALSE)
    # computeKeyMetrics <- function(stratum) {
    #     metrics <- MethodEvaluation::computeOhdsiBenchmarkMetrics(exportFolder = exportFolder, calibrated = FALSE, stratum = stratum)
    #     caliMetrics <- MethodEvaluation::computeOhdsiBenchmarkMetrics(exportFolder = exportFolder, calibrated = TRUE, stratum = stratum)
    #     selectMetrics <- data.frame(auc = metrics$auc,
    #                                 precision = caliMetrics$meanP)
    #     colnames(selectMetrics) <- paste(stratum, colnames(selectMetrics), sep = "_")
    #     return(selectMetrics)
    # }
    # metrics <- lapply(strata, computeKeyMetrics)
    # metrics <- do.call("cbind", metrics)
    # metrics <- cbind(reference[, 2:4], metrics)
    # write.csv(metrics, file.path(exportFolder, "keyMetricsPerStrata.csv"), row.names = FALSE)


    # Create plot using Shiny app structure:
    shinySettings <- list(exportFolder = "C:/Users/mschuemi/git/ShinyDeploy/MethodEvalViewer/data")
    source("C:/Users/mschuemi/git/ShinyDeploy/MethodEvalViewer/global.R")
    calibrated <- "Calibrated"
    metric = "Mean precision"
    # metric = "AUC"
    computeMetrics <- function(forEval, metric = "Mean precision") {
        if (metric == "AUC")
            y <- round(pROC::auc(pROC::roc(forEval$targetEffectSize > 1, forEval$logRr, algorithm = 3)), 2)
        else if (metric == "Coverage")
            y <- round(mean(forEval$ci95Lb < forEval$trueEffectSize & forEval$ci95Ub > forEval$trueEffectSize), 2)
        else if (metric == "Mean precision")
            y <- round(-1 + exp(mean(log(1 + (1/(forEval$seLogRr^2))))), 2)
        else if (metric == "Mean squared error (MSE)")
            y <- round(mean((forEval$logRr - log(forEval$trueEffectSize))^2), 2)
        else if (metric == "Type I error")
            y <- round(mean(forEval$p[forEval$targetEffectSize == 1] < 0.05), 2)
        else if (metric == "Type II error")
            y <- round(mean(forEval$p[forEval$targetEffectSize > 1] >= 0.05), 2)
        else if (metric == "Non-estimable")
            y <- round(mean(forEval$seLogRr >= 99), 2)
        return(data.frame(database = forEval$database[1],
                          method = forEval$method[1],
                          analysisId = forEval$analysisId[1],
                          stratum = forEval$stratum[1],
                          y = y))
    }
    subset <- estimates[!is.na(estimates$mdrrTarget) & estimates$mdrrTarget <= 1.25, ]
    if (calibrated == "Calibrated") {
        subset$logRr <- subset$calLogRr
        subset$seLogRr <- subset$calSeLogRr
        subset$ci95Lb <- subset$calCi95Lb
        subset$ci95Ub <- subset$calCi95Ub
        subset$p <- subset$calP
    }

    groups <- split(subset, paste(subset$method, subset$analysisId, subset$database, subset$stratum))
    metrics <- lapply(groups, computeMetrics, metric = metric)
    metrics <- do.call("rbind", metrics)
    strataSubset <- strata[strata != "All"]
    strataSubset <- data.frame(stratum = strataSubset,
                               x = 1:length(strataSubset),
                               stringsAsFactors = FALSE)
    metrics <- merge(metrics, strataSubset)
    methods <- unique(metrics$method)
    methods <- methods[order(methods)]
    n <- length(methods)
    methods <- data.frame(method = methods,
                          offsetX = ((1:n / (n + 1)) - ((n + 1) / 2) / (n + 1)))
    metrics <- merge(metrics, methods)
    metrics$x <- metrics$x + metrics$offsetX
    metrics$stratum <- as.character(metrics$stratum)
    metrics$stratum[metrics$stratum == "Inflammatory Bowel Disease"] <- "IBD"
    metrics$stratum[metrics$stratum == "Acute pancreatitis"] <- "Acute\npancreatitis"
    strataSubset$stratum <- as.character(strataSubset$stratum)
    strataSubset$stratum[strataSubset$stratum == "Inflammatory Bowel Disease"] <- "IBD"
    strataSubset$stratum[strataSubset$stratum == "Acute pancreatitis"] <- "Acute\npancreatitis"
    metrics$method <- as.character(metrics$method)
    metrics$method[metrics$method == "CaseControl"] <- "Case-control"
    metrics$method[metrics$method == "CaseCrossover"] <- "Case-crossover"
    metrics$method[metrics$method == "CohortMethod"] <- "Cohort method"
    metrics$method[metrics$method == "SelfControlledCaseSeries"] <- "Self-controlled case series (SCCS)"
    metrics$method[metrics$method == "SelfControlledCohort"] <- "Self-controlled cohort (SCC)"
    metrics <- metrics[metrics$y != 0, ]

    fiveColors <- c(
        "#781C86",
        "#83BA70",
        "#D3AE4E",
        "#547BD3",
        "#DF4327"
    )


    yLabel <- paste0(metric, if (calibrated == "Calibrated") " after empirical calibration" else "")
    point <- scales::format_format(big.mark = " ", decimal.mark = ".", scientific = FALSE)
    plot <- ggplot2::ggplot(metrics, ggplot2::aes(x = x, y = y, color = method)) +
        ggplot2::geom_vline(xintercept = 0.5 + 0:(nrow(strataSubset) - 1), linetype = "dashed") +
        ggplot2::geom_point(size = 2.5, alpha = 0.5, shape = 16) +
        ggplot2::scale_colour_manual(values = fiveColors) +
        ggplot2::scale_x_continuous("Stratum", breaks = strataSubset$x, labels = strataSubset$stratum) +
        ggplot2::facet_grid(database~., scales = "free_y") +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
              panel.background = ggplot2::element_rect(fill = "#F0F0F0F0", colour = NA),
              panel.grid.major.x = ggplot2::element_blank(),
              panel.grid.major.y = ggplot2::element_line(colour = "#CCCCCC"),
              axis.ticks = ggplot2::element_blank(),
              legend.position = "top",
              legend.title = ggplot2::element_blank())
    if (metric %in% c("Mean precision", "Mean squared error (MSE)")) {
        plot <- plot + ggplot2::scale_y_log10(yLabel, labels = point)
    } else {
        plot <- plot + ggplot2::scale_y_continuous(yLabel, labels = point)
    }
    ggplot2::ggsave(filename = file.path(exportFolder, "PrecisionPerStrata.png"), plot = plot, width = 7.5, height = 7)
}
