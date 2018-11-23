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
showExampleControls <- function(exportFolder) {
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

}
