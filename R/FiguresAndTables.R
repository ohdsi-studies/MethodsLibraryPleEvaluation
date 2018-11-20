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
