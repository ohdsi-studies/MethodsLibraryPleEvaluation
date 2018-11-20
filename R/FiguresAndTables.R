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
    x <- cor(t(d), use = "na.or.complete")
    x[is.nan(x)] <- 0
    # tidyr::gather(x)

    xm <- reshape2::melt(x)
    library(ggplot2)
    ggplot2::ggplot(data = xm, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
        ggplot2::geom_tile() +
        # ggplot2::scale_fill_gradientn(colours = c(rgb(0.8, 0, 0), rgb(1,1,1), rgb(0, 0, 0.8)), values = c(-1, 0, 1)) +
        ggplot2::scale_fill_gradient2() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                       axis.title = ggplot2::element_blank(),
                       plot.background = ggplot2::element_blank())
    ggplot2::ggsave(filename = file.path(outputFolder, "correlation.png"), width = 8, height = 5, dpi = 300)
}
