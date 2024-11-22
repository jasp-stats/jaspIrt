#
# Copyright (C) 2013-2023 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

itemResponseTheoryDichotomous <- function(jaspResults, dataset, options, ...) {
  # Preliminary work
  dataset <- .irtReadData(dataset, options)
  ready <- .irtReady(options)
  .irtIRTErrorHandling(dataset, options)

  # Create the summary table
  .irtIRTSummaryTable(dataset, options, jaspResults, ready, position = 1)

  # Create the test fit statistics table
  .irtIRTFitStatisticsTable(dataset, options, jaspResults, ready, position = 3)

  # Create the item fit statistics table
  .irtIRTItemFitStatisticsTable(dataset, options, jaspResults, ready, position = 5)

  # Create the DIF-analysis table
  .irtDifAnalysisTable(dataset, options, jaspResults, ready, position = 7)

  # Create the histogram of latent ability
  .irtIRTHistogram(dataset, options, jaspResults, ready, position = 9)

  # Create the test information function
  .irtIRTTestInfoCurve(dataset, options, jaspResults, ready, position = 11)

  # Create the item information curves
  .irtIRTItemInfoCurve(dataset, options, jaspResults, ready, position = 13)

  # Create the item information curves
  .irtIRTItemCharCurve(dataset, options, jaspResults, ready, position = 15)
}

.irtIRTState <- function(dataset, options, jaspResults) {
  if (!is.null(jaspResults[["state"]])) {
    return(jaspResults[["state"]]$object)
  }
  p <- try({
    # Data
    items <- dataset[, unlist(options[["items"]]), drop = FALSE]
    if (length(options[["covariates"]]) > 0) {
      covariates <- dataset[, unlist(options[["covariates"]]), drop = FALSE]
    } else {
      covariates <- NULL
    }
    fit <- mirt::mirt(data = items, model = 1, itemtype = options[["model"]], covdata = covariates, formula = ~., SE = FALSE, verbose = FALSE, TOL = options[["emTolerance"]], technical = list(NCYCLES = options[["emIterations"]], set.seed = options[["seed"]]))
    if (options[["model"]] == "grsm") {
      thetaRange <- seq(-10, 10, by = 0.1)
    } else {
      thetaRange <- seq(-25, 25, by = 0.01) # Takes too long with grsm
    }
    latentScores <- as.numeric(mirt::fscores(fit))
    # Tables
    anovaTable <- mirt::anova(fit)
    if (length(options[["covariates"]]) == 0) {
      fitStatistics <- try({
        mirt::M2(fit, CI = options[["tableFitStatisticsCI"]], theta_lim = range(thetaRange))
      })
    }
    itemStatistics <- mirt::itemfit(fit)
    coefficients <- mirt::coef(fit, simplify = TRUE, IRTpars = isTRUE(options[["model"]] != "grsm"))$items
    # Plots
    testInformation <- mirt::testinfo(fit, thetaRange)
    testSE <- 1 / sqrt(testInformation)
    plotDataTestInformation <- data.frame(
      x = rep(thetaRange, 2),
      y = c(testInformation, testSE),
      type = c(rep(gettext("Information"), length(thetaRange)), rep(gettext("Std. Error"), length(thetaRange)))
    )
    plotDataItemInformation <- data.frame(x = thetaRange, y = mirt::iteminfo(x = mirt::extract.item(fit, 1), Theta = thetaRange), item = 1)
    for (i in 2:length(options[["items"]])) {
      plotDataItemInformation <- rbind(plotDataItemInformation, data.frame(x = thetaRange, y = mirt::iteminfo(x = mirt::extract.item(fit, i), Theta = thetaRange), item = i))
    }
    probs <- mirt::probtrace(x = mirt::extract.item(fit, 1), Theta = thetaRange)
    plotDataItemCharacteristic <- data.frame(x = thetaRange, y = probs[, ncol(probs)], item = 1)
    for (i in 2:length(options[["items"]])) {
      probs <- mirt::probtrace(x = mirt::extract.item(fit, i), Theta = thetaRange)
      plotDataItemCharacteristic <- rbind(plotDataItemCharacteristic, data.frame(x = thetaRange, y = probs[, ncol(probs)], item = i))
    }
    # Object
    result <- list()
    result[["items"]] <- items
    result[["fit"]] <- fit
    result[["converged"]] <- fit@OptimInfo$converged
    result[["diagnostics"]] <- anovaTable
    result[["thetaRange"]] <- thetaRange
    if (length(options[["covariates"]]) == 0) {
      result[["fitStatistics"]] <- fitStatistics
    }
    result[["itemStatistics"]] <- itemStatistics
    result[["coefficients"]] <- coefficients
    result[["latentScores"]] <- latentScores
    result[["plotDataTestInformation"]] <- plotDataTestInformation
    result[["plotDataItemInformation"]] <- plotDataItemInformation
    result[["plotDataItemCharacteristic"]] <- plotDataItemCharacteristic
  })
  if (jaspBase:::isTryError(p)) {
    jaspBase:::.quitAnalysis(gettextf("An error occurred in the analysis: %1$s", jaspBase:::.extractErrorMessage(p)))
  }
  jaspResults[["state"]] <- createJaspState(result)
  jaspResults[["state"]]$dependOn(options = .irtCommonDeps(type = "irt"))
  return(jaspResults[["state"]]$object)
}
