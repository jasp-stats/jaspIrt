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

itemResponseTheoryDichotomousBayesian <- function(jaspResults, dataset, options, ...) {
  # Preliminary work
  dataset <- .irtReadData(options)
  ready <- .irtReady(options)
  .irtIRTErrorHandling(dataset, options)

  # Create the summary table
  .irtIRTSummaryTable(dataset, options, jaspResults, ready, position = 1)

  # Create the item fit statistics table
  .irtIRTItemFitStatisticsTable(dataset, options, jaspResults, ready, position = 3)

  # Create the parameter estimates table
  .irtIRTParameterEstimatesTable(dataset, options, jaspResults, ready, position = 5)

  # Create the histogram of latent ability
  .irtIRTHistogram(dataset, options, jaspResults, ready, position = 7)

  # Create the test information function
  .irtIRTTestInfoCurve(dataset, options, jaspResults, ready, position = 9)

  # Create the item information curves
  .irtIRTItemInfoCurve(dataset, options, jaspResults, ready, position = 11)

  # Create the item information curves
  .irtIRTItemCharCurve(dataset, options, jaspResults, ready, position = 13)

  # Create the prior and posterior plot
  .irtIRTPriorPosteriorPlot(dataset, options, jaspResults, ready, position = 15)
}

.irtIRTStateBayesian <- function(dataset, options, jaspResults) {
  if (!is.null(jaspResults[["state"]])) {
    return(jaspResults[["state"]]$object)
  }
  p <- try({
    # Model fit
    modelSpecs <- .bayesianIrtModelSpecs(options, dataset)
    fit <- rstan::sampling(
      object = modelSpecs[["model"]], data = modelSpecs[["data"]], pars = modelSpecs[["pars"]],
      warmup = options[["burnin"]], iter = options[["samples"]], chains = options[["chains"]],
      seed = options[["seed"]], cores = parallel::detectCores(), refresh = 0, control = list(adapt_delta = options[["nutsAdaptDelta"]], max_treedepth = options[["nutsMaxTreedepth"]])
    )
    extraction <- rstan::extract(fit)
    thetaRange <- seq(-25, 25, by = 0.01)
    # Tables
    if (!options[["dichotomous"]]) {
      items <- dataset[, unlist(options[["items"]]), drop = FALSE]
      if (options[["model"]] %in% c("rsm", "grsm")) {
        coefficients <- data.frame(b = colMeans(extraction[["beta_rsm"]]))
        coefficients <- cbind(coefficients, as.data.frame(matrix(colMeans(extraction[["threshold"]]), nrow = 1, ncol = ncol(extraction[["threshold"]]))))
        colnames(coefficients) <- c("b", paste0("t", seq_len(ncol(extraction[["threshold"]]))))
      } else {
        if (options[["model"]] == "nominal") {
          coefficients <- as.data.frame(colMeans(extraction[["beta_cat"]]))
        } else {
          coefficients <- as.data.frame(colMeans(extraction[["beta"]]))
        }
        nCategories <- as.numeric(apply(items, 2, function(x) length(unique(x))))
        indexMaxCategory <- if (options[["model"]] == "nominal") max(nCategories) else max(nCategories) - 1
        for (i in seq_len(nrow(coefficients))) {
          if (nCategories[i] != max(nCategories)) {
            coefficients[i, (nCategories[i]):indexMaxCategory] <- NA
          }
        }
        colnames(coefficients) <- paste0("b", seq_len(indexMaxCategory))
      }
    } else {
      coefficients <- data.frame(b = colMeans(extraction[["beta"]]))
    }
    samples <- list()
    samples[["theta"]] <- as.matrix(extraction$theta)
    latentScores <- as.numeric(colMeans(extraction[["theta"]]))
    # Coefficients
    if (options[["model"]] %in% c("2PL", "3PL", "4PL", "graded", "nominal", "gpcm", "grsm")) {
      if (options[["model"]] == "nominal") {
        coefAlpha <- as.data.frame(colMeans(extraction[["alpha_cat"]]))
        nCategories <- as.numeric(apply(items, 2, function(x) length(unique(x))))
        for (i in seq_len(nrow(coefAlpha))) {
          if (nCategories[i] != max(nCategories)) {
            coefAlpha[i, (nCategories[i]):(max(nCategories))] <- NA
          }
        }
        colnames(coefAlpha) <- paste0("a", seq_len(max(nCategories)))
        coefficients <- cbind(coefAlpha, coefficients)
        samples[["alpha"]] <- extraction[["alpha_cat"]]
      } else {
        coefficients <- cbind(a = colMeans(extraction[["alpha"]]), coefficients)
        if (options[["dichotomous"]] || options[["model"]] %in% c("grsm", "gpcm", "graded")) {
          samples[["alpha"]] <- as.matrix(extraction[["alpha"]])
        } else {
          samples[["alpha"]] <- extraction[["alpha"]]
        }
      }
    } else {
      coefficients <- cbind(a = 1, coefficients)
    }
    if (options[["dichotomous"]]) {
      samples[["beta"]] <- as.matrix(extraction[["beta"]])
    } else {
      if (options[["model"]] == "nominal") {
        samples[["beta"]] <- extraction[["beta_cat"]]
      } else if (options[["model"]] %in% c("rsm", "grsm")) {
        samples[["beta"]] <- as.matrix(extraction[["beta_rsm"]])
      } else {
        samples[["beta"]] <- extraction[["beta"]]
      }
    }
    if (options[["dichotomous"]]) {
      if (options[["model"]] %in% c("3PL", "4PL")) {
        coefficients[["g"]] <- colMeans(extraction[["gamma"]])
        samples[["gamma"]] <- as.matrix(extraction[["gamma"]])
      } else {
        coefficients[["g"]] <- 0
      }
      if (options[["model"]] == "4PL") {
        coefficients[["u"]] <- colMeans(extraction[["delta"]])
        samples[["delta"]] <- as.matrix(extraction[["delta"]])
      } else {
        coefficients[["u"]] <- 1
      }
    }
    # Item information
    itemInfo <- data.frame(item = options[["items"]], ppp = colMeans(extraction[["ppp"]]))
    estimates <- rstan::summary(fit)$summary
    diagnostics <- list()
    diagnostics[["avgppp"]] <- mean(itemInfo[["ppp"]], na.rm = TRUE)
    diagnostics[["maxrhat"]] <- max(estimates[, "Rhat"], na.rm = TRUE)
    diagnostics[["ndivergent"]] <- rstan::get_num_divergent(fit)
    diagnostics[["avgbfmi"]] <- mean(rstan::get_bfmi(fit), na.rm = TRUE)
    # Parameter estimates
    estimates <- cbind.data.frame(par = rownames(estimates), estimates)
    estimates <- estimates[-which(grepl(pattern = "ppp", x = rownames(estimates))), ]
    estimates <- estimates[-which(grepl(pattern = "lp_", x = rownames(estimates))), ]
    estimates[["par"]] <- gsub(pattern = "theta_sigma", replacement = "theta_sd", x = estimates[["par"]])
    estimates[["par"]] <- gsub(pattern = "alpha_cat_sigma", replacement = "a_sd", x = estimates[["par"]])
    estimates[["par"]] <- gsub(pattern = "alpha_sigma", replacement = "a_sd", x = estimates[["par"]])
    estimates[["par"]] <- gsub(pattern = "beta_cat_sigma", replacement = "b_sd", x = estimates[["par"]])
    estimates[["par"]] <- gsub(pattern = "beta_rsm_sigma", replacement = "b_sd", x = estimates[["par"]])
    estimates[["par"]] <- gsub(pattern = "beta_sigma", replacement = "b_sd", x = estimates[["par"]])
    estimates[["par"]] <- gsub(pattern = "gamma_sigma", replacement = "c_sd", x = estimates[["par"]])
    estimates[["par"]] <- gsub(pattern = "delta_sigma", replacement = "d_sd", x = estimates[["par"]])
    if (options[["model"]] == "nominal") {
      estimates[["par"]] <- gsub(pattern = "alpha_cat", replacement = "a", x = estimates[["par"]])
      estimates[["par"]] <- gsub(pattern = "beta_cat", replacement = "b", x = estimates[["par"]])
    }
    if (options[["dichotomous"]]) {
      estimates[["par"]] <- gsub(pattern = "gamma", replacement = "c", x = estimates[["par"]])
      estimates[["par"]] <- gsub(pattern = "delta", replacement = "d", x = estimates[["par"]])
    } else if (options[["model"]] %in% c("rsm", "grsm")) {
      estimates[["par"]] <- gsub(pattern = "beta_rsm", replacement = "b", x = estimates[["par"]])
      estimates[["par"]] <- gsub(pattern = "threshold", replacement = "t", x = estimates[["par"]])
    }
    estimates[["par"]] <- gsub(pattern = "alpha", replacement = "a", x = estimates[["par"]])
    estimates[["par"]] <- gsub(pattern = "beta", replacement = "b", x = estimates[["par"]])
    estimates[["par"]] <- gsub(pattern = "zeta", replacement = "\u03B2", x = estimates[["par"]])
    colnames(estimates) <- c("par", "mean", "se", "sd", "q2.5", "q25", "q50", "q75", "q97.5", "neff", "rhat")
    # Plots
    itemInformation <- data.frame(x = thetaRange)
    if (options[["dichotomous"]]) {
      prob <- .irtCalculateProbabilityDichotomous(options, thetaRange, coefficients[["a"]], coefficients[["b"]], coefficients[["g"]], coefficients[["u"]], options[["priorScaling"]])
      for (i in seq_along(options[["items"]])) {
        itemInformation[[options[["items"]][i]]] <- .irtCalculateItemInformationDichotomous(options, prob[, i], coefficients[["a"]][i], coefficients[["g"]][i], coefficients[["u"]][i], options[["priorScaling"]])
      }
    } else {
      prob <- list()
      for (i in seq_along(options[["items"]])) {
        prob[[options[["items"]][i]]] <- .irtCalculateProbabilityPolytomous(options, i, thetaRange, coefficients[which(grepl("a", colnames(coefficients)))], coefficients[which(grepl("b", colnames(coefficients)))], coefficients[which(grepl("t", colnames(coefficients)))], options[["priorScaling"]])
        itemInformation[[options[["items"]][i]]] <- .irtCalculateItemInformationPolytomous(options, i, thetaRange, prob[[i]], coefficients[which(grepl("a", colnames(coefficients)))], coefficients[which(grepl("b", colnames(coefficients)))], options[["priorScaling"]])
      }
    }
    testInformation <- apply(X = itemInformation[, -1], MARGIN = 1, FUN = sum)
    testSE <- 1 / sqrt(testInformation)
    # Item information plot
    plotDataTestInformation <- data.frame(x = rep(thetaRange, 2), y = c(testInformation, testSE), type = c(rep(gettext("Information"), length(thetaRange)), rep(gettext("Std. Error"), length(thetaRange))))
    plotDataItemInformation <- data.frame(x = thetaRange, y = itemInformation[, 2], item = 1)
    for (i in 2:length(options[["items"]])) {
      plotDataItemInformation <- rbind(plotDataItemInformation, data.frame(x = thetaRange, y = itemInformation[, i + 1], item = i))
    }
    # Item characteristic plots
    if (options[["dichotomous"]]) {
      plotDataItemCharacteristic <- data.frame(x = thetaRange, y = prob[, 1], item = 1)
      for (i in 2:length(options[["items"]])) {
        plotDataItemCharacteristic <- rbind(plotDataItemCharacteristic, data.frame(x = thetaRange, y = prob[, i], item = i))
      }
    } else {
      plotDataItemCharacteristic <- data.frame(x = rep(thetaRange, ncol(prob[[1]])), y = unlist(c(prob[[1]])), item = 1)
      for (i in 2:length(options[["items"]])) {
        plotDataItemCharacteristic <- rbind(plotDataItemCharacteristic, data.frame(x = rep(thetaRange, ncol(prob[[i]])), item = i, y = unlist(c(prob[[i]]))))
      }
    }
    # Prior and posterior plots
    ## Per person
    plotDataPriorPosterior <- list()
    plotDataPriorPosterior[["persons"]] <- data.frame(x = numeric(), person = character())
    for (i in seq_len(nrow(dataset))) {
      plotDataPriorPosterior[["persons"]] <- rbind(plotDataPriorPosterior[["persons"]], data.frame(x = samples[["theta"]][, i], person = i))
    }
    ## Per item
    plotDataPriorPosterior[["items"]] <- data.frame(x = numeric(), item = character(), param = character(), category = numeric())
    for (i in seq_along(options[["items"]])) {
      for (j in 2:length(samples)) {
        if (options[["dichotomous"]]) {
          plotDataPriorPosterior[["items"]] <- rbind(plotDataPriorPosterior[["items"]], data.frame(x = samples[[j]][, i], item = options[["items"]][i], param = names(samples)[j], category = 1))
        } else {
          if (names(samples)[j] == "beta" && !(options[["model"]] %in% c("rsm", "grsm"))) {
            plotDataPriorPosterior[["items"]] <- rbind(plotDataPriorPosterior[["items"]], data.frame(x = c(samples[[j]][, i, ]), item = options[["items"]][i], param = names(samples)[j], category = rep(1:ncol(samples[[j]][, i, ]), each = nrow(samples[[j]][, i, ]))))
          } else {
            if (options[["model"]] == "nominal") {
              plotDataPriorPosterior[["items"]] <- rbind(plotDataPriorPosterior[["items"]], data.frame(x = c(samples[[j]][, i, ]), item = options[["items"]][i], param = names(samples)[j], category = rep(1:ncol(samples[[j]][, i, ]), each = nrow(samples[[j]][, i, ]))))
            } else {
              plotDataPriorPosterior[["items"]] <- rbind(plotDataPriorPosterior[["items"]], data.frame(x = samples[[j]][, i], item = options[["items"]][i], param = names(samples)[j], category = 1))
            }
          }
        }
      }
    }
    ## Per threshold
    if (options[["model"]] %in% c("rsm", "grsm")) {
      samples[["threshold"]] <- as.matrix(extraction[["threshold"]])
      plotDataPriorPosterior[["threshold"]] <- data.frame(x = numeric(), param = character(), category = numeric())
      for (i in seq_len(ncol(samples[["threshold"]]))) {
        plotDataPriorPosterior[["threshold"]] <- rbind(plotDataPriorPosterior[["threshold"]], data.frame(x = samples[["threshold"]][, i], param = "threshold", category = i))
      }
    }
    ## Per covariate
    if (length(options[["covariates"]]) > 0) {
      samples[["zeta"]] <- as.matrix(extraction[["zeta"]])
      plotDataPriorPosterior[["coef"]] <- data.frame(x = numeric(), param = character())
      for (i in seq_along(options[["covariates"]])) {
        plotDataPriorPosterior[["coef"]] <- rbind(plotDataPriorPosterior[["coef"]], data.frame(x = samples[["zeta"]][, i], param = options[["covariates"]][i]))
      }
    }
    ## Per hierarchical prior
    plotDataPriorPosterior[["heterogeneity"]] <- data.frame(x = numeric(), param = character())
    if (options[["priorTheta"]] == "priorThetaHierarchical") {
      samples[["theta_sigma"]] <- as.matrix(extraction[["theta_sigma"]])
      plotDataPriorPosterior[["heterogeneity"]] <- rbind(plotDataPriorPosterior[["heterogeneity"]], data.frame(x = samples[["theta_sigma"]], param = "theta"))
    }
    if (options[["priorAlpha"]] == "priorAlphaHierarchical" && options[["model"]] %in% c("2PL", "3PL", "4PL", "gpcm", "grsm", "graded", "nominal")) {
      if (options[["model"]] == "nominal") {
        samples[["alpha_sigma"]] <- as.matrix(extraction[["alpha_cat_sigma"]])
      } else {
        samples[["alpha_sigma"]] <- as.matrix(extraction[["alpha_sigma"]])
      }
      plotDataPriorPosterior[["heterogeneity"]] <- rbind(plotDataPriorPosterior[["heterogeneity"]], data.frame(x = samples[["alpha_sigma"]], param = "alpha"))
    }
    if (options[["priorBeta"]] == "priorBetaHierarchical") {
      if (options[["model"]] == "nominal") {
        samples[["beta_sigma"]] <- as.matrix(extraction[["beta_cat_sigma"]])
      } else if (options[["model"]] %in% c("rsm", "grsm")) {
        samples[["beta_rsm_sigma"]] <- as.matrix(extraction[["beta_rsm_sigma"]])
      } else {
        samples[["beta_sigma"]] <- as.matrix(extraction[["beta_sigma"]])
      }
      plotDataPriorPosterior[["heterogeneity"]] <- rbind(plotDataPriorPosterior[["heterogeneity"]], data.frame(x = samples[["beta_sigma"]], param = "beta"))
    }
    if (options[["priorGamma"]] == "priorGammaHierarchical" && options[["model"]] %in% c("3PL", "4PL")) {
      samples[["gamma_sigma"]] <- as.matrix(extraction[["gamma_sigma"]])
      plotDataPriorPosterior[["heterogeneity"]] <- rbind(plotDataPriorPosterior[["heterogeneity"]], data.frame(x = samples[["gamma_sigma"]], param = "gamma"))
    }
    if (options[["priorDelta"]] == "priorDeltaHierarchical" && options[["model"]] == "4PL") {
      samples[["delta_sigma"]] <- as.matrix(extraction[["delta_sigma"]])
      plotDataPriorPosterior[["heterogeneity"]] <- rbind(plotDataPriorPosterior[["heterogeneity"]], data.frame(x = samples[["delta_sigma"]], param = "delta"))
    }
    if (options[["priorDelta"]] == "priorDeltaHierarchical" && length(options[["covariates"]]) > 0) {
      samples[["zeta_sigma"]] <- as.matrix(extraction[["zeta_sigma"]])
      plotDataPriorPosterior[["heterogeneity"]] <- rbind(plotDataPriorPosterior[["heterogeneity"]], data.frame(x = samples[["zeta_sigma"]], param = "zeta"))
    }
    # Object
    result <- list()
    result[["items"]] <- dataset[, unlist(options[["items"]]), drop = FALSE]
    result[["covariates"]] <- dataset[, unlist(options[["covariates"]]), drop = FALSE]
    result[["fit"]] <- fit
    result[["thetaRange"]] <- thetaRange
    result[["diagnostics"]] <- diagnostics
    result[["itemStatistics"]] <- itemInfo
    result[["coefficients"]] <- coefficients
    result[["latentScores"]] <- latentScores
    result[["estimates"]] <- estimates
    result[["plotDataTestInformation"]] <- plotDataTestInformation
    result[["plotDataItemInformation"]] <- plotDataItemInformation
    result[["plotDataItemCharacteristic"]] <- plotDataItemCharacteristic
    result[["plotDataPriorPosterior"]] <- plotDataPriorPosterior
  })
  if (jaspBase:::isTryError(p)) {
    jaspBase:::.quitAnalysis(gettextf("An error occurred in the analysis: %1$s", jaspBase:::.extractErrorMessage(p)))
  }
  jaspResults[["state"]] <- createJaspState(result)
  jaspResults[["state"]]$dependOn(options = .irtCommonDeps(type = "irt"))
  return(jaspResults[["state"]]$object)
}

# Calculate probability p(Response = 1 | theta, a, b, c, d) with scaling constant D
.irtCalculateProbabilityDichotomous <- function(options, theta, a, b, c, d, D = 1.702) {
  p <- matrix(nrow = length(theta), ncol = length(b))
  for (i in 1:length(b)) {
    if (options[["model"]] == "Rasch") {
      logitP <- (theta - b[i])
      p[, i] <- exp(logitP) / (1 + exp(logitP))
    } else if (options[["model"]] == "2PL") {
      logitP <- (D * a[i]) * (theta - b[i])
      p[, i] <- exp(logitP) / (1 + exp(logitP))
    } else if (options[["model"]] == "3PL") {
      logitP <- (D * a[i]) * (theta - b[i])
      p[, i] <- c[i] + (1 - c[i]) * (exp(logitP) / (1 + exp(logitP)))
    } else if (options[["model"]] == "4PL") {
      logitP <- (D * a[i]) * (theta - b[i])
      p[, i] <- c[i] + (d[i] - c[i]) * (exp(logitP) / (1 + exp(logitP)))
    }
  }
  return(p)
}

.irtCalculateItemInformationDichotomous <- function(options, p, a, c, d, D = 1.702) {
  info <- switch(options[["model"]],
    "Rasch" = p * (1 - p),
    "2PL" = D^2 * a^2 * (p * (1 - p)),
    "3PL" = D^2 * a^2 * ((p - c)^2 / (1 - c)^2) * ((1 - p) / p),
    "4PL" = (D^2 * a^2 * (p - c)^2 * (d - p)^2) / ((d - c)^2 * p * (1 - p))
  )
  return(info)
}
