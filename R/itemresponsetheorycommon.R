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

.irtCommonDeps <- function(type = "none") {
  deps <- "items"
  if (type == "irt") {
    deps <- c(
      deps, "tableFitStatisticsCI", "model", "covariates",
      "chains", "adapt", "burnin", "samples", "seed", "priorScaling", "nutsMaxTreedepth", "nutsAdaptDelta", "emIterations", "emTolerance",
      "priorTheta", "priorThetaNormalMean", "priorThetaNormalSd", "priorThetaStudentDf", "priorThetaStudentLocation", "priorThetaCauchyLocation", "priorThetaCauchyScale", "priorThetaHierarchicalSd",
      "priorZeta", "priorZetaNormalMean", "priorZetaNormalSd", "priorZetaStudentDf", "priorZetaStudentLocation", "priorZetaCauchyLocation", "priorZetaCauchyScale", "priorZetaHierarchicalSd",
      "priorAlpha", "priorAlphaLogNormalMean", "priorAlphaLogNormalSd", "priorAlphaNormalMean", "priorAlphaNormalSd", "priorAlphaStudentLocation", "priorAlphaStudentDf", "priorAlphaCauchyLocation", "priorAlphaCauchyScale", "priorAlphaHierarchicalSd",
      "priorBeta", "priorBetaNormalMean", "priorBetaNormalSd", "priorBetaStudentLocation", "priorBetaStudentDf", "priorBetaCauchyLocation", "priorBetaCauchyScale", "priorBetaHierarchicalSd",
      "priorGamma", "priorGammaUniformMin", "priorGammaUniformMax", "priorGammaBetaAlpha", "priorGammaBetaBeta", "priorGammaNormalMean", "priorGammaNormalSd", "priorGammaStudentLocation", "priorGammaStudentDf", "priorGammaCauchyLocation", "priorGammaCauchyScale", "priorGammaHierarchicalSd",
      "priorDelta", "priorDeltaUniformMin", "priorDeltaUniformMax", "priorDeltaBetaAlpha", "priorDeltaBetaBeta", "priorDeltaNormalMean", "priorDeltaNormalSd", "priorDeltaStudentLocation", "priorDeltaStudentDf", "priorDeltaCauchyLocation", "priorDeltaCauchyScale", "priorDeltaHierarchicalSd",
      "priorThreshold", "priorThresholdNormalMean", "priorThresholdNormalSd", "priorThresholdStudentLocation", "priorThresholdStudentDf", "priorThresholdCauchyLocation", "priorThresholdCauchyScale", "priorThresholdUniformMin", "priorThresholdUniformMax"
    )
  } else if (type == "ctt") {
    deps <- c(deps, "customMaxScore", "tableCronbachsAlphaCI")
  }
  return(deps)
}

.irtReadData <- function(options) {
  variables <- unlist(options[["items"]])
  variables <- variables[variables != ""]
  covariates <- unlist(options[["covariates"]])
  if (length(covariates) > 0) {
    variables <- c(variables, covariates)
  }
  dataset <- .readDataSetToEnd(columns.as.numeric = variables)
  .hasErrors(dataset,
    type = c("infinity", "observations"),
    all.target = c(options[["items"]], options[["covariates"]]),
    observations.amount = paste0("< ", nrow(dataset))
  )
  return(dataset)
}

.irtReady <- function(options) {
  ready <- length(options[["items"]]) > 1 && all(unlist(options[["items"]]) != "")
  return(ready)
}

.irtIRTErrorHandling <- function(dataset, options) {
  items <- dataset[, unlist(options[["items"]]), drop = FALSE]
  model <- switch(options[["model"]],
    "Rasch" = if (options[["dichotomous"]]) gettext("Rasch (i.e., 1-parameter logistic)") else gettext("partial credit"),
    "2PL" = gettext("2-parameter logistic"),
    "3PL" = gettext("3-parameter logistic"),
    "4PL" = gettext("4-parameter logistic"),
    "gpcm" = gettext("generalized partial credit"),
    "rsm" = gettext("rating scale"),
    "grsm" = gettext("generalized rating scale"),
    "graded" = gettext("graded response"),
    "nominal" = gettext("nominal response")
  )
  itemsWithOneResponse <- apply(items, 2, function(x) length(unique(x)) == 1)
  if (any(itemsWithOneResponse)) {
    jaspBase:::.quitAnalysis(gettextf("The following items have only one response category and cannot be estimated under the %1$s model: %2$s.", model, paste0(options[["items"]][itemsWithOneResponse], collapse = ", ")))
  }
  if (options[["dichotomous"]]) {
    colsWithMoreThanTwoResponses <- apply(items, 2, function(x) length(unique(x)) > 2)
    if (any(colsWithMoreThanTwoResponses)) {
      jaspBase:::.quitAnalysis(gettextf("The following items have more than two response categories and cannot be estimated under the %1$s model: %2$s.", model, paste0(options[["items"]][colsWithMoreThanTwoResponses], collapse = ", ")))
    }
    itemsWithOtherValues <- apply(items, 2, function(x) !all(x %in% c(0, 1)))
    if (any(itemsWithOtherValues)) {
      jaspBase:::.quitAnalysis(gettextf("The following items do not have 0's and 1's and cannot be estimated under the %1$s model: %2$s.", model, paste0(options[["items"]][itemsWithOtherValues], collapse = ", ")))
    }
  } else {
    itemsWithOneOrTwoResponses <- apply(items, 2, function(x) length(unique(x)) < 3)
    if (any(itemsWithOneOrTwoResponses)) {
      jaspBase:::.quitAnalysis(gettextf("The following items have only one or two response categories and cannot be estimated under the %1$s model: %2$s.", model, paste0(options[["items"]][itemsWithOneOrTwoResponses], collapse = ", ")))
    }
    if (options[["bayesian"]] && length(options[["items"]]) > 0) {
      dataHasSameNumberOfCategories <- length(unique(apply(items, 2, function(x) length(unique(x))))) == 1
      if (!dataHasSameNumberOfCategories) {
        jaspBase:::.quitAnalysis(gettext("The responses must contain the same number of answer categories for each item."))
      }
    }
  }
}

.irtIRTSummaryTable <- function(dataset, options, jaspResults, ready, position) {
  if (options[["explanatoryText"]]) {
    if (options[["dichotomous"]]) {
      thirdPart <- gettextf("p(Response = 1)%1$s", switch(options[["model"]],
        "Rasch" = "<sub>p,i</sub> = <sup>exp(\u03B8<sub>p</sub> - b<sub>i</sub>)</sup>  &#8260;  <sub>(1 + exp(\u03B8<sub>p</sub> - b<sub>i</sub>))</sub>",
        "2PL" = paste0("<sub>p,i</sub> = <sup>exp(", options[["priorScaling"]], "a<sub>i</sub>(\u03B8<sub>p</sub> - b<sub>i</sub>))</sup>  &#8260;  <sub>(1 + exp(", options[["priorScaling"]], "a<sub>i</sub>(\u03B8<sub>p</sub> - b<sub>i</sub>)))</sub>"),
        "3PL" = paste0("<sub>p,i</sub> = c<sub>i</sub> + (1 - c<sub>i</sub>) * <sup>exp(", options[["priorScaling"]], "a<sub>i</sub>(\u03B8<sub>p</sub> - b<sub>i</sub>))</sup>  &#8260;  <sub>(1 + exp(", options[["priorScaling"]], "a<sub>i</sub>(\u03B8<sub>p</sub> - b<sub>i</sub>)))</sub>"),
        "4PL" = paste0("<sub>p,i</sub> = c<sub>i</sub> + (d<sub>i</sub> - c<sub>i</sub>) * <sup>exp(", options[["priorScaling"]], "a<sub>i</sub>(\u03B8<sub>p</sub> - b<sub>i</sub>))</sup>  &#8260;  <sub>(1 + exp(", options[["priorScaling"]], "a<sub>i</sub>(\u03B8<sub>p</sub> - b<sub>i</sub>)))</sub>")
      ))
      modelString <- switch(options[["model"]],
        "Rasch" = gettext("Rasch (i.e., 1-parameter logistic)"),
        "2PL" = gettext("2-parameter logistic"),
        "3PL" = gettext("3-parameter logistic"),
        "4PL" = gettext("4-parameter logistic")
      )
      secondPart <- gettextf("Under the currently selected %1$s model, the probability of person <i>p</i> responding correctly on item <i>i</i> is given by the item response function:\n\n%2$s", modelString, thirdPart)
    } else {
      thirdPart <- gettextf("p(Response = r)%1$s", switch(options[["model"]],
        "Rasch" = "<sub>p,i</sub> = <sup>exp(&#8721;<sup>r</sup><sub>j=0</sub> (\u03B8<sub>p</sub> - b<sub>i,r</sub>))</sup>   &#8260;  <sub>&#8721;<sup>m-1</sup><sub>l=0</sub> exp(&#8721;<sup>i</sup><sub>j=0</sub>(\u03B8<sub>p</sub> - b<sub>i,r</sub>))</sub>",
        "gpcm" = paste0("<sub>p,i</sub> = <sup>exp(&#8721;<sup>r</sup><sub>j=0</sub> (", options[["priorScaling"]], "a<sub>i</sub>\u03B8<sub>p</sub> - b<sub>i,r</sub>))</sup>   &#8260;  <sub>&#8721;<sup>m-1</sup><sub>l=0</sub> exp(&#8721;<sup>i</sup><sub>j=0</sub>(", options[["priorScaling"]], "a<sub>i</sub>\u03B8<sub>p</sub> - b<sub>i,r</sub>))</sub>"),
        "rsm" = "<sub>p,i</sub> = <sup>exp(&#8721;<sup>r</sup><sub>j=0</sub> (\u03B8<sub>p</sub> - (b<sub>i</sub> + t<sub>r</sub>)))</sup>   &#8260;  <sub>&#8721;<sup>m-1</sup><sub>l=0</sub> exp(&#8721;<sup>i</sup><sub>j=0</sub>(\u03B8<sub>p</sub> - (b<sub>i</sub> + t<sub>j</sub>)))</sub>",
        "grsm" = paste0("<sub>p,i</sub> = <sup>exp(&#8721;<sup>r</sup><sub>j=0</sub> (", options[["priorScaling"]], "a<sub>i</sub>\u03B8<sub>p</sub> - (b<sub>i</sub> + t<sub>r</sub>)))</sup>   &#8260;  <sub>&#8721;<sup>m-1</sup><sub>l=0</sub> exp(&#8721;<sup>i</sup><sub>j=0</sub>(", options[["priorScaling"]], "a<sub>i</sub>\u03B8<sub>p</sub> - (b<sub>i</sub> + t<sub>j</sub>)))</sub>"),
        "graded" = paste0("<sub>p,i</sub> = [<sup>exp(", options[["priorScaling"]], "a<sub>i</sub>(\u03B8<sub>p</sub> - b<sub>i,r</sub>))</sup>   &#8260;  <sub>(1 + exp(", options[["priorScaling"]], "a<sub>i</sub>(\u03B8<sub>p</sub> - b<sub>i,r</sub>)))</sub>] - [<sup>exp(", options[["priorScaling"]], "a<sub>i</sub>(\u03B8<sub>p</sub> - b<sub>i,r+1</sub>))</sup>   &#8260;  <sub>(1 + exp(", options[["priorScaling"]], "a<sub>i</sub>(\u03B8<sub>p</sub> - b<sub>i,r+1</sub>)))</sub>]"),
        "nominal" = paste0("<sub>p,i</sub> = <sup>exp(", options[["priorScaling"]], "a<sub>i,r</sub>\u03B8<sub>p</sub> + b<sub>i,r</sub>)</sup>  &#8260;  <sub>&#8721;<sub>r</sub> exp(", options[["priorScaling"]], "a<sub>i,r</sub>\u03B8<sub>p</sub> + b<sub>i,r</sub>) </sub>")
      ))
      modelString <- switch(options[["model"]],
        "Rasch" = gettext("partial credit"),
        "gpcm" = gettext("generalized partial credit"),
        "rsm" = gettext("rating scale"),
        "grsm" = gettext("generalized rating scale"),
        "graded" = gettext("graded response"),
        "nominal" = gettext("nominal response")
      )
      secondPart <- gettextf("Under the currently selected %1$s model, the probability of person <i>p</i> responding in category <i>r</i> on item <i>i</i> is given by the item response function:\n\n%2$s", modelString, thirdPart)
    }
    text <- createJaspHtml(gettextf("<h3>Explanatory Text: %1$s Item Response Theory</h3>Item Response Theory (IRT) is a statistical framework used in educational and psychological testing to assess the relationship between a person's ability and their performance on a set of items. It assumes that a person's performance on the test is influenced by their overall ability as well as certain characteristics of the items, such as the item's difficulty and its ability to discriminate between persons with different abilities. In IRT, the probability of a person giving a certain response is modeled as a function of these item characteristics.\n\n%2$s", if (options[["dichotomous"]]) gettext("Dichotomous") else gettext("Polytomous"), secondPart))
    text$position <- position
    text$dependOn(options = "explanatoryText")
    jaspResults[["tableSummaryText"]] <- text
  }
  if (!is.null(jaspResults[["tableSummary"]])) {
    return()
  }
  tb <- createJaspTable(title = gettext("Model Summary"))
  tb$position <- position + 1
  tb$addColumnInfo(name = "items", title = gettext("Items"), type = "integer")
  tb$addColumnInfo(name = "n", title = gettext("Respondents"), type = "integer")
  if (!options[["bayesian"]]) {
    if (length(options[["covariates"]]) == 0) {
      tb$addColumnInfo(name = "m2", title = "M\u2082", type = "number")
      tb$addColumnInfo(name = "df", title = gettext("df"), type = "integer")
      tb$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
    }
    tb$addColumnInfo(name = "aic", title = gettext("AIC"), type = "number")
    tb$addColumnInfo(name = "bic", title = gettext("BIC"), type = "number")
    tb$addFootnote(gettext("The null hypothesis implies independence between items."))
  } else {
    tb$addColumnInfo(name = "avgppp", title = gettext("Avg. PPP"), type = "number")
    tb$addColumnInfo(name = "avgbfmi", title = gettext("Avg. BFMI"), type = "number")
    tb$addColumnInfo(name = "maxrhat", title = gettext("Max. Rhat"), type = "number")
    tb$addFootnote(gettext("Posterior predictive p-value"), colName = "avgppp")
    tb$addFootnote(gettext("Bayesian fraction of missing information"), colName = "avgbfmi")
  }
  tb$dependOn(options = .irtCommonDeps(type = "irt"))
  jaspResults[["tableSummary"]] <- tb
  if (!ready) {
    return()
  }
  if (options[["bayesian"]]) {
    state <- .irtIRTStateBayesian(dataset, options, jaspResults)
  } else {
    state <- .irtIRTState(dataset, options, jaspResults)
  }
  tb[["items"]] <- length(options[["items"]])
  tb[["n"]] <- nrow(state[["items"]])
  if (!options[["bayesian"]]) {
    if (length(options[["covariates"]]) == 0) {
      if (jaspBase:::isTryError(state[["fitStatistics"]])) {
        tb[["m2"]] <- NA
        tb[["df"]] <- NA
        tb[["p"]] <- NA
        tb$addFootnote(jaspBase:::.extractErrorMessage(state[["fitStatistics"]]), colName = "m2")
      } else {
        tb[["m2"]] <- state[["fitStatistics"]][["M2"]]
        tb[["df"]] <- state[["fitStatistics"]][["df"]]
        tb[["p"]] <- state[["fitStatistics"]][["p"]]
      }
    }
    tb[["aic"]] <- state[["diagnostics"]][["AIC"]]
    tb[["bic"]] <- state[["diagnostics"]][["BIC"]]
    if (!state[["converged"]]) {
      tb$addFootnote(symbol = gettext("<i>Warning</i>"), gettextf("The model did not converge within %1$s EM cycles using a tolerance of %2$s. Adjusting the number of cycles may improve the accuracy.", options[["emIterations"]], options[["emTolerance"]]))
    }
  } else {
    tb[["avgppp"]] <- state[["diagnostics"]][["avgppp"]]
    tb[["avgbfmi"]] <- state[["diagnostics"]][["avgbfmi"]]
    tb[["maxrhat"]] <- state[["diagnostics"]][["maxrhat"]]
    if (state[["diagnostics"]][["maxrhat"]] > 1.10) {
      tb$addFootnote(gettextf("The largest Potential Scale Reduction Factor (Rhat) is %1$s, indicating that chains have not mixed. Adjusting the prior distributions or running the chains for more iterations may help.", round(state[["diagnostics"]][["maxrhat"]], 3)), colName = "maxrhat")
    } else if (state[["diagnostics"]][["ndivergent"]] > 0) {
      tb$addFootnote(gettextf("There were %1$s divergent iterations after warmup.", state[["diagnostics"]][["ndivergent"]]), colName = "maxrhat")
    }
  }
}

.irtIRTItemFitStatisticsTable <- function(dataset, options, jaspResults, ready, position) {
  if (options[["explanatoryText"]] && options[["tableItemStatistics"]]) {
    text <- createJaspHtml(gettext("<h3>Explanatory Text: Item Information</h3>Understanding the characteristics of individual items is important for evaluating their quality. The table below provides insight into the characteristics of each item in the context of your assessment. It includes a goodness-of-fit measure for each item, which serves as an indicator of how well the item aligns with the underlying model, and also presents the estimated parameters for the item characteristic function.\n\nThe estimated parameters, in the table referred to as IRT parameters, describe the key characteristics of each item. These parameters include the discrimination parameter (a), the difficulty parameter (b), the guessing parameter (c), and the slip parameter (d) where applicable."))
    text$position <- position
    text$dependOn(options = c("explanatoryText", "tableItemStatistics"))
    jaspResults[["tableItemStatisticsText"]] <- text
  }
  if (!is.null(jaspResults[["tableItemStatistics"]]) || !options[["tableItemStatistics"]]) {
    return()
  }
  tb <- createJaspTable(title = gettext("Item Information"))
  tb$position <- position + 1
  tb$addColumnInfo(name = "item", title = gettext("Item"), type = "string")
  if (!options[["bayesian"]]) {
    tb$addColumnInfo(name = "s", title = gettextf("Signed X%1$s", "\u00B2"), type = "number")
    tb$addColumnInfo(name = "df", title = gettext("df"), type = "integer")
    tb$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
    tb$addColumnInfo(name = "rmsea", title = gettext("RMSEA"), type = "number")
    tb$addFootnote(gettext("The null hypothesis specifies local independence. However, local dependence indexes are intended to be used not for hypothesis testing but for diagnostic purposes (Chen & Thissen, 1997)."), colName = "p")
  } else {
    tb$addColumnInfo(name = "ppp", title = gettext("PPP"), type = "number")
    tb$addFootnote(gettext("Posterior predictive p-value"), colName = "ppp")
  }
  tb$dependOn(options = c(.irtCommonDeps(type = "irt"), "tableItemStatistics"))
  jaspResults[["tableItemStatistics"]] <- tb
  if (!ready) {
    return()
  }
  state <- .irtIRTState(dataset, options, jaspResults)
  coef <- state[["coefficients"]]
  if (options[["dichotomous"]]) {
    coef <- switch(options[["model"]],
      "Rasch" = coef[, which(grepl("b", colnames(coef))), drop = FALSE],
      "2PL" = coef[, which(grepl("a", colnames(coef)) | grepl("b", colnames(coef))), drop = FALSE],
      "3PL" = coef[, which(grepl("a", colnames(coef)) | grepl("b", colnames(coef)) | grepl("g", colnames(coef))), drop = FALSE],
      "4PL" = coef[, which(grepl("a", colnames(coef)) | grepl("b", colnames(coef)) | grepl("g", colnames(coef)) | grepl("u", colnames(coef))), drop = FALSE]
    )
  } else {
    if (options[["model"]] %in% c("Rasch", "rsm")) {
      coef <- coef[, -which(grepl("a", colnames(coef))), drop = FALSE]
    }
  }
  if (options[["model"]] == "grsm" && !options[["bayesian"]]) {
    colnames(coef) <- gsub(pattern = "\\ba1", replacement = gettext("Discrimination a"), x = colnames(coef)) # Item discrimination
  } else {
    colnames(coef) <- gsub(pattern = "\\ba", replacement = gettext("Discrimination a"), x = colnames(coef)) # Item discrimination
  }
  if (options[["model"]] %in% c("rsm", "grsm")) {
    if (options[["bayesian"]]) {
      colnames(coef) <- gsub(pattern = "\\bt", replacement = gettext("Threshold "), x = colnames(coef)) # Item difficulty
    } else {
      colnames(coef) <- gsub(pattern = "\\bb", replacement = gettext("Threshold "), x = colnames(coef)) # Item difficulty
    }
  }
  colnames(coef) <- gsub(pattern = "\\bb", replacement = gettext("Difficulty b"), x = colnames(coef)) # Item difficulty
  colnames(coef) <- gsub(pattern = "\\bc", replacement = gettext("Difficulty b"), x = colnames(coef)) # Item difficulty
  colnames(coef) <- gsub(pattern = "\\bg\\b", replacement = gettext("Guessing c"), x = colnames(coef)) # Item guessing
  colnames(coef) <- gsub(pattern = "\\bu\\b", replacement = gettext("Slip d"), x = colnames(coef)) # Item slip
  for (i in seq_along(colnames(coef))) {
    tb$addColumnInfo(name = colnames(coef)[i], title = colnames(coef)[i], type = "number", overtitle = gettext("IRT parameters"))
  }
  rows <- data.frame(item = options[["items"]])
  if (!options[["bayesian"]]) {
    rows <- cbind(rows,
      s = state[["itemStatistics"]][["S_X2"]],
      df = state[["itemStatistics"]][["df.S_X2"]],
      p = state[["itemStatistics"]][["p.S_X2"]],
      rmsea = state[["itemStatistics"]][["RMSEA.S_X2"]]
    )
  } else {
    rows <- cbind(rows, ppp = state[["itemStatistics"]][["ppp"]])
  }
  rows <- cbind(rows, coef)
  tb$addRows(rows)
}

.irtIRTFitStatisticsTable <- function(dataset, options, jaspResults, ready, position) {
  if (options[["explanatoryText"]] && options[["tableFitStatistics"]]) {
    text <- createJaspHtml(gettext("<h3>Explanatory Text: Additional Fit Statistics</h3>In the context of Item Response Theory (IRT) models, fit statistics like RMSEA, SRMSR, TLI, and CFI are essential tools for evaluating how well a model fits the observed data.\n\n<b>1. RMSEA (Root Mean Square Error of Approximation):</b> RMSEA measures how well the model reproduces the observed data. A lower RMSEA value suggests a better fit, with values below 0.05 indicating a 'good' model fit.\n<b>2. SRMSR (Standardized Root Mean Square Residual):</b> SRMSR is another measure of the discrepancies between the model and the observed data. It is similar to RMSEA but focuses on the standardized residuals.\n<b>3. TLI (Tucker-Lewis Index):</b> TLI is a measure of how well the model fits compared to a baseline model (often a null model). TLI values range from 0 to 1, with values above 0.90 indicating a reasonable fit and values above 0.95 indicating a good fit.\n<b>4. CFI (Comparative Fit Index):</b> CFI also compares the model to a baseline model. CFI values should be close to 1.0 for a good fit, with values above 0.95 generally considered acceptable."))
    text$position <- position
    text$dependOn(options = c("explanatoryText", "tableFitStatistics"))
    jaspResults[["tableFitStatisticsText"]] <- text
  }
  if (!is.null(jaspResults[["tableFitStatistics"]]) || !options[["tableFitStatistics"]]) {
    return()
  }
  tb <- createJaspTable(title = gettext("Additional Fit Statistics"))
  tb$position <- position + 1
  overtitle <- gettextf("%1$s%% Confidence Interval", round(options[["tableFitStatisticsCI"]] * 100, 3))
  tb$addColumnInfo(name = "rmsea", title = gettext("RMSEA"), type = "number")
  tb$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  tb$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  if (options[["model"]] != "nominal") {
    tb$addColumnInfo(name = "srmsr", title = gettext("SRMSR"), type = "number")
  }
  tb$addColumnInfo(name = "tli", title = gettext("TLI"), type = "number")
  tb$addColumnInfo(name = "cfi", title = gettext("CFI"), type = "number")
  tb$dependOn(options = c(.irtCommonDeps(type = "irt"), "tableFitStatistics"))
  jaspResults[["tableFitStatistics"]] <- tb
  if (!ready) {
    return()
  }
  state <- .irtIRTState(dataset, options, jaspResults)
  if (jaspBase:::isTryError(state[["fitStatistics"]])) {
    tb$addFootnote(jaspBase:::.extractErrorMessage(state[["fitStatistics"]]))
    return()
  }
  tb[["rmsea"]] <- state[["fitStatistics"]][["RMSEA"]]
  tb[["lower"]] <- state[["fitStatistics"]][[5]]
  tb[["upper"]] <- state[["fitStatistics"]][[6]]
  if (options[["model"]] != "nominal") {
    tb[["srmsr"]] <- state[["fitStatistics"]][["SRMSR"]]
  }
  if (!is.null(state[["fitStatistics"]][["TLI"]])) {
    tb[["tli"]] <- state[["fitStatistics"]][["TLI"]]
  }
  if (!is.null(state[["fitStatistics"]][["CFI"]])) {
    tb[["cfi"]] <- state[["fitStatistics"]][["CFI"]]
  }
}

.irtIRTParameterEstimatesTable <- function(dataset, options, jaspResults, ready, position) {
  if (options[["explanatoryText"]] && options[["tableParameterEstimates"]]) {
    text <- createJaspHtml(gettext("<h3>Explanatory Text: Parameter Estimates</h3> Bla bla bla..."))
    text$position <- position
    text$dependOn(options = c("explanatoryText", "tableParameterEstimates"))
    jaspResults[["tableParameterEstimatesText"]] <- text
  }
  if (!is.null(jaspResults[["tableParameterEstimates"]]) || !options[["tableParameterEstimates"]]) {
    return()
  }
  tb <- createJaspTable(title = gettext("Parameter Estimates"))
  tb$position <- position + 1
  tb$addColumnInfo(name = "par", title = gettext("Parameter"), type = "string")
  tb$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
  tb$addColumnInfo(name = "se", title = gettext("Std. error"), type = "number")
  tb$addColumnInfo(name = "sd", title = gettext("Std. deviation"), type = "number")
  tb$addColumnInfo(name = "q2.5", title = "2.5%", type = "number")
  tb$addColumnInfo(name = "q25", title = "25%", type = "number")
  tb$addColumnInfo(name = "q50", title = "50%", type = "number")
  tb$addColumnInfo(name = "q75", title = "75%", type = "number")
  tb$addColumnInfo(name = "q97.5", title = "97.5%", type = "number")
  tb$addColumnInfo(name = "neff", title = gettext("N Eff."), type = "number")
  tb$addColumnInfo(name = "rhat", title = gettext("Rhat"), type = "number")
  tb$dependOn(options = c(.irtCommonDeps(type = "irt"), "tableParameterEstimates"))
  tb$addFootnote(gettext("Effective sample size."), colName = "neff")
  jaspResults[["tableParameterEstimates"]] <- tb
  if (!ready) {
    return()
  }
  state <- .irtIRTStateBayesian(dataset, options, jaspResults)
  tb$setData(state[["estimates"]])
}

.irtIRTHistogram <- function(dataset, options, jaspResults, ready, position) {
  if (options[["explanatoryText"]] && options[["plotHistogramAbility"]]) {
    text <- createJaspHtml(gettext("<h3>Explanatory Text: Latent Ability</h3>A histogram of latent abilities shows a visual representation of how good or skilled a group of people are at something, even if we can't directly measure their abilities. It looks like a bar chart where the height of each bar represents the number of people with a particular level of ability. The higher the bar, the more people have that level of ability. So, it gives you a sense of the distribution of skills or abilities within a group, showing where most people fall in terms of their abilities."))
    text$position <- position
    text$dependOn(options = c("explanatoryText", "plotHistogramAbility"))
    jaspResults[["plotHistogramAbilityText"]] <- text
  }
  if (!is.null(jaspResults[["plotHistogramAbility"]]) || !options[["plotHistogramAbility"]]) {
    return()
  }
  fg <- createJaspPlot(title = gettext("Histogram of Latent Ability"), height = 320, width = 480)
  fg$position <- position + 1
  fg$dependOn(options = c(.irtCommonDeps(type = "irt"), "plotHistogramAbility"))
  jaspResults[["plotHistogramAbility"]] <- fg
  if (!ready) {
    return()
  }
  state <- .irtIRTState(dataset, options, jaspResults)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(state[["latentScores"]]), min.n = 4)
  xBins <- seq(min(xBreaks), max(xBreaks), length = 20)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, hist(state[["latentScores"]], plot = FALSE, breaks = xBins)$counts), min.n = 4)
  p <- ggplot2::ggplot(data = data.frame(x = state[["latentScores"]]), mapping = ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(fill = "lightgray", col = "black", breaks = xBins) +
    ggplot2::scale_x_continuous(name = "\u03B8", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Counts"), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  fg$plotObject <- p
}

.irtIRTTestInfoCurve <- function(dataset, options, jaspResults, ready, position) {
  if (options[["explanatoryText"]] && options[["plotTestInformation"]]) {
    text <- createJaspHtml(gettext("<h3>Explanatory Text: Test Information Function</h3>The test information function shows how much information the test provides about the ability level of a respondent at different points along the ability scale. It is like a graph that tells you where the test is most accurate in measuring a person's skills or knowledge. The higher the curve, the more precise the test is in that ability range. This figure helps us understand the test's reliability and where it is most useful for making accurate assessments."))
    text$position <- position
    text$dependOn(options = c("explanatoryText", "plotTestInformation"))
    jaspResults[["plotTestInformationText"]] <- text
  }
  if (!is.null(jaspResults[["plotTestInformation"]]) || !options[["plotTestInformation"]]) {
    return()
  }
  fg <- createJaspPlot(title = gettext("Test Information Function"), height = 320, width = 480)
  fg$position <- position + 1
  fg$dependOn(options = c(.irtCommonDeps(type = "irt"), "plotTestInformation"))
  jaspResults[["plotTestInformation"]] <- fg
  if (!ready) {
    return()
  }
  if (options[["bayesian"]]) {
    state <- .irtIRTStateBayesian(dataset, options, jaspResults)
  } else {
    state <- .irtIRTState(dataset, options, jaspResults)
  }
  plotdata <- state[["plotDataTestInformation"]]
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(-6, 6), min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, subset(plotdata$y, plotdata$x >= -6 & plotdata$x <= 6)), min.n = 4)
  p <- ggplot2::ggplot(data = plotdata, mapping = ggplot2::aes(x = x, y = y, color = type)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(name = "\u03B8", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_color_manual(name = NULL, values = c("black", "firebrick")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "top")
  fg$plotObject <- p
}

.irtIRTItemInfoCurve <- function(dataset, options, jaspResults, ready, position) {
  if (options[["explanatoryText"]] && length(options[["plotItemInformationItems"]]) > 0) {
    text <- createJaspHtml(gettext("<h3>Explanatory Text: Item Information Curves</h3>An item information curve shows how well a specific item on the test is at providing information about the ability of a respondent. The curve typically looks like a bell curve. The steeper and taller the curve, the more information the item offers, making it more useful for distinguishing between people with different abilities. On the other hand, a flat or shallow curve indicates that the item does not provide much information and may not be very effective at discriminating between individuals with varying levels of ability."))
    text$position <- position
    text$dependOn(options = c("explanatoryText", "plotItemInformationItems"))
    jaspResults[["plotItemInformationText"]] <- text
  }
  if (!is.null(jaspResults[["plotItemInformation"]]) || length(options[["plotItemInformationItems"]]) == 0) {
    return()
  }
  fg <- createJaspPlot(title = gettext("Item Information Curves"), height = 320, width = 480)
  fg$position <- position + 1
  fg$dependOn(options = c(.irtCommonDeps(type = "irt"), "plotItemInformationItems", "plotItemInformationLabels"))
  jaspResults[["plotItemInformation"]] <- fg
  if (!ready) {
    return()
  }
  if (options[["bayesian"]]) {
    state <- .irtIRTStateBayesian(dataset, options, jaspResults)
  } else {
    state <- .irtIRTState(dataset, options, jaspResults)
  }
  plotdata <- state[["plotDataItemInformation"]]
  plotdata <- subset(plotdata, plotdata$item %in% match(options[["plotItemInformationItems"]], options[["items"]]))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(-6, 6), min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, subset(plotdata$y, plotdata$x >= -6 & plotdata$x <= 6)), min.n = 4)
  colors <- colorspace::qualitative_hcl(length(options[["plotItemInformationItems"]]))
  p <- ggplot2::ggplot(data = plotdata, mapping = ggplot2::aes(x = x, y = y, col = factor(item))) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(name = "\u03B8", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Information"), breaks = yBreaks, limits = c(0, max(yBreaks) * 1.1)) +
    ggplot2::scale_color_manual(values = colors) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  if (options[["plotItemInformationLabels"]]) {
    subdata <- data.frame(x = NA, y = NA, label = options[["plotItemInformationItems"]])
    for (i in seq_len(nrow(subdata))) {
      item <- match(options[["plotItemInformationItems"]][i], options[["items"]])
      subdata$x[i] <- subset(plotdata$x, plotdata$item == item)[which.max(subset(plotdata$y, plotdata$item == item))]
      subdata$y[i] <- max(subset(plotdata$y, plotdata$item == item))
    }
    p <- p + ggrepel::geom_text_repel(data = subdata, mapping = ggplot2::aes(x = x, y = y + 0.01, label = label), inherit.aes = FALSE, color = colors, direction = "both", xlim = range(xBreaks), ylim = c(0, max(yBreaks) * 1.1), seed = 1, segment.color = "black", segment.linetype = "dashed")
  }
  fg$plotObject <- p
}

.irtIRTItemCharCurve <- function(dataset, options, jaspResults, ready, position) {
  if (options[["explanatoryText"]] && options[["plotItemCharacteristic"]]) {
    text <- createJaspHtml(gettext("<h3>Explanatory Text: Item Characteristic Curves</h3>An item characteristic curve (ICC) plot shows how different test questions or items perform. It is like a graph that helps us understand how easy or hard each question is and how likely people with different abilities are to answer them correctly. In the figure, you can see a line for each question, and the shape of the line tells you if the question is easy or difficult to answer fully correctly as a function of the ability of a respondent."))
    text$position <- position
    text$dependOn(options = c("explanatoryText", "plotItemCharacteristicItems"))
    jaspResults[["plotItemCharacteristicText"]] <- text
  }
  if (!is.null(jaspResults[["plotItemCharacteristic"]]) || length(options[["plotItemCharacteristicItems"]]) == 0) {
    return()
  }
  if (options[["plotItemCharacteristicGroup"]]) {
    object <- createJaspPlot(title = gettext("Item Characteristic Curves"), height = 320, width = 480)
  } else {
    object <- createJaspContainer(title = gettext("Item Characteristic Curves"))
  }
  object$position <- position + 1
  object$dependOn(options = c(.irtCommonDeps(type = "irt"), "plotItemCharacteristicItems", "plotItemCharacteristicLabels"))
  jaspResults[["plotItemCharacteristic"]] <- object
  if (!ready) {
    return()
  }
  if (options[["bayesian"]]) {
    state <- .irtIRTStateBayesian(dataset, options, jaspResults)
  } else {
    state <- .irtIRTState(dataset, options, jaspResults)
  }
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(-6, 6), min.n = 4)
  if (options[["plotItemCharacteristicGroup"]]) {
    plotdata <- state[["plotDataItemCharacteristic"]]
    plotdata <- subset(plotdata, plotdata$item %in% match(options[["plotItemCharacteristicItems"]], options[["items"]]))
    colors <- colorspace::qualitative_hcl(length(options[["plotItemCharacteristicItems"]]))
    p <- ggplot2::ggplot(data = plotdata, mapping = ggplot2::aes(x = x, y = y, col = factor(item))) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(name = "\u03B8", breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::scale_y_continuous(name = gettext("P(Response = 1)"), breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1.1)) +
      ggplot2::scale_color_manual(values = colors) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
    if (options[["plotItemCharacteristicLabels"]]) {
      subdata <- data.frame(x = NA, y = NA, label = options[["plotItemCharacteristicItems"]])
      for (i in seq_len(nrow(subdata))) {
        item <- match(options[["plotItemCharacteristicItems"]][i], options[["items"]])
        index <- which(subset(plotdata$y, plotdata$item == item) > 0.9)[1]
        subdata$x[i] <- subset(plotdata$x, plotdata$item == item)[index]
        subdata$y[i] <- subset(plotdata$y, plotdata$item == item)[index]
      }
      p <- p + ggrepel::geom_text_repel(data = subdata, mapping = ggplot2::aes(x = x, y = y + 0.01, label = label), inherit.aes = FALSE, color = colors, direction = "both", xlim = range(xBreaks), ylim = c(1, 1.1), seed = 1, segment.color = "black", segment.linetype = "dashed")
    }
    object$plotObject <- p
  } else {
    for (i in seq_along(options[["plotItemCharacteristicItems"]])) {
      fg <- createJaspPlot(title = gettextf("Item %1$s", options[["plotItemCharacteristicItems"]][i]), height = 240, width = 360)
      object[[paste0("plot", i)]] <- fg
      if (options[["bayesian"]]) {
        probs <- subset(state[["plotDataItemCharacteristic"]]$y, state[["plotDataItemCharacteristic"]]$item == i)
        if (options[["dichotomous"]]) {
          probs <- data.frame("P.0" = 1 - probs, "P.1" = probs)
        } else {
          probs <- matrix(probs, nrow = length(state[["thetaRange"]]), ncol = length(probs) / length(state[["thetaRange"]]))
          colnames(probs) <- paste0("P.", seq_len(ncol(probs)))
        }
      } else {
        probs <- mirt::probtrace(x = mirt::extract.item(state[["fit"]], i), Theta = state[["thetaRange"]])
      }
      plotdata <- data.frame(x = rep(state[["thetaRange"]], ncol(probs)), y = as.numeric(unlist(probs)), response = rep(colnames(probs), each = length(state[["thetaRange"]])))
      plotdata[["response"]] <- gsub(pattern = "P.", replacement = "", x = plotdata[["response"]])
      colors <- colorspace::qualitative_hcl(ncol(probs))
      p <- ggplot2::ggplot(data = plotdata, mapping = ggplot2::aes(x = x, y = y, col = factor(response))) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(name = "\u03B8", breaks = xBreaks, limits = range(xBreaks)) +
        ggplot2::scale_y_continuous(name = gettext("P(Response = r)"), breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1.1)) +
        ggplot2::scale_color_manual(name = if (options[["dichotomous"]]) NULL else "r", values = colors) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw(legend.position = if (options[["plotItemCharacteristicLabels"]]) "top" else "none")
      fg$plotObject <- p
    }
  }
}

.irtIRTPriorPosteriorPlot <- function(dataset, options, jaspResults, ready, position) {
  if (options[["explanatoryText"]] && options[["plotPriorPosterior"]]) {
    text <- createJaspHtml(gettext("<h3>Explanatory Text: Prior and Posterior Distributions</h3> In these figures, the prior distribution (dotted line if present) represents the knowledge about a parameter before seeing the data, while the posterior distribution (solid lines) show the updated knowledge about a parameter after seeing new data."))
    text$position <- position
    text$dependOn(options = c("explanatoryText", "plotPriorPosterior"))
    jaspResults[["plotTestInformationText"]] <- text
  }
  if (!is.null(jaspResults[["plotPriorPosterior"]]) || !options[["plotPriorPosterior"]]) {
    return()
  }
  container <- createJaspContainer(title = gettext("Prior and Posterior Distributions"))
  container$position <- position + 1
  container$dependOn(options = c(.irtCommonDeps(type = "irt"), "plotPriorPosterior", "plotPriorPosteriorLabels"))
  jaspResults[["plotPriorPosterior"]] <- container
  if (!ready) {
    return()
  }
  state <- .irtIRTStateBayesian(dataset, options, jaspResults)
  plotdataItems <- state[["plotDataPriorPosterior"]][["items"]]
  parNames <- c("theta", if (length(options[["covariates"]]) > 0) "zeta" else NULL, unique(plotdataItems[["param"]]), if (options[["model"]] %in% c("rsm", "grsm")) "threshold" else NULL)
  for (i in seq_along(parNames)) {
    plotTitle <- switch(parNames[i],
      "theta" = "\u03B8",
      "alpha" = gettext("Discrimination a"),
      "beta" = gettext("Difficulty b"),
      "gamma" = gettext("Guessing c"),
      "delta" = gettext("Slip d"),
      "zeta" = gettextf("Latent Regression Coefficient %1$s", "\u03B2"),
      "threshold" = gettext("Threshold t")
    )
    has_hierarchical_prior <- nrow(subset(state[["plotDataPriorPosterior"]][["heterogeneity"]], state[["plotDataPriorPosterior"]][["heterogeneity"]]$param == parNames[i])) > 0
    if (!has_hierarchical_prior) {
      fg <- createJaspPlot(title = plotTitle, height = 320, width = 480)
      fg$position <- i
      jaspResults[["plotPriorPosterior"]][[parNames[i]]] <- fg
    } else {
      container <- createJaspContainer(title = plotTitle)
      container$position <- i
      jaspResults[["plotPriorPosterior"]][[parNames[i]]] <- container
      fg <- createJaspPlot(title = gettext("Parameters"), height = 320, width = 480)
      fg$position <- 1
      jaspResults[["plotPriorPosterior"]][[parNames[i]]][["plot1"]] <- fg
      fg2 <- createJaspPlot(title = gettext("Heterogeneity"), height = 320, width = 480)
      fg2$position <- 2
      jaspResults[["plotPriorPosterior"]][[parNames[i]]][["plot2"]] <- fg2
    }
    if (parNames[i] == "theta") {
      subdata <- state[["plotDataPriorPosterior"]][["persons"]]
      maxDens <- 0
      for (j in seq_len(nrow(dataset))) {
        newMaxDens <- max(density(subset(subdata$x, subdata$person == j))$y)
        if (newMaxDens > maxDens) {
          maxDens <- newMaxDens
        }
      }
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(subdata$x, min.n = 4)
      colors <- rep("lightgray", nrow(dataset))
    } else if (parNames[i] == "threshold") {
      subdata <- state[["plotDataPriorPosterior"]][["threshold"]]
      maxDens <- 0
      for (j in seq_along(unique(subdata[["category"]]))) {
        newMaxDens <- max(density(subset(subdata$x, subdata$category == j))$y)
        if (newMaxDens > maxDens) {
          maxDens <- newMaxDens
        }
      }
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(subdata$x, min.n = 4)
      colors <- rep("black", length(unique(subdata[["category"]])))
      if (options[["plotPriorPosteriorLabels"]]) {
        subsubdata <- data.frame(x = NA, y = NA, label = seq_along(unique(subdata[["category"]])))
        for (j in seq_len(nrow(subsubdata))) {
          subsubsubdata <- subset(subdata$x, subdata$category == j)
          dens <- density(subsubsubdata, from = min(xBreaks), to = max(xBreaks))
          maxDensityIndex <- which.max(dens$y)
          subsubdata$x[j] <- dens$x[maxDensityIndex]
          subsubdata$y[j] <- dens$y[maxDensityIndex]
        }
      }
    } else if (parNames[i] == "zeta") {
      subdata <- state[["plotDataPriorPosterior"]][["coef"]]
      maxDens <- 0
      for (j in seq_along(options[["covariates"]])) {
        newMaxDens <- max(density(subset(subdata$x, subdata$param == options[["covariates"]][j]))$y)
        if (newMaxDens > maxDens) {
          maxDens <- newMaxDens
        }
      }
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(subdata$x, min.n = 4)
      colors <- rep("black", length(options[["covariates"]]))
      if (options[["plotPriorPosteriorLabels"]]) {
        subsubdata <- data.frame(x = NA, y = NA, label = options[["covariates"]])
        for (j in seq_len(nrow(subsubdata))) {
          subsubsubdata <- subset(subdata$x, subdata$param == options[["covariates"]][j])
          dens <- density(subsubsubdata, from = min(xBreaks), to = max(xBreaks))
          maxDensityIndex <- which.max(dens$y)
          subsubdata$x[j] <- dens$x[maxDensityIndex]
          subsubdata$y[j] <- dens$y[maxDensityIndex]
        }
      }
    } else {
      subdata <- subset(plotdataItems, plotdataItems$param == parNames[i])
      maxDens <- 0
      for (j in seq_along(options[["items"]])) {
        for (k in seq_along(unique(subdata[["category"]]))) {
          newMaxDens <- max(density(subset(subdata$x, subdata$item == options[["items"]][j] & subdata$category == k))$y)
          if (newMaxDens > maxDens) {
            maxDens <- newMaxDens
          }
        }
      }
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(subdata$x, min.n = 4)
      colors <- colorspace::qualitative_hcl(length(options[["items"]]))
      if (options[["plotPriorPosteriorLabels"]]) {
        subsubdata <- data.frame(x = NA, y = NA, label = options[["items"]])
        for (j in seq_len(nrow(subsubdata))) {
          for (k in seq_along(unique(subdata[["category"]]))) {
            subsubsubdata <- subset(subdata$x, subdata$item == options[["items"]][j] & subdata$category == k)
            dens <- density(subsubsubdata, from = min(xBreaks), to = max(xBreaks))
            maxDensityIndex <- which.max(dens$y)
            subsubdata$x[j] <- dens$x[maxDensityIndex]
            subsubdata$y[j] <- dens$y[maxDensityIndex]
          }
        }
      }
    }
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, maxDens), min.n = 4)
    if (parNames[i] == "theta") {
      p <- ggplot2::ggplot(data = subdata, mapping = ggplot2::aes(x = x, color = factor(person)))
    } else if (parNames[i] == "zeta") {
      p <- ggplot2::ggplot(data = subdata, mapping = ggplot2::aes(x = x, color = factor(param)))
    } else if (parNames[i] == "threshold") {
      p <- ggplot2::ggplot(data = subdata, mapping = ggplot2::aes(x = x, color = factor(category)))
    } else {
      p <- ggplot2::ggplot(data = subdata, mapping = ggplot2::aes(x = x, color = factor(item), linetype = factor(category)))
    }
    p <- p + ggplot2::geom_density(alpha = if (parNames[i] == "theta") 0.25 else 1) +
      ggplot2::scale_x_continuous(name = plotTitle, breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_linetype_manual(values = rep("solid", length(unique(subdata$category)))) +
      ggplot2::guides(linetype = "none") +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
    if (parNames[i] == "theta" && length(options[["covariates"]]) == 0) {
      p <- switch(options[["priorTheta"]],
        "priorThetaNormal" = p + ggplot2::stat_function(fun = stats::dnorm, geom = "line", args = list(mean = options[["priorThetaNormalMean"]], sd = options[["priorThetaNormalSd"]]), linetype = "dashed", color = "black"),
        "priorThetaStudent" = p + ggplot2::stat_function(fun = stats::dt, geom = "line", args = list(ncp = options[["priorThetaStudentLocation"]], df = options[["priorThetaStudentDf"]]), linetype = "dashed", color = "black"),
        "priorThetaCauchy" = p + ggplot2::stat_function(fun = stats::dcauchy, geom = "line", args = list(location = options[["priorThetaCauchyLocation"]], scale = options[["priorThetaCauchyScale"]]), linetype = "dashed", color = "black"),
        "priorThetaHierarchical" = p
      )
    }
    if (parNames[i] == "zeta" && length(options[["covariates"]]) > 0) {
      p <- switch(options[["priorZeta"]],
        "priorZetaNormal" = p + ggplot2::stat_function(fun = stats::dnorm, geom = "line", args = list(mean = options[["priorZetaNormalMean"]], sd = options[["priorZetaNormalSd"]]), linetype = "dashed", color = "black"),
        "priorZetaStudent" = p + ggplot2::stat_function(fun = stats::dt, geom = "line", args = list(ncp = options[["priorZetaStudentLocation"]], df = options[["priorZetaStudentDf"]]), linetype = "dashed", color = "black"),
        "priorZetaCauchy" = p + ggplot2::stat_function(fun = stats::dcauchy, geom = "line", args = list(location = options[["priorZetaCauchyLocation"]], scale = options[["priorZetaCauchyScale"]]), linetype = "dashed", color = "black"),
        "priorZetaHierarchical" = p
      )
    }
    if (parNames[i] == "threshold" && options[["model"]] %in% c("rsm", "grsm")) {
      p <- switch(options[["priorThreshold"]],
        "priorThresholdNormal" = p + ggplot2::stat_function(fun = stats::dnorm, geom = "line", args = list(mean = options[["priorThresholdNormalMean"]], sdlog = options[["priorThresholdNormalSd"]]), linetype = "dashed", color = "black"),
        "priorThresholdStudent" = p + ggplot2::stat_function(fun = stats::dt, geom = "line", args = list(ncp = options[["priorThresholdStudentLocation"]], df = options[["priorThresholdStudentDf"]]), linetype = "dashed", color = "black"),
        "priorThresholdCauchy" = p + ggplot2::stat_function(fun = stats::dcauchy, geom = "line", args = list(location = options[["priorThresholdCauchyLocation"]], scale = options[["priorThresholdCauchyScale"]]), linetype = "dashed", color = "black"),
        "priorThresholdUniform" = p + ggplot2::stat_function(fun = stats::dunif, geom = "line", args = list(min = options[["priorThresholdUniformMin"]], max = options[["priorThresholdUniformMax"]]), linetype = "dashed", color = "black")
      )
    }
    if (parNames[i] == "alpha" && options[["model"]] != "nominal") {
      p <- switch(options[["priorAlpha"]],
        "priorAlphaHierarchical" = p,
        "priorAlphaLogNormal" = p + ggplot2::stat_function(fun = stats::dlnorm, geom = "line", args = list(meanlog = options[["priorAlphaLogNormalMean"]], sdlog = options[["priorAlphaLogNormalSd"]]), linetype = "dashed", color = "black"),
        "priorAlphaNormal" = p + ggplot2::stat_function(fun = truncdist::dtrunc, geom = "line", args = list(spec = "norm", a = 0, mean = options[["priorAlphaNormalMean"]], sd = options[["priorAlphaNormalSd"]]), linetype = "dashed", color = "black"),
        "priorAlphaStudent" = p + ggplot2::stat_function(fun = truncdist::dtrunc, geom = "line", args = list(spec = "t", a = 0, ncp = options[["priorAlphaStudentLocation"]], df = options[["priorAlphaStudentDf"]]), linetype = "dashed", color = "black"),
        "priorAlphaCauchy" = p + ggplot2::stat_function(fun = truncdist::dtrunc, geom = "line", args = list(spec = "cauchy", a = 0, location = options[["priorAlphaCauchyLocation"]], scale = options[["priorAlphaCauchyScale"]]), linetype = "dashed", color = "black")
      )
    }
    if (parNames[i] == "beta") {
      p <- switch(options[["priorBeta"]],
        "priorBetaHierarchical" = p,
        "priorBetaNormal" = p + ggplot2::stat_function(fun = stats::dnorm, geom = "line", args = list(mean = options[["priorBetaNormalMean"]], sd = options[["priorBetaNormalSd"]]), linetype = "dashed", color = "black"),
        "priorBetaStudent" = p + ggplot2::stat_function(fun = stats::dt, geom = "line", args = list(ncp = options[["priorBetaStudentLocation"]], df = options[["priorBetaStudentDf"]]), linetype = "dashed", color = "black"),
        "priorBetaCauchy" = p + ggplot2::stat_function(fun = stats::dcauchy, geom = "line", args = list(location = options[["priorBetaCauchyLocation"]], scale = options[["priorBetaCauchyScale"]]), linetype = "dashed", color = "black")
      )
    }
    if (parNames[i] == "gamma") {
      p <- switch(options[["priorGamma"]],
        "priorGammaHierarchical" = p,
        "priorGammaUniform" = p + ggplot2::stat_function(fun = stats::dunif, geom = "line", args = list(min = options[["priorGammaUniformMin"]], max = options[["priorGammaUniformMax"]]), linetype = "dashed", color = "black"),
        "priorGammaBeta" = p + ggplot2::stat_function(fun = stats::dbeta, geom = "line", args = list(shape1 = options[["priorGammaBetaAlpha"]], shape2 = options[["priorGammaBetaBeta"]]), linetype = "dashed", color = "black"),
        "priorGammaNormal" = p + ggplot2::stat_function(fun = truncdist::dtrunc, geom = "line", args = list(spec = "norm", a = 0, b = 1, mean = options[["priorGammaNormalMean"]], sd = options[["priorGammaNormalSd"]]), linetype = "dashed", color = "black"),
        "priorGammaStudent" = p + ggplot2::stat_function(fun = truncdist::dtrunc, geom = "line", args = list(spec = "t", a = 0, b = 1, ncp = options[["priorGammaStudentLocation"]], df = options[["priorGammaStudentDf"]]), linetype = "dashed", color = "black"),
        "priorGammaCauchy" = p + ggplot2::stat_function(fun = truncdist::dtrunc, geom = "line", args = list(spec = "cauchy", a = 0, b = 1, location = options[["priorGammaCauchyLocation"]], scale = options[["priorGammaCauchyScale"]]), linetype = "dashed", color = "black")
      )
    }
    if (parNames[i] == "delta") {
      p <- switch(options[["priorDelta"]],
        "priorDeltaHierarchical" = p,
        "priorDeltaUniform" = p + ggplot2::stat_function(fun = stats::dunif, geom = "line", args = list(min = options[["priorDeltaUniformMin"]], max = options[["priorDeltaUniformMax"]]), linetype = "dashed", color = "black"),
        "priorDeltaBeta" = p + ggplot2::stat_function(fun = stats::dbeta, geom = "line", args = list(shape1 = options[["priorDeltaBetaAlpha"]], shape2 = options[["priorDeltaBetaBeta"]]), linetype = "dashed", color = "black"),
        "priorDeltaNormal" = p + ggplot2::stat_function(fun = truncdist::dtrunc, geom = "line", args = list(spec = "norm", a = 0, b = 1, mean = options[["priorDeltaNormalMean"]], sd = options[["priorDeltaNormalSd"]]), linetype = "dashed", color = "black"),
        "priorDeltaStudent" = p + ggplot2::stat_function(fun = truncdist::dtrunc, geom = "line", args = list(spec = "t", a = 0, b = 1, ncp = options[["priorDeltaStudentLocation"]], df = options[["priorDeltaStudentDf"]]), linetype = "dashed", color = "black"),
        "priorDeltaCauchy" = p + ggplot2::stat_function(fun = truncdist::dtrunc, geom = "line", args = list(spec = "cauchy", a = 0, b = 1, location = options[["priorDeltaCauchyLocation"]], scale = options[["priorDeltaCauchyScale"]]), linetype = "dashed", color = "black")
      )
    }
    if (options[["plotPriorPosteriorLabels"]] && parNames[i] != "theta") {
      p <- p + ggrepel::geom_text_repel(data = subsubdata, mapping = ggplot2::aes(x = x, y = y + 0.01, label = label), inherit.aes = FALSE, color = colors, direction = "both", xlim = range(xBreaks), seed = 1, segment.color = "black", segment.linetype = "dashed")
    }
    fg$plotObject <- p
    if (has_hierarchical_prior) {
      scale <- switch(parNames[i],
        "theta" = options[["priorThetaHierarchicalSd"]],
        "alpha" = options[["priorAlphaHierarchicalSd"]],
        "beta" = options[["priorBetaHierarchicalSd"]],
        "gamma" = options[["priorGammaHierarchicalSd"]],
        "delta" = options[["priorDeltaHierarchicalSd"]],
        "zeta" = options[["priorZetaHierarchicalSd"]]
      )
      subdata <- subset(state[["plotDataPriorPosterior"]][["heterogeneity"]], state[["plotDataPriorPosterior"]][["heterogeneity"]]$param == parNames[i])
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(subdata$x, min.n = 4)
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, density(subdata$x)$y), min.n = 4)
      p <- ggplot2::ggplot(data = subdata, mapping = ggplot2::aes(x = x)) +
        ggplot2::geom_density() +
        ggplot2::stat_function(fun = truncdist::dtrunc, geom = "line", args = list(spec = "cauchy", a = 0, location = 0, scale = scale), linetype = "dashed", color = "black") +
        ggplot2::scale_x_continuous(name = gettext("Standard deviation"), breaks = xBreaks, limits = range(xBreaks)) +
        ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = range(yBreaks)) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw() +
        ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
      fg2$plotObject <- p
    }
  }
}

.bayesianIrtModelSpecs <- function(options, dataset) {
  items <- dataset[, unlist(options[["items"]]), drop = FALSE]
  data <- list()
  data[["y"]] <- unlist(c(items))
  data[["N"]] <- length(unlist(c(items)))
  data[["I"]] <- ncol(items)
  data[["P"]] <- nrow(items)
  data[["ii"]] <- rep(seq_len(ncol(items)), each = nrow(items))
  data[["pp"]] <- rep(seq_len(nrow(items)), times = ncol(items))
  if (length(options[["covariates"]]) > 0) {
    data[["x"]] <- as.matrix(scale(dataset[, unlist(options[["covariates"]]), drop = FALSE]))
  } else {
    data[["x"]] <- matrix(nrow = nrow(dataset), ncol = 0)
  }
  data[["K"]] <- ncol(data[["x"]])
  data[["D"]] <- options[["priorScaling"]]
  if (!options[["dichotomous"]]) {
    model <- jaspIrtStanModels:::stanmodels[["polytomous"]]
    data[["C"]] <- max(apply(items, 2, function(x) length(unique(x))))
    data[["modelType"]] <- match(options[["model"]], c("Rasch", "gpcm", "rsm", "grsm", "graded", "nominal"))
  } else {
    model <- jaspIrtStanModels:::stanmodels[["dichotomous"]]
    data[["modelType"]] <- match(options[["model"]], c("Rasch", "2PL", "3PL", "4PL"))
    # Prior for c's
    data[["priorGamma"]] <- match(options[["priorGamma"]], c("priorGammaUniform", "priorGammaBeta", "priorGammaNormal", "priorGammaStudent", "priorGammaCauchy", "priorGammaHierarchical"))
    data[["priorGammaUniformMin"]] <- options[["priorGammaUniformMin"]]
    data[["priorGammaUniformMax"]] <- options[["priorGammaUniformMax"]]
    data[["priorGammaBetaAlpha"]] <- options[["priorGammaBetaAlpha"]]
    data[["priorGammaBetaBeta"]] <- options[["priorGammaBetaBeta"]]
    data[["priorGammaNormalMean"]] <- options[["priorGammaNormalMean"]]
    data[["priorGammaNormalSd"]] <- options[["priorGammaNormalSd"]]
    data[["priorGammaStudentLocation"]] <- options[["priorGammaStudentLocation"]]
    data[["priorGammaStudentDf"]] <- options[["priorGammaStudentDf"]]
    data[["priorGammaCauchyLocation"]] <- options[["priorGammaCauchyLocation"]]
    data[["priorGammaCauchyScale"]] <- options[["priorGammaCauchyScale"]]
    data[["priorGammaHierarchicalSd"]] <- options[["priorGammaHierarchicalSd"]]
    # Prior for d's
    data[["priorDelta"]] <- match(options[["priorDelta"]], c("priorDeltaUniform", "priorDeltaBeta", "priorDeltaNormal", "priorDeltaStudent", "priorDeltaCauchy", "priorDeltaHierarchical"))
    data[["priorDeltaUniformMin"]] <- options[["priorDeltaUniformMin"]]
    data[["priorDeltaUniformMax"]] <- options[["priorDeltaUniformMax"]]
    data[["priorDeltaBetaAlpha"]] <- options[["priorDeltaBetaAlpha"]]
    data[["priorDeltaBetaBeta"]] <- options[["priorDeltaBetaBeta"]]
    data[["priorDeltaNormalMean"]] <- options[["priorDeltaNormalMean"]]
    data[["priorDeltaNormalSd"]] <- options[["priorDeltaNormalSd"]]
    data[["priorDeltaStudentLocation"]] <- options[["priorDeltaStudentLocation"]]
    data[["priorDeltaStudentDf"]] <- options[["priorDeltaStudentDf"]]
    data[["priorDeltaCauchyLocation"]] <- options[["priorDeltaCauchyLocation"]]
    data[["priorDeltaCauchyScale"]] <- options[["priorDeltaCauchyScale"]]
    data[["priorDeltaHierarchicalSd"]] <- options[["priorDeltaHierarchicalSd"]]
  }
  # Prior for theta's
  data[["priorTheta"]] <- match(options[["priorTheta"]], c("priorThetaNormal", "priorThetaStudent", "priorThetaCauchy", "priorThetaHierarchical"))
  data[["priorThetaNormalMean"]] <- options[["priorThetaNormalMean"]]
  data[["priorThetaNormalSd"]] <- options[["priorThetaNormalSd"]]
  data[["priorThetaStudentLocation"]] <- options[["priorThetaStudentLocation"]]
  data[["priorThetaStudentDf"]] <- options[["priorThetaStudentDf"]]
  data[["priorThetaCauchyLocation"]] <- options[["priorThetaCauchyLocation"]]
  data[["priorThetaCauchyScale"]] <- options[["priorThetaCauchyScale"]]
  data[["priorThetaHierarchicalSd"]] <- options[["priorThetaHierarchicalSd"]]
  # Prior for b's
  data[["priorBeta"]] <- match(options[["priorBeta"]], c("priorBetaNormal", "priorBetaStudent", "priorBetaCauchy", "priorBetaHierarchical"))
  data[["priorBetaNormalMean"]] <- options[["priorBetaNormalMean"]]
  data[["priorBetaNormalSd"]] <- options[["priorBetaNormalSd"]]
  data[["priorBetaStudentLocation"]] <- options[["priorBetaStudentLocation"]]
  data[["priorBetaStudentDf"]] <- options[["priorBetaStudentDf"]]
  data[["priorBetaCauchyLocation"]] <- options[["priorBetaCauchyLocation"]]
  data[["priorBetaCauchyScale"]] <- options[["priorBetaCauchyScale"]]
  data[["priorBetaHierarchicalSd"]] <- options[["priorBetaHierarchicalSd"]]
  # Prior for a's
  data[["priorAlpha"]] <- match(options[["priorAlpha"]], c("priorAlphaLogNormal", "priorAlphaNormal", "priorAlphaStudent", "priorAlphaCauchy", "priorAlphaHierarchical"))
  data[["priorAlphaLogNormalMean"]] <- options[["priorAlphaLogNormalMean"]]
  data[["priorAlphaLogNormalSd"]] <- options[["priorAlphaLogNormalSd"]]
  data[["priorAlphaNormalMean"]] <- options[["priorAlphaNormalMean"]]
  data[["priorAlphaNormalSd"]] <- options[["priorAlphaNormalSd"]]
  data[["priorAlphaStudentLocation"]] <- options[["priorAlphaStudentLocation"]]
  data[["priorAlphaStudentDf"]] <- options[["priorAlphaStudentDf"]]
  data[["priorAlphaCauchyLocation"]] <- options[["priorAlphaCauchyLocation"]]
  data[["priorAlphaCauchyScale"]] <- options[["priorAlphaCauchyScale"]]
  data[["priorAlphaHierarchicalSd"]] <- options[["priorAlphaHierarchicalSd"]]
  # Prior for latent regression coefficients
  data[["priorZeta"]] <- match(options[["priorZeta"]], c("priorZetaNormal", "priorZetaStudent", "priorZetaCauchy", "priorZetaHierarchical"))
  data[["priorZetaNormalMean"]] <- options[["priorZetaNormalMean"]]
  data[["priorZetaNormalSd"]] <- options[["priorZetaNormalSd"]]
  data[["priorZetaStudentLocation"]] <- options[["priorZetaStudentLocation"]]
  data[["priorZetaStudentDf"]] <- options[["priorZetaStudentDf"]]
  data[["priorZetaCauchyLocation"]] <- options[["priorZetaCauchyLocation"]]
  data[["priorZetaCauchyScale"]] <- options[["priorZetaCauchyScale"]]
  data[["priorZetaHierarchicalSd"]] <- options[["priorZetaHierarchicalSd"]]
  # Prior for threshold parameters
  data[["priorThreshold"]] <- match(options[["priorThreshold"]], c("priorThresholdNormal", "priorThresholdStudent", "priorThresholdCauchy", "priorThresholdUniform"))
  data[["priorThresholdNormalMean"]] <- options[["priorThresholdNormalMean"]]
  data[["priorThresholdNormalSd"]] <- options[["priorThresholdNormalSd"]]
  data[["priorThresholdStudentLocation"]] <- options[["priorThresholdStudentLocation"]]
  data[["priorThresholdStudentDf"]] <- options[["priorThresholdStudentDf"]]
  data[["priorThresholdCauchyLocation"]] <- options[["priorThresholdCauchyLocation"]]
  data[["priorThresholdCauchyScale"]] <- options[["priorThresholdCauchyScale"]]
  data[["priorThresholdUniformMin"]] <- options[["priorThresholdUniformMin"]]
  data[["priorThresholdUniformMax"]] <- options[["priorThresholdUniformMax"]]
  # Parameters to monitor
  pars <- c("theta")
  theta_sigma <- if (options[["priorTheta"]] == "priorThetaHierarchical") "theta_sigma" else NULL
  alpha_sigma <- if (options[["priorAlpha"]] == "priorAlphaHierarchical") "alpha_sigma" else NULL
  alpha_cat_sigma <- if (options[["priorAlpha"]] == "priorAlphaHierarchical") "alpha_cat_sigma" else NULL
  beta_sigma <- if (options[["priorBeta"]] == "priorBetaHierarchical") "beta_sigma" else NULL
  beta_cat_sigma <- if (options[["priorBeta"]] == "priorBetaHierarchical") "beta_cat_sigma" else NULL
  beta_rsm_sigma <- if (options[["priorBeta"]] == "priorBetaHierarchical") "beta_rsm_sigma" else NULL
  gamma_sigma <- if (options[["priorGamma"]] == "priorGammaHierarchical") "gamma_sigma" else NULL
  delta_sigma <- if (options[["priorDelta"]] == "priorDeltaHierarchical") "delta_sigma" else NULL
  pars <- switch(options[["model"]],
    "Rasch" = c("theta", theta_sigma, "beta", beta_sigma, "ppp"),
    "2PL" = c("theta", theta_sigma, "alpha", alpha_sigma, "beta", beta_sigma, "ppp"),
    "3PL" = c("theta", theta_sigma, "alpha", alpha_sigma, "beta", beta_sigma, "gamma", gamma_sigma, "ppp"),
    "4PL" = c("theta", theta_sigma, "alpha", alpha_sigma, "beta", beta_sigma, "gamma", gamma_sigma, "delta", delta_sigma, "ppp"),
    "gpcm" = c("theta", theta_sigma, "alpha", alpha_sigma, "beta", beta_sigma, "ppp"),
    "rsm" = c("theta", theta_sigma, "beta_rsm", beta_rsm_sigma, "threshold", "ppp"),
    "grsm" = c("theta", theta_sigma, "alpha", alpha_sigma, "beta_rsm", beta_rsm_sigma, "threshold", "ppp"),
    "graded" = c("theta", theta_sigma, "alpha", alpha_sigma, "beta", beta_sigma, "ppp"),
    "nominal" = c("theta", theta_sigma, "alpha_cat", alpha_cat_sigma, "beta_cat", beta_cat_sigma, "ppp")
  )
  if (length(options[["covariates"]]) > 0) {
    pars <- c(pars, "zeta", if (options[["priorZeta"]] == "priorZetaHierarchical") "zeta_sigma" else NULL, "epsilon", "epsilon_sigma")
  }
  result <- list(model = model, data = data, pars = pars)
  return(result)
}
