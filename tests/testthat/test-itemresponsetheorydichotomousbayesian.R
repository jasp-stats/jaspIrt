context("[IRT] Bayesian Dichotomous Item Response Theory")

options("jaspRoundToPrecision" = function(x) signif(round(x, digits = 2), digits = 2))

# Consistency test 1, file: binary.csv, model: Rasch ###########################

options <- initIRTOptions("itemResponseTheoryDichotomousBayesian")
options$burnin <- 1000
options$chains <- 4
options$covariates <- list()
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3", "Q4")
options$model <- "Rasch"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemCharacteristicGroup <- TRUE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemInformationLabels <- FALSE
options$plotPriorPosterior <- TRUE
options$plotPriorPosteriorLabels <- FALSE
options$plotTestInformation <- TRUE
options$priorAlpha <- "priorAlphaLogNormal"
options$priorAlphaCauchyLocation <- 0
options$priorAlphaCauchyScale <- 1
options$priorAlphaHierarchicalSd <- 1
options$priorAlphaLogNormalMean <- 0
options$priorAlphaLogNormalSd <- 1
options$priorAlphaNormalMean <- 0
options$priorAlphaNormalSd <- 1
options$priorAlphaStudentDf <- 1
options$priorAlphaStudentLocation <- 0
options$priorBeta <- "priorBetaNormal"
options$priorBetaCauchyLocation <- 0
options$priorBetaCauchyScale <- 1
options$priorBetaHierarchicalSd <- 1
options$priorBetaNormalMean <- 0
options$priorBetaNormalSd <- 1
options$priorBetaStudentDf <- 1
options$priorBetaStudentLocation <- 0
options$priorDelta <- "priorDeltaNormal"
options$priorDeltaBetaAlpha <- 1
options$priorDeltaBetaBeta <- 1
options$priorDeltaCauchyLocation <- 0
options$priorDeltaCauchyScale <- 1
options$priorDeltaHierarchicalSd <- 0.1
options$priorDeltaNormalMean <- 1
options$priorDeltaNormalSd <- 0.1
options$priorDeltaStudentDf <- 1
options$priorDeltaStudentLocation <- 1
options$priorDeltaUniformMax <- 1
options$priorDeltaUniformMin <- 0.75
options$priorGamma <- "priorGammaNormal"
options$priorGammaBetaAlpha <- 1
options$priorGammaBetaBeta <- 1
options$priorGammaCauchyLocation <- 0
options$priorGammaCauchyScale <- 1
options$priorGammaHierarchicalSd <- 0.1
options$priorGammaNormalMean <- 0
options$priorGammaNormalSd <- 0.1
options$priorGammaStudentDf <- 1
options$priorGammaStudentLocation <- 0
options$priorGammaUniformMax <- 0.25
options$priorGammaUniformMin <- 0
options$priorScaling <- 1.702
options$priorTheta <- "priorThetaNormal"
options$priorThetaCauchyLocation <- 0
options$priorThetaCauchyScale <- 1
options$priorThetaHierarchicalSd <- 1
options$priorThetaNormalMean <- 0
options$priorThetaNormalSd <- 1
options$priorThetaStudentDf <- 1
options$priorThetaStudentLocation <- 0
options$priorThreshold <- "priorThresholdNormal"
options$priorThresholdCauchyLocation <- 0
options$priorThresholdCauchyScale <- 1
options$priorThresholdNormalMean <- 0
options$priorThresholdNormalSd <- 1
options$priorThresholdStudentDf <- 1
options$priorThresholdStudentLocation <- 1
options$priorThresholdUniformMax <- 0
options$priorThresholdUniformMin <- 1
options$priorZeta <- "priorZetaNormal"
options$priorZetaCauchyLocation <- 0
options$priorZetaCauchyScale <- 1
options$priorZetaHierarchicalSd <- 1
options$priorZetaNormalMean <- 0
options$priorZetaNormalSd <- 1
options$priorZetaStudentDf <- 1
options$priorZetaStudentLocation <- 0
options$samples <- 2000
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryDichotomousBayesian", "binary.csv", options)

test_that("Item Information table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -0.756207803974456, "Q1", 0.46525, 0.474385673717423, "Q2", 0.5625,
      -0.614866593873767, "Q3", 0.47025, -1.66159711852612, "Q4",
      0.44275
    )
  )
})

test_that("Model Summary table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.997033302291133, 0.4851875, 4, 1.0231987080494, 99)
  )
})

# Consistency test 2, file: binary2.csv, model: Rasch ##########################

options <- initIRTOptions("itemResponseTheoryDichotomousBayesian")
options$burnin <- 1000
options$chains <- 4
options$covariates <- list()
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3", "Q4")
options$model <- "Rasch"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemCharacteristicGroup <- TRUE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemInformationLabels <- FALSE
options$plotPriorPosterior <- TRUE
options$plotPriorPosteriorLabels <- FALSE
options$plotTestInformation <- TRUE
options$priorAlpha <- "priorAlphaLogNormal"
options$priorAlphaCauchyLocation <- 0
options$priorAlphaCauchyScale <- 1
options$priorAlphaHierarchicalSd <- 1
options$priorAlphaLogNormalMean <- 0
options$priorAlphaLogNormalSd <- 1
options$priorAlphaNormalMean <- 0
options$priorAlphaNormalSd <- 1
options$priorAlphaStudentDf <- 1
options$priorAlphaStudentLocation <- 0
options$priorBeta <- "priorBetaNormal"
options$priorBetaCauchyLocation <- 0
options$priorBetaCauchyScale <- 1
options$priorBetaHierarchicalSd <- 1
options$priorBetaNormalMean <- 0
options$priorBetaNormalSd <- 1
options$priorBetaStudentDf <- 1
options$priorBetaStudentLocation <- 0
options$priorDelta <- "priorDeltaNormal"
options$priorDeltaBetaAlpha <- 1
options$priorDeltaBetaBeta <- 1
options$priorDeltaCauchyLocation <- 0
options$priorDeltaCauchyScale <- 1
options$priorDeltaHierarchicalSd <- 0.1
options$priorDeltaNormalMean <- 1
options$priorDeltaNormalSd <- 0.1
options$priorDeltaStudentDf <- 1
options$priorDeltaStudentLocation <- 1
options$priorDeltaUniformMax <- 1
options$priorDeltaUniformMin <- 0.75
options$priorGamma <- "priorGammaNormal"
options$priorGammaBetaAlpha <- 1
options$priorGammaBetaBeta <- 1
options$priorGammaCauchyLocation <- 0
options$priorGammaCauchyScale <- 1
options$priorGammaHierarchicalSd <- 0.1
options$priorGammaNormalMean <- 0
options$priorGammaNormalSd <- 0.1
options$priorGammaStudentDf <- 1
options$priorGammaStudentLocation <- 0
options$priorGammaUniformMax <- 0.25
options$priorGammaUniformMin <- 0
options$priorScaling <- 1.702
options$priorTheta <- "priorThetaNormal"
options$priorThetaCauchyLocation <- 0
options$priorThetaCauchyScale <- 1
options$priorThetaHierarchicalSd <- 1
options$priorThetaNormalMean <- 0
options$priorThetaNormalSd <- 1
options$priorThetaStudentDf <- 1
options$priorThetaStudentLocation <- 0
options$priorThreshold <- "priorThresholdNormal"
options$priorThresholdCauchyLocation <- 0
options$priorThresholdCauchyScale <- 1
options$priorThresholdNormalMean <- 0
options$priorThresholdNormalSd <- 1
options$priorThresholdStudentDf <- 1
options$priorThresholdStudentLocation <- 1
options$priorThresholdUniformMax <- 0
options$priorThresholdUniformMin <- 1
options$priorZeta <- "priorZetaNormal"
options$priorZetaCauchyLocation <- 0
options$priorZetaCauchyScale <- 1
options$priorZetaHierarchicalSd <- 1
options$priorZetaNormalMean <- 0
options$priorZetaNormalSd <- 1
options$priorZetaStudentDf <- 1
options$priorZetaStudentLocation <- 0
options$samples <- 2000
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryDichotomousBayesian", "binary2.csv", options)

test_that("Item Information table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -1.4175561180533, "Q1", 0.40375, -0.233963030392934, "Q2", 0.5155,
      0.279865875736186, "Q3", 0.575, 0.195978840856061, "Q4", 0.564
    )
  )
})

test_that("Model Summary table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.966572891686646, 0.5145625, 4, 1.01690069055497, 50)
  )
})

# Consistency test 3, file: binary.csv, model: 2-PL ############################

options <- initIRTOptions("itemResponseTheoryDichotomousBayesian")
options$burnin <- 1000
options$chains <- 4
options$covariates <- list()
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3", "Q4")
options$model <- "2PL"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemCharacteristicGroup <- TRUE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemInformationLabels <- FALSE
options$plotPriorPosterior <- TRUE
options$plotPriorPosteriorLabels <- FALSE
options$plotTestInformation <- TRUE
options$priorAlpha <- "priorAlphaLogNormal"
options$priorAlphaCauchyLocation <- 0
options$priorAlphaCauchyScale <- 1
options$priorAlphaHierarchicalSd <- 1
options$priorAlphaLogNormalMean <- 0
options$priorAlphaLogNormalSd <- 1
options$priorAlphaNormalMean <- 0
options$priorAlphaNormalSd <- 1
options$priorAlphaStudentDf <- 1
options$priorAlphaStudentLocation <- 0
options$priorBeta <- "priorBetaNormal"
options$priorBetaCauchyLocation <- 0
options$priorBetaCauchyScale <- 1
options$priorBetaHierarchicalSd <- 1
options$priorBetaNormalMean <- 0
options$priorBetaNormalSd <- 1
options$priorBetaStudentDf <- 1
options$priorBetaStudentLocation <- 0
options$priorDelta <- "priorDeltaNormal"
options$priorDeltaBetaAlpha <- 1
options$priorDeltaBetaBeta <- 1
options$priorDeltaCauchyLocation <- 0
options$priorDeltaCauchyScale <- 1
options$priorDeltaHierarchicalSd <- 0.1
options$priorDeltaNormalMean <- 1
options$priorDeltaNormalSd <- 0.1
options$priorDeltaStudentDf <- 1
options$priorDeltaStudentLocation <- 1
options$priorDeltaUniformMax <- 1
options$priorDeltaUniformMin <- 0.75
options$priorGamma <- "priorGammaNormal"
options$priorGammaBetaAlpha <- 1
options$priorGammaBetaBeta <- 1
options$priorGammaCauchyLocation <- 0
options$priorGammaCauchyScale <- 1
options$priorGammaHierarchicalSd <- 0.1
options$priorGammaNormalMean <- 0
options$priorGammaNormalSd <- 0.1
options$priorGammaStudentDf <- 1
options$priorGammaStudentLocation <- 0
options$priorGammaUniformMax <- 0.25
options$priorGammaUniformMin <- 0
options$priorScaling <- 1.702
options$priorTheta <- "priorThetaNormal"
options$priorThetaCauchyLocation <- 0
options$priorThetaCauchyScale <- 1
options$priorThetaHierarchicalSd <- 1
options$priorThetaNormalMean <- 0
options$priorThetaNormalSd <- 1
options$priorThetaStudentDf <- 1
options$priorThetaStudentLocation <- 0
options$priorThreshold <- "priorThresholdNormal"
options$priorThresholdCauchyLocation <- 0
options$priorThresholdCauchyScale <- 1
options$priorThresholdNormalMean <- 0
options$priorThresholdNormalSd <- 1
options$priorThresholdStudentDf <- 1
options$priorThresholdStudentLocation <- 1
options$priorThresholdUniformMax <- 0
options$priorThresholdUniformMin <- 1
options$priorZeta <- "priorZetaNormal"
options$priorZetaCauchyLocation <- 0
options$priorZetaCauchyScale <- 1
options$priorZetaHierarchicalSd <- 1
options$priorZetaNormalMean <- 0
options$priorZetaNormalSd <- 1
options$priorZetaStudentDf <- 1
options$priorZetaStudentLocation <- 0
options$samples <- 2000
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryDichotomousBayesian", "binary.csv", options)

test_that("Item Information table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -0.851853446726259, 0.622839097338647, "Q1", 0.39775, 0.470271043295749,
      0.651541286905622, "Q2", 0.54775, -0.694061286006959, 0.635939398295682,
      "Q3", 0.5285, -1.13963060772471, 1.86720320441591, "Q4", 0.5785
    )
  )
})

test_that("Model Summary table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.778346590961187, 0.513125, 4, 1.5935776980596, 99)
  )
})

# Consistency test 4, file: binary2.csv, model: 2-PL ###########################

options <- initIRTOptions("itemResponseTheoryDichotomousBayesian")
options$burnin <- 1000
options$chains <- 4
options$covariates <- list()
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3", "Q4")
options$model <- "2PL"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemCharacteristicGroup <- TRUE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemInformationLabels <- FALSE
options$plotPriorPosterior <- TRUE
options$plotPriorPosteriorLabels <- FALSE
options$plotTestInformation <- TRUE
options$priorAlpha <- "priorAlphaLogNormal"
options$priorAlphaCauchyLocation <- 0
options$priorAlphaCauchyScale <- 1
options$priorAlphaHierarchicalSd <- 1
options$priorAlphaLogNormalMean <- 0
options$priorAlphaLogNormalSd <- 1
options$priorAlphaNormalMean <- 0
options$priorAlphaNormalSd <- 1
options$priorAlphaStudentDf <- 1
options$priorAlphaStudentLocation <- 0
options$priorBeta <- "priorBetaNormal"
options$priorBetaCauchyLocation <- 0
options$priorBetaCauchyScale <- 1
options$priorBetaHierarchicalSd <- 1
options$priorBetaNormalMean <- 0
options$priorBetaNormalSd <- 1
options$priorBetaStudentDf <- 1
options$priorBetaStudentLocation <- 0
options$priorDelta <- "priorDeltaNormal"
options$priorDeltaBetaAlpha <- 1
options$priorDeltaBetaBeta <- 1
options$priorDeltaCauchyLocation <- 0
options$priorDeltaCauchyScale <- 1
options$priorDeltaHierarchicalSd <- 0.1
options$priorDeltaNormalMean <- 1
options$priorDeltaNormalSd <- 0.1
options$priorDeltaStudentDf <- 1
options$priorDeltaStudentLocation <- 1
options$priorDeltaUniformMax <- 1
options$priorDeltaUniformMin <- 0.75
options$priorGamma <- "priorGammaNormal"
options$priorGammaBetaAlpha <- 1
options$priorGammaBetaBeta <- 1
options$priorGammaCauchyLocation <- 0
options$priorGammaCauchyScale <- 1
options$priorGammaHierarchicalSd <- 0.1
options$priorGammaNormalMean <- 0
options$priorGammaNormalSd <- 0.1
options$priorGammaStudentDf <- 1
options$priorGammaStudentLocation <- 0
options$priorGammaUniformMax <- 0.25
options$priorGammaUniformMin <- 0
options$priorScaling <- 1.702
options$priorTheta <- "priorThetaNormal"
options$priorThetaCauchyLocation <- 0
options$priorThetaCauchyScale <- 1
options$priorThetaHierarchicalSd <- 1
options$priorThetaNormalMean <- 0
options$priorThetaNormalSd <- 1
options$priorThetaStudentDf <- 1
options$priorThetaStudentLocation <- 0
options$priorThreshold <- "priorThresholdNormal"
options$priorThresholdCauchyLocation <- 0
options$priorThresholdCauchyScale <- 1
options$priorThresholdNormalMean <- 0
options$priorThresholdNormalSd <- 1
options$priorThresholdStudentDf <- 1
options$priorThresholdStudentLocation <- 1
options$priorThresholdUniformMax <- 0
options$priorThresholdUniformMin <- 1
options$priorZeta <- "priorZetaNormal"
options$priorZetaCauchyLocation <- 0
options$priorZetaCauchyScale <- 1
options$priorZetaHierarchicalSd <- 1
options$priorZetaNormalMean <- 0
options$priorZetaNormalSd <- 1
options$priorZetaStudentDf <- 1
options$priorZetaStudentLocation <- 0
options$samples <- 2000
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryDichotomousBayesian", "binary2.csv", options)

test_that("Item Information table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -1.23476220723894, 1.2903171963234, "Q1", 0.512, -0.326449251891161,
      0.852329914591813, "Q2", 0.54975, 0.340510011334715, 0.357499122683821,
      "Q3", 0.66625, 0.201329312209326, 0.377532804908692, "Q4", 0.632
    )
  )
})

test_that("Model Summary table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.695074074757175, 0.59, 4, 1.34246821558699, 50)
  )
})

# Consistency test 5, file: binary.csv, model: 3-PL ############################

options <- initIRTOptions("itemResponseTheoryDichotomousBayesian")
options$burnin <- 1000
options$chains <- 4
options$covariates <- list()
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3", "Q4")
options$model <- "3PL"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemCharacteristicGroup <- TRUE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemInformationLabels <- FALSE
options$plotPriorPosterior <- TRUE
options$plotPriorPosteriorLabels <- FALSE
options$plotTestInformation <- TRUE
options$priorAlpha <- "priorAlphaLogNormal"
options$priorAlphaCauchyLocation <- 0
options$priorAlphaCauchyScale <- 1
options$priorAlphaHierarchicalSd <- 1
options$priorAlphaLogNormalMean <- 0
options$priorAlphaLogNormalSd <- 1
options$priorAlphaNormalMean <- 0
options$priorAlphaNormalSd <- 1
options$priorAlphaStudentDf <- 1
options$priorAlphaStudentLocation <- 0
options$priorBeta <- "priorBetaNormal"
options$priorBetaCauchyLocation <- 0
options$priorBetaCauchyScale <- 1
options$priorBetaHierarchicalSd <- 1
options$priorBetaNormalMean <- 0
options$priorBetaNormalSd <- 1
options$priorBetaStudentDf <- 1
options$priorBetaStudentLocation <- 0
options$priorDelta <- "priorDeltaNormal"
options$priorDeltaBetaAlpha <- 1
options$priorDeltaBetaBeta <- 1
options$priorDeltaCauchyLocation <- 0
options$priorDeltaCauchyScale <- 1
options$priorDeltaHierarchicalSd <- 0.1
options$priorDeltaNormalMean <- 1
options$priorDeltaNormalSd <- 0.1
options$priorDeltaStudentDf <- 1
options$priorDeltaStudentLocation <- 1
options$priorDeltaUniformMax <- 1
options$priorDeltaUniformMin <- 0.75
options$priorGamma <- "priorGammaNormal"
options$priorGammaBetaAlpha <- 1
options$priorGammaBetaBeta <- 1
options$priorGammaCauchyLocation <- 0
options$priorGammaCauchyScale <- 1
options$priorGammaHierarchicalSd <- 0.1
options$priorGammaNormalMean <- 0
options$priorGammaNormalSd <- 0.1
options$priorGammaStudentDf <- 1
options$priorGammaStudentLocation <- 0
options$priorGammaUniformMax <- 0.25
options$priorGammaUniformMin <- 0
options$priorScaling <- 1.702
options$priorTheta <- "priorThetaNormal"
options$priorThetaCauchyLocation <- 0
options$priorThetaCauchyScale <- 1
options$priorThetaHierarchicalSd <- 1
options$priorThetaNormalMean <- 0
options$priorThetaNormalSd <- 1
options$priorThetaStudentDf <- 1
options$priorThetaStudentLocation <- 0
options$priorThreshold <- "priorThresholdNormal"
options$priorThresholdCauchyLocation <- 0
options$priorThresholdCauchyScale <- 1
options$priorThresholdNormalMean <- 0
options$priorThresholdNormalSd <- 1
options$priorThresholdStudentDf <- 1
options$priorThresholdStudentLocation <- 1
options$priorThresholdUniformMax <- 0
options$priorThresholdUniformMin <- 1
options$priorZeta <- "priorZetaNormal"
options$priorZetaCauchyLocation <- 0
options$priorZetaCauchyScale <- 1
options$priorZetaHierarchicalSd <- 1
options$priorZetaNormalMean <- 0
options$priorZetaNormalSd <- 1
options$priorZetaStudentDf <- 1
options$priorZetaStudentLocation <- 0
options$samples <- 2000
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryDichotomousBayesian", "binary.csv", options)

test_that("Item Information table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -0.7232486013253, 0.501680704060429, 0.0938957959872284, "Q1",
      0.45325, 0.667258969963348, 1.18430929393727, 0.0842779878981198,
      "Q2", 0.55975, -0.473370858518562, 0.721573003456158, 0.0868486887027245,
      "Q3", 0.49875, -1.02796463893112, 2.29388095813566, 0.0856291343163646,
      "Q4", 0.516
    )
  )
})

test_that("Model Summary table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.522735357952333, 0.5069375, 4, 1.00506065050154, 99)
  )
})

# Consistency test 6, file: binary2.csv, model: 3-PL ###########################

options <- initIRTOptions("itemResponseTheoryDichotomousBayesian")
options$burnin <- 1000
options$chains <- 4
options$covariates <- list()
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3", "Q4")
options$model <- "3PL"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemCharacteristicGroup <- TRUE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemInformationLabels <- FALSE
options$plotPriorPosterior <- TRUE
options$plotPriorPosteriorLabels <- FALSE
options$plotTestInformation <- TRUE
options$priorAlpha <- "priorAlphaLogNormal"
options$priorAlphaCauchyLocation <- 0
options$priorAlphaCauchyScale <- 1
options$priorAlphaHierarchicalSd <- 1
options$priorAlphaLogNormalMean <- 0
options$priorAlphaLogNormalSd <- 1
options$priorAlphaNormalMean <- 0
options$priorAlphaNormalSd <- 1
options$priorAlphaStudentDf <- 1
options$priorAlphaStudentLocation <- 0
options$priorBeta <- "priorBetaNormal"
options$priorBetaCauchyLocation <- 0
options$priorBetaCauchyScale <- 1
options$priorBetaHierarchicalSd <- 1
options$priorBetaNormalMean <- 0
options$priorBetaNormalSd <- 1
options$priorBetaStudentDf <- 1
options$priorBetaStudentLocation <- 0
options$priorDelta <- "priorDeltaNormal"
options$priorDeltaBetaAlpha <- 1
options$priorDeltaBetaBeta <- 1
options$priorDeltaCauchyLocation <- 0
options$priorDeltaCauchyScale <- 1
options$priorDeltaHierarchicalSd <- 0.1
options$priorDeltaNormalMean <- 1
options$priorDeltaNormalSd <- 0.1
options$priorDeltaStudentDf <- 1
options$priorDeltaStudentLocation <- 1
options$priorDeltaUniformMax <- 1
options$priorDeltaUniformMin <- 0.75
options$priorGamma <- "priorGammaNormal"
options$priorGammaBetaAlpha <- 1
options$priorGammaBetaBeta <- 1
options$priorGammaCauchyLocation <- 0
options$priorGammaCauchyScale <- 1
options$priorGammaHierarchicalSd <- 0.1
options$priorGammaNormalMean <- 0
options$priorGammaNormalSd <- 0.1
options$priorGammaStudentDf <- 1
options$priorGammaStudentLocation <- 0
options$priorGammaUniformMax <- 0.25
options$priorGammaUniformMin <- 0
options$priorScaling <- 1.702
options$priorTheta <- "priorThetaNormal"
options$priorThetaCauchyLocation <- 0
options$priorThetaCauchyScale <- 1
options$priorThetaHierarchicalSd <- 1
options$priorThetaNormalMean <- 0
options$priorThetaNormalSd <- 1
options$priorThetaStudentDf <- 1
options$priorThetaStudentLocation <- 0
options$priorThreshold <- "priorThresholdNormal"
options$priorThresholdCauchyLocation <- 0
options$priorThresholdCauchyScale <- 1
options$priorThresholdNormalMean <- 0
options$priorThresholdNormalSd <- 1
options$priorThresholdStudentDf <- 1
options$priorThresholdStudentLocation <- 1
options$priorThresholdUniformMax <- 0
options$priorThresholdUniformMin <- 1
options$priorZeta <- "priorZetaNormal"
options$priorZetaCauchyLocation <- 0
options$priorZetaCauchyScale <- 1
options$priorZetaHierarchicalSd <- 1
options$priorZetaNormalMean <- 0
options$priorZetaNormalSd <- 1
options$priorZetaStudentDf <- 1
options$priorZetaStudentLocation <- 0
options$samples <- 2000
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryDichotomousBayesian", "binary2.csv", options)

test_that("Item Information table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -1.08915927885119, 1.44399491690632, 0.0903526425502477, "Q1",
      0.45975, -0.066699707676913, 1.13991781316919, 0.0844017949758871,
      "Q2", 0.52375, 0.588835076682088, 0.604884737055777, 0.0859332719817475,
      "Q3", 0.6185, 0.454295260626238, 0.488017537497509, 0.0795038940264037,
      "Q4", 0.62875
    )
  )
})

test_that("Model Summary table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.625283252824509, 0.5576875, 4, 1.00947393154312, 50)
  )
})

# Consistency test 7, file: binary.csv, model: 4-PL ############################

options <- initIRTOptions("itemResponseTheoryDichotomousBayesian")
options$burnin <- 1000
options$chains <- 4
options$covariates <- list()
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3", "Q4")
options$model <- "4PL"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemCharacteristicGroup <- TRUE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemInformationLabels <- FALSE
options$plotPriorPosterior <- TRUE
options$plotPriorPosteriorLabels <- FALSE
options$plotTestInformation <- TRUE
options$priorAlpha <- "priorAlphaLogNormal"
options$priorAlphaCauchyLocation <- 0
options$priorAlphaCauchyScale <- 1
options$priorAlphaHierarchicalSd <- 1
options$priorAlphaLogNormalMean <- 0
options$priorAlphaLogNormalSd <- 1
options$priorAlphaNormalMean <- 0
options$priorAlphaNormalSd <- 1
options$priorAlphaStudentDf <- 1
options$priorAlphaStudentLocation <- 0
options$priorBeta <- "priorBetaNormal"
options$priorBetaCauchyLocation <- 0
options$priorBetaCauchyScale <- 1
options$priorBetaHierarchicalSd <- 1
options$priorBetaNormalMean <- 0
options$priorBetaNormalSd <- 1
options$priorBetaStudentDf <- 1
options$priorBetaStudentLocation <- 0
options$priorDelta <- "priorDeltaNormal"
options$priorDeltaBetaAlpha <- 1
options$priorDeltaBetaBeta <- 1
options$priorDeltaCauchyLocation <- 0
options$priorDeltaCauchyScale <- 1
options$priorDeltaHierarchicalSd <- 0.1
options$priorDeltaNormalMean <- 1
options$priorDeltaNormalSd <- 0.1
options$priorDeltaStudentDf <- 1
options$priorDeltaStudentLocation <- 1
options$priorDeltaUniformMax <- 1
options$priorDeltaUniformMin <- 0.75
options$priorGamma <- "priorGammaNormal"
options$priorGammaBetaAlpha <- 1
options$priorGammaBetaBeta <- 1
options$priorGammaCauchyLocation <- 0
options$priorGammaCauchyScale <- 1
options$priorGammaHierarchicalSd <- 0.1
options$priorGammaNormalMean <- 0
options$priorGammaNormalSd <- 0.1
options$priorGammaStudentDf <- 1
options$priorGammaStudentLocation <- 0
options$priorGammaUniformMax <- 0.25
options$priorGammaUniformMin <- 0
options$priorScaling <- 1.702
options$priorTheta <- "priorThetaNormal"
options$priorThetaCauchyLocation <- 0
options$priorThetaCauchyScale <- 1
options$priorThetaHierarchicalSd <- 1
options$priorThetaNormalMean <- 0
options$priorThetaNormalSd <- 1
options$priorThetaStudentDf <- 1
options$priorThetaStudentLocation <- 0
options$priorThreshold <- "priorThresholdNormal"
options$priorThresholdCauchyLocation <- 0
options$priorThresholdCauchyScale <- 1
options$priorThresholdNormalMean <- 0
options$priorThresholdNormalSd <- 1
options$priorThresholdStudentDf <- 1
options$priorThresholdStudentLocation <- 1
options$priorThresholdUniformMax <- 0
options$priorThresholdUniformMin <- 1
options$priorZeta <- "priorZetaNormal"
options$priorZetaCauchyLocation <- 0
options$priorZetaCauchyScale <- 1
options$priorZetaHierarchicalSd <- 1
options$priorZetaNormalMean <- 0
options$priorZetaNormalSd <- 1
options$priorZetaStudentDf <- 1
options$priorZetaStudentLocation <- 0
options$samples <- 2000
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryDichotomousBayesian", "binary.csv", options)

test_that("Item Information table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -0.940214836965288, 0.950731814304158, 0.0897655521241369, 0.912638790567193,
      "Q1", 0.4905, 0.536477612940088, 1.20901281170151, 0.0897987051781905,
      0.91350488131814, "Q2", 0.55025, -0.62897411704692, 1.17294986673781,
      0.0855412778717539, 0.91775089209441, "Q3", 0.4795, -1.16772189518987,
      2.53947774599198, 0.0808348089790158, 0.953500035941236, "Q4",
      0.459
    )
  )
})

test_that("Model Summary table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.564530401768719, 0.4948125, 4, 1.04593444985526, 99)
  )
})

# Consistency test 8, file: binary2.csv, model: 4-PL ###########################

options <- initIRTOptions("itemResponseTheoryDichotomousBayesian")
options$burnin <- 1000
options$chains <- 4
options$covariates <- list()
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3", "Q4")
options$model <- "4PL"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemCharacteristicGroup <- TRUE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemInformationLabels <- FALSE
options$plotPriorPosterior <- TRUE
options$plotPriorPosteriorLabels <- FALSE
options$plotTestInformation <- TRUE
options$priorAlpha <- "priorAlphaLogNormal"
options$priorAlphaCauchyLocation <- 0
options$priorAlphaCauchyScale <- 1
options$priorAlphaHierarchicalSd <- 1
options$priorAlphaLogNormalMean <- 0
options$priorAlphaLogNormalSd <- 1
options$priorAlphaNormalMean <- 0
options$priorAlphaNormalSd <- 1
options$priorAlphaStudentDf <- 1
options$priorAlphaStudentLocation <- 0
options$priorBeta <- "priorBetaNormal"
options$priorBetaCauchyLocation <- 0
options$priorBetaCauchyScale <- 1
options$priorBetaHierarchicalSd <- 1
options$priorBetaNormalMean <- 0
options$priorBetaNormalSd <- 1
options$priorBetaStudentDf <- 1
options$priorBetaStudentLocation <- 0
options$priorDelta <- "priorDeltaNormal"
options$priorDeltaBetaAlpha <- 1
options$priorDeltaBetaBeta <- 1
options$priorDeltaCauchyLocation <- 0
options$priorDeltaCauchyScale <- 1
options$priorDeltaHierarchicalSd <- 0.1
options$priorDeltaNormalMean <- 1
options$priorDeltaNormalSd <- 0.1
options$priorDeltaStudentDf <- 1
options$priorDeltaStudentLocation <- 1
options$priorDeltaUniformMax <- 1
options$priorDeltaUniformMin <- 0.75
options$priorGamma <- "priorGammaNormal"
options$priorGammaBetaAlpha <- 1
options$priorGammaBetaBeta <- 1
options$priorGammaCauchyLocation <- 0
options$priorGammaCauchyScale <- 1
options$priorGammaHierarchicalSd <- 0.1
options$priorGammaNormalMean <- 0
options$priorGammaNormalSd <- 0.1
options$priorGammaStudentDf <- 1
options$priorGammaStudentLocation <- 0
options$priorGammaUniformMax <- 0.25
options$priorGammaUniformMin <- 0
options$priorScaling <- 1.702
options$priorTheta <- "priorThetaNormal"
options$priorThetaCauchyLocation <- 0
options$priorThetaCauchyScale <- 1
options$priorThetaHierarchicalSd <- 1
options$priorThetaNormalMean <- 0
options$priorThetaNormalSd <- 1
options$priorThetaStudentDf <- 1
options$priorThetaStudentLocation <- 0
options$priorThreshold <- "priorThresholdNormal"
options$priorThresholdCauchyLocation <- 0
options$priorThresholdCauchyScale <- 1
options$priorThresholdNormalMean <- 0
options$priorThresholdNormalSd <- 1
options$priorThresholdStudentDf <- 1
options$priorThresholdStudentLocation <- 1
options$priorThresholdUniformMax <- 0
options$priorThresholdUniformMin <- 1
options$priorZeta <- "priorZetaNormal"
options$priorZetaCauchyLocation <- 0
options$priorZetaCauchyScale <- 1
options$priorZetaHierarchicalSd <- 1
options$priorZetaNormalMean <- 0
options$priorZetaNormalSd <- 1
options$priorZetaStudentDf <- 1
options$priorZetaStudentLocation <- 0
options$samples <- 2000
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryDichotomousBayesian", "binary2.csv", options)

test_that("Item Information table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -1.23288654862242, 1.97913176710109, 0.0880626236380975, 0.935612110058775,
      "Q1", 0.456, -0.223505066878701, 1.52318126435554, 0.0863943744090167,
      0.922090057710658, "Q2", 0.4985, 0.358869120863817, 0.743231347947238,
      0.0849610192254547, 0.913399965970728, "Q3", 0.58175, 0.178898346612004,
      0.572819931583887, 0.0822698706272262, 0.904203497202466, "Q4",
      0.5855
    )
  )
})

test_that("Model Summary table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.623741644659081, 0.5304375, 4, 1.02752104393526, 50)
  )
})

# Consistency test 9, file: binary.csv, model: 2-PL Hierarchical ###############

options <- initIRTOptions("itemResponseTheoryDichotomousBayesian")
options$burnin <- 1000
options$chains <- 4
options$covariates <- list()
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3", "Q4")
options$model <- "2PL"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemCharacteristicGroup <- TRUE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4")
options$plotItemInformationLabels <- FALSE
options$plotPriorPosterior <- TRUE
options$plotPriorPosteriorLabels <- FALSE
options$plotTestInformation <- TRUE
options$priorAlpha <- "priorAlphaHierarchical"
options$priorAlphaCauchyLocation <- 0
options$priorAlphaCauchyScale <- 1
options$priorAlphaHierarchicalSd <- 0.1
options$priorAlphaLogNormalMean <- 0
options$priorAlphaLogNormalSd <- 1
options$priorAlphaNormalMean <- 0
options$priorAlphaNormalSd <- 1
options$priorAlphaStudentDf <- 1
options$priorAlphaStudentLocation <- 0
options$priorBeta <- "priorBetaHierarchical"
options$priorBetaCauchyLocation <- 0
options$priorBetaCauchyScale <- 1
options$priorBetaHierarchicalSd <- 1
options$priorBetaNormalMean <- 0
options$priorBetaNormalSd <- 1
options$priorBetaStudentDf <- 1
options$priorBetaStudentLocation <- 0
options$priorDelta <- "priorDeltaNormal"
options$priorDeltaBetaAlpha <- 1
options$priorDeltaBetaBeta <- 1
options$priorDeltaCauchyLocation <- 0
options$priorDeltaCauchyScale <- 1
options$priorDeltaHierarchicalSd <- 0.1
options$priorDeltaNormalMean <- 1
options$priorDeltaNormalSd <- 0.1
options$priorDeltaStudentDf <- 1
options$priorDeltaStudentLocation <- 1
options$priorDeltaUniformMax <- 1
options$priorDeltaUniformMin <- 0.75
options$priorGamma <- "priorGammaNormal"
options$priorGammaBetaAlpha <- 1
options$priorGammaBetaBeta <- 1
options$priorGammaCauchyLocation <- 0
options$priorGammaCauchyScale <- 1
options$priorGammaHierarchicalSd <- 0.1
options$priorGammaNormalMean <- 0
options$priorGammaNormalSd <- 0.1
options$priorGammaStudentDf <- 1
options$priorGammaStudentLocation <- 0
options$priorGammaUniformMax <- 0.25
options$priorGammaUniformMin <- 0
options$priorScaling <- 1.702
options$priorTheta <- "priorThetaNormal"
options$priorThetaCauchyLocation <- 0
options$priorThetaCauchyScale <- 1
options$priorThetaHierarchicalSd <- 1
options$priorThetaNormalMean <- 0
options$priorThetaNormalSd <- 1
options$priorThetaStudentDf <- 1
options$priorThetaStudentLocation <- 0
options$priorThreshold <- "priorThresholdNormal"
options$priorThresholdCauchyLocation <- 0
options$priorThresholdCauchyScale <- 1
options$priorThresholdNormalMean <- 0
options$priorThresholdNormalSd <- 1
options$priorThresholdStudentDf <- 1
options$priorThresholdStudentLocation <- 1
options$priorThresholdUniformMax <- 0
options$priorThresholdUniformMin <- 1
options$priorZeta <- "priorZetaNormal"
options$priorZetaCauchyLocation <- 0
options$priorZetaCauchyScale <- 1
options$priorZetaHierarchicalSd <- 1
options$priorZetaNormalMean <- 0
options$priorZetaNormalSd <- 1
options$priorZetaStudentDf <- 1
options$priorZetaStudentLocation <- 0
options$samples <- 2000
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryDichotomousBayesian", "binary.csv", options)

test_that("Item Information table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -0.661161800627897, 0.793688546642886, "Q1", 0.48675, 0.397379746350271,
      0.847894325403872, "Q2", 0.5535, -0.49093831613831, 0.881879033700168,
      "Q3", 0.4945, -1.13428924390419, 1.25697992802762, "Q4", 0.456
    )
  )
})

test_that("Model Summary table results match", {
  skip("Does not reproduce")
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.597381096071961, 0.4976875, 4, 1.0164938500573, 99)
  )
})

################################################################################
