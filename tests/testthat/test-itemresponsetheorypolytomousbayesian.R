context("[IRT] Bayesian Polytomous Item Response Theory")

# Test 1, file: nominal.csv, model: partial credit #############################

options <- initIRTOptions("itemResponseTheoryPolytomousBayesian")
options$burnin <- 200
options$chains <- 1
options$covariates <- list()
options$dichotomous <- FALSE
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3")
options$model <- "Rasch"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3")
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3")
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
options$priorThresholdNormalSd <- 5
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
options$samples <- 500
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomousBayesian", "nominal.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-1")
})

test_that("Item Q1 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q1-1")
})

test_that("Item Q2 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q2-1")
})

test_that("Item Q3 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q3-1")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-1")
})

test_that("Difficulty b plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_beta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "difficulty-b-1")
})

test_that("θ plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "theta-1")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-1")
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -0.169138434665008, -0.105764998003632, -0.0587571098684168, 0.0143423618971954,
      "Q1", 0.496666666666667, 0.103496382981523, 0.197122439167883,
      0.3033567983571, 0.375928578940355, "Q2", 0.476666666666667,
      0.150150966828549, 0.19346959472266, 0.227436344915372, 0.269108534788975,
      "Q3", 0.533333333333333
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.996403147632882, 0.502222222222222, 3, 1.03132074600664, 200)
  )
})

# Test 2, file: nominal.csv, model: generalized partial credit #################

options <- initIRTOptions("itemResponseTheoryPolytomousBayesian")
options$burnin <- 200
options$chains <- 1
options$covariates <- list()
options$dichotomous <- FALSE
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3")
options$model <- "gpcm"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3")
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3")
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
options$priorThresholdNormalSd <- 5
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
options$samples <- 500
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomousBayesian", "nominal.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-2")
})

test_that("Item Q1 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q1-2")
})

test_that("Item Q2 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q2-2")
})

test_that("Item Q3 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q3-2")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-2")
})

test_that("Discrimination a plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_alpha"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "discrimination-a-2")
})

test_that("Difficulty b plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_beta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "difficulty-b-2")
})

test_that("θ plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "theta-2")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-2")
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -0.21063786071615, -0.140614667230526, -0.0908327213353095, -0.0212182582466679,
      0.375187610234185, "Q1", 0.526666666666667, 0.206593204670942,
      0.303308177602597, 0.396950478916537, 0.4950439953875, 0.284488523221339,
      "Q2", 0.526666666666667, 0.0140942816270336, 0.0966219319145534,
      0.220909615875163, 0.270347600689917, 2.59570132618807, "Q3",
      0.53
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.300246723362976, 0.527777777777778, 3, 1.02162113466554, 200)
  )
})

# Test 3, file: nominal.csv, model: rating scale ###############################

options <- initIRTOptions("itemResponseTheoryPolytomousBayesian")
options$burnin <- 200
options$chains <- 1
options$covariates <- list()
options$dichotomous <- FALSE
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3")
options$model <- "rsm"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3")
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3")
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
options$priorThresholdNormalSd <- 5
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
options$samples <- 500
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomousBayesian", "nominal.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-3")
})

test_that("Item Q1 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q1-3")
})

test_that("Item Q2 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q2-3")
})

test_that("Item Q3 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q3-3")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-3")
})

test_that("Difficulty b plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_beta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "difficulty-b-3")
})

test_that("θ plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "theta-3")
})

test_that("Threshold t plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_threshold"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "threshold-t-3")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-3")
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -0.0780405339649777, 0.852099461718604, -0.196919100429061, 0.395474182376784,
      -0.962802582479314, "Q1", 0.54, 0.173318196445345, 0.852099461718604,
      -0.196919100429061, 0.395474182376784, -0.962802582479314, "Q2",
      0.533333333333333, 0.154899296757696, 0.852099461718604, -0.196919100429061,
      0.395474182376784, -0.962802582479314, "Q3", 0.553333333333333
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(1.01521573912304, 0.542222222222222, 3, 1.01983573101391, 200)
  )
})

# Test 4, file: nominal.csv, model: generalized rating scale ###################

options <- initIRTOptions("itemResponseTheoryPolytomousBayesian")
options$burnin <- 200
options$chains <- 1
options$covariates <- list()
options$dichotomous <- FALSE
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3")
options$model <- "grsm"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3")
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3")
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
options$priorThresholdNormalSd <- 5
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
options$samples <- 500
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomousBayesian", "nominal.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-4")
})

test_that("Item Q1 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q1-4")
})

test_that("Item Q2 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q2-4")
})

test_that("Item Q3 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q3-4")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-4")
})

test_that("Discrimination a plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_alpha"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "discrimination-a-4")
})

test_that("Difficulty b plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_beta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "difficulty-b-4")
})

test_that("θ plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "theta-4")
})

test_that("Threshold t plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_threshold"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "threshold-t-4")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-4")
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -0.2358099304782, 0.299386162290692, 2.16167400851278, -0.183415658001808,
      0.740138470808876, -2.23810175640927, "Q1", 0.523333333333333,
      0.175935062518607, 0.266581103966937, 2.16167400851278, -0.183415658001808,
      0.740138470808876, -2.23810175640927, "Q2", 0.493333333333333,
      0.0896837874460927, 0.380769415064171, 2.16167400851278, -0.183415658001808,
      0.740138470808876, -2.23810175640927, "Q3", 0.53
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.609698915934384, 0.515555555555556, 3, 1.01879962787638, 200)
  )
})

# Test 5, file: nominal.csv, model: graded response ############################

options <- initIRTOptions("itemResponseTheoryPolytomousBayesian")
options$burnin <- 200
options$chains <- 1
options$covariates <- list()
options$dichotomous <- FALSE
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3")
options$model <- "graded"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3")
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3")
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
options$priorThresholdNormalSd <- 5
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
options$samples <- 500
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomousBayesian", "nominal.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-5")
})

test_that("Item Q1 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q1-5")
})

test_that("Item Q2 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q2-5")
})

test_that("Item Q3 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q3-5")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-5")
})

test_that("Discrimination a plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_alpha"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "discrimination-a-5")
})

test_that("Difficulty b plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_beta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "difficulty-b-5")
})

test_that("θ plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "theta-5")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-5")
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -1.27968514224254, -0.449182151863489, -0.0168228594821843, 0.996204968788614,
      1.4113084102282, "Q1", 0.476666666666667, -0.703022527922186,
      -0.0105018184451355, 0.962318929352175, 1.37370931896206, 0.932644703597696,
      "Q2", 0.536666666666667, -0.325921019651672, 0.0679433924976986,
      0.542693756732675, 0.781661549561003, 0.688452590455619, "Q3",
      0.59
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.464998661874839, 0.534444444444444, 3, 1.01630533362009, 200)
  )
})

# Test 6, file: nominal.csv, model: nominal response ###########################

options <- initIRTOptions("itemResponseTheoryPolytomousBayesian")
options$burnin <- 200
options$chains <- 1
options$covariates <- list()
options$dichotomous <- FALSE
options$explanatoryText <- FALSE
options$items <- c("Q1", "Q2", "Q3")
options$model <- "nominal"
options$nutsAdaptDelta <- 0.8
options$nutsMaxTreedepth <- 10
options$plotHistogramAbility <- TRUE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3")
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3")
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
options$priorThresholdNormalSd <- 5
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
options$samples <- 500
options$seed <- 1
options$tableFitStatistics <- FALSE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomousBayesian", "nominal.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-6")
})

test_that("Item Q1 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q1-6")
})

test_that("Item Q2 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q2-6")
})

test_that("Item Q3 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q3-6")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-6")
})

test_that("Discrimination a plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_alpha"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "discrimination-a-6")
})

test_that("Difficulty b plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_beta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "difficulty-b-6")
})

test_that("θ plot matches", {
  plotName <- results[["results"]][["plotPriorPosterior"]][["collection"]][["plotPriorPosterior_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "theta-6")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-6")
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      0.408378965714784, -0.267216894228265, -0.775887795642741, 0.0585264173630792,
      0.576199306793142, 0.977439070129831, 0.44518978806431, 0.0193308592949252,
      -0.402441275925403, -1.03951844156366, "Q1", 0.5, 0.637546716299224,
      -0.129006596388576, 0.118719981252037, -0.749382727396987, 0.122122626234303,
      1.0056940096197, 0.284504998501875, -0.417747631585226, 0.0582756209758225,
      -0.930726997512167, "Q2", 0.58, 1.19468105982758, -0.533092701049714,
      -0.315227621624483, -1.09647880159323, 0.750118064439841, 0.339443213475034,
      0.236559054310655, -0.185381452102118, 0.23259370132073, -0.623214517004303,
      "Q3", 0.523333333333333
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.685643825770559, 0.534444444444444, 3, 1.03820301328212, 200)
  )
})

################################################################################
