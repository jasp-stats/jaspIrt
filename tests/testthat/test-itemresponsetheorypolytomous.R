context("[IRT] Polytomous Item Response Theory")

options("jaspRoundToPrecision" = function(x) signif(round(x, digits = 2), digits = 2))

# Consistency test 1, file: nominal.csv, model: partial credit #################

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristicItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "Rasch"
options$items <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal.csv", options)

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

test_that("Item Q4 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot4"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q4-1")
})

test_that("Item Q5 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot5"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q5-1")
})

test_that("Item Q6 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot6"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q6-1")
})

test_that("Item Q7 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot7"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q7-1")
})

test_that("Item Q8 plot matches", {
  plotName <- results[["results"]][["plotItemCharacteristic"]][["collection"]][["plotItemCharacteristic_plot8"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-q8-1")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-1")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-1")
})

test_that("Additional Fit Statistics table results match", {
  table <- results[["results"]][["tableFitStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(1, 0, 0, 0.0682246642041449, 1.17719578893775, 0.128647593916561)
  )
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      0.849068993456361, 0.539850840274914, -0.812647802627737, -0.732512972705405,
      39, "Q1", 0.628303873275708, 0, 35.5440025352012, 0.966229742872704,
      -0.390666161889348, 1.17768131995582, -1.16539046577034, 40,
      "Q2", 0.159213277818104, 0.0333378528627466, 48.8468429706452,
      1.55927525056928, -0.213209964268743, 0.965028587420002, -1.81951690516011,
      31, "Q3", 0.00932259415127331, 0.0590109301962661, 52.4822462859357,
      0.521160189295474, -0.0305637570206821, -0.743944259238296,
      -1.35814515812708, 34, "Q4", 0.0787439848784764, 0.042507551783958,
      46.2254309923333, 0.175378806802617, 0.481961926116838, -0.121173339130843,
      -1.24032417795439, 44, "Q5", 0.577431444813035, 0, 41.5443823257552,
      1.59416683992828, 0.570293547022449, -0.66667655215843, -0.636247969530641,
      35, "Q6", 0.709185195948734, 0, 29.9767840297243, 1.11465330756202,
      -0.572309034029589, -0.558167963249437, -2.16334547093361, 20,
      "Q7", 0.201524690190379, 0.0354353903611146, 24.9975542223774,
      1.41600344110227, -0.482722329476794, 0.887179001543029, -1.3193034306071,
      35, "Q8", 0.199872889461605, 0.0312051356930233, 41.7822418380633
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      3878.27258722517, 3987.11706032125, 3, 8, 2.69182814125708, 200,
      0.4416178029421
    )
  )
})

# Consistency test 2, file: nominal2.csv, model: partial credit ################

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristic <- FALSE
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "Rasch"
options$items <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal2.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-2")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-2")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-2")
})

test_that("Additional Fit Statistics table results match", {
  table <- results[["results"]][["tableFitStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0, 0.165606555386376, 0.161385519828726, 0.326719182295371)
  )
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      1.3299952971843, -0.649044940453284, -0.0908920471775763, -2.2449890912138,
      4, "Q1", 0.171689605742031, 0.110484135090091, 6.39252184489467,
      -0.934107933836589, -0.868609928664192, 0.291268820306333, -1.42116190866103,
      5, "Q2", 0.452517308938026, 0, 4.70821271906384, 1.28683434417964,
      0.450851305119536, -1.07105509568683, "", 6, "Q3", 0.362856318372457,
      0.0438750979644093, 6.56595712108763, 0.194465181653523, 0.547836126188217,
      -0.203442427356302, -1.17956074622293, 8, "Q4", 0.533578925650797,
      0, 7.02833238284333, 0.906830461649105, 0.310580230819719, -0.313148970094642,
      0.235714318786848, 6, "Q5", 0.147697868748261, 0.108997633665819,
      9.49286233855594, -0.075399664686752, -1.26236583523358, 0.0368368198594375,
      -2.19000203703121, 3, "Q6", 0.00862037451457833, 0.242797830979622,
      11.6657656490762, 0.94215372640049, 0.118178833010969, -0.98884361594723,
      -0.787122888523111, 7, "Q7", 0.0248955058831886, 0.162203079472567,
      16.0242747737016, 2.65646483442632, -0.637471139694743, 0.930094101838335,
      -1.18644006170065, 1, "Q8", 0.0117585447417359, 0.330334319218101,
      6.34691736021104
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      992.586720032602, 1053.7714562063, 4, 8, 9.3754041126404, 50,
      0.0523713419671541
    )
  )
})

# Consistency test 3, file: nominal.csv, model: generalized partial credit #####

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristic <- FALSE
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "gpcm"
options$items <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-3")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-3")
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
      1.30623343084736, 0.839989506255898, -1.30365162113174, -1.1276132169039,
      0.620037368120191, 39, "Q1", 0.637370066156149, 0, 35.3442896665758,
      1.33849071991704, -0.595411824725092, 1.80994310538207, -1.58739268267051,
      0.673781617160227, 41, "Q2", 0.310354979435152, 0.0219700154495243,
      44.9381990018562, 4.34803928148173, -0.538170232025279, 2.34070725340027,
      -5.15514449374721, 0.381802194504373, 35, "Q3", 0.0950510101261514,
      0.0403641879858394, 46.3478493337818, 0.135122815267663, -0.297932721669162,
      -0.901355402351374, -1.34999893288912, 0.920703766278161, 30,
      "Q4", 0.060704395473106, 0.0463498266191492, 42.8253893729224,
      0.64200929240246, 1.17859628581214, -0.31377883685306, -2.96370140715588,
      0.444038263534937, 47, "Q5", 0.895211281660177, 0, 35.2956036628787,
      3.7472814387792, 1.28038189537392, -1.6427953073453, -1.69004161604395,
      0.441594516140556, 37, "Q6", 0.755638405612476, 0, 30.7572179655985,
      1.23493618617523, -0.973625539162062, -0.856379253413943, -2.9066128418796,
      0.735688857831467, 21, "Q7", 0.380885287744743, 0.0178191268979483,
      22.3269214433503, 2.09973056998651, -0.75921556725807, 1.4061987324922,
      -1.92158383949418, 0.650633111573485, 33, "Q8", 0.30994015262474,
      0.0230307535450805, 36.4832393033458
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(3874.32502799301, 4006.25772265493, "", 8, "", 200, "")
  )
})

# Consistency test 4, file: nominal2.csv, model: generalized partial credit ####

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristic <- FALSE
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "gpcm"
options$items <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal2.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-4")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-4")
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
      4.45283953009363, -1.5726733324767, -0.111378733339547, -6.76982162822028,
      0.336770707343693, 4, "Q1", 0.25902829300615, 0.081055670392262,
      5.28772425373685, -1.73518570326305, -1.45535557351582, 0.231082348528455,
      -1.7105517831817, 0.784318460721768, 4, "Q2", 0.114069255618093,
      0.13261496520504, 7.44699888328146, 0.549063112072816, 0.440139188677979,
      -0.124361394076789, "", 1.57894039785115, 4, "Q3", 0.555512616100537,
      0, 3.01383623002032, 0.690388375333628, 1.48479989790136, -0.49487962647742,
      -3.06839161912343, 0.402820116964611, 7, "Q4", 0.490200020133644,
      0, 6.43294819389414, 2.30788674003225, 0.780492887206431, -0.840707427861814,
      0.337904034891663, 0.415792064200176, 6, "Q5", 0.121100116522764,
      0.117881402658256, 10.0854313772473, 1.89454086475132, -5.61483403701992,
      1.14801838672805, -12.7176207262014, 0.172519556426908, 3, "Q6",
      0.00309959771511383, 0.27182534063046, 13.8616853239033, 1.95890040848929,
      0.278648006380051, -2.00222415907593, -1.6215824315965, 0.487480118665219,
      7, "Q7", 0.0312282275862363, 0.156469104654374, 15.3975251839901,
      3.47152686074028, -0.75355136119854, 1.48696487127954, -1.20377113804901,
      0.74781100047957, 0, "Q8", "NaN", "NaN", "NaN"
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(994.155925774437, 1068.72482298613, "", 8, "", 50, "")
  )
})

# Consistency test 5, file: nominal.csv, model: rating scale ###################

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristic <- FALSE
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "rsm"
options$items <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-5")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-5")
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
      0, 0.991730774335657, -0.0413493254272275, 0.0751015907747766,
      -1.21960281009895, 39, "Q1", 0.340546835385308, 0.0197980271109215,
      42.0420161311594, -0.197886187246428, 0.991730774335657, -0.0413493254272275,
      0.0751015907747766, -1.21960281009895, 39, "Q2", 0.000734080336579036,
      0.0664369447033168, 73.2560266105513, -0.181932223268382, 0.991730774335657,
      -0.0413493254272275, 0.0751015907747766, -1.21960281009895,
      39, "Q3", 0.00848630331604499, 0.0557990033464508, 63.1640968185626,
      0.355028282410221, 0.991730774335657, -0.0413493254272275, 0.0751015907747766,
      -1.21960281009895, 33, "Q4", 0.0523572096043417, 0.0464494434089598,
      47.1686360576452, 0.0933127937741791, 0.991730774335657, -0.0413493254272275,
      0.0751015907747766, -1.21960281009895, 37, "Q5", 0.125440697211778,
      0.0368629900837985, 47.0054337191919, -0.251494154394732, 0.991730774335657,
      -0.0413493254272275, 0.0751015907747766, -1.21960281009895,
      40, "Q6", 0.845316413645463, 0, 31.0060727152785, 0.538475337420748,
      0.991730774335657, -0.0413493254272275, 0.0751015907747766,
      -1.21960281009895, 24, "Q7", 0.135404847265048, 0.0400726850352774,
      31.6693967304329, -0.173972906653492, 0.991730774335657, -0.0413493254272275,
      0.0751015907747766, -1.21960281009895, 39, "Q8", 0.147007749544474,
      0.0345376480441448, 48.2577021167211
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      3898.03540879597, 3937.61521719455, 24, 8, 23.1401675144342, 200,
      0.511536565285798
    )
  )
})

# Consistency test 6, file: nominal2.csv, model: rating scale ##################

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristic <- FALSE
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "rsm"
options$items <- c("Q1", "Q2", "Q4", "Q5", "Q6", "Q7", "Q8")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal2.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-6")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-6")
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
      0, 0.544181726171072, -0.518719624528768, -0.296694120701223,
      -1.4390195600607, 6, "Q1", 0.459604525752568, 0, 5.68313338707758,
      0.0938538490994695, 0.544181726171072, -0.518719624528768, -0.296694120701223,
      -1.4390195600607, 4, "Q2", 0.00839104742138612, 0.222229053635722,
      13.6796074468464, -0.308739940497796, 0.544181726171072, -0.518719624528768,
      -0.296694120701223, -1.4390195600607, 4, "Q4", 0.295125423592309,
      0.0686816870381971, 4.92456613034492, -0.643930708905295, 0.544181726171072,
      -0.518719624528768, -0.296694120701223, -1.4390195600607, 4,
      "Q5", 0.457284476355495, 0, 3.63747867268835, 0.365949250112994,
      0.544181726171072, -0.518719624528768, -0.296694120701223, -1.4390195600607,
      2, "Q6", 0.0206948659402375, 0.24234692756393, 7.75573926336831,
      -0.240118192230449, 0.544181726171072, -0.518719624528768, -0.296694120701223,
      -1.4390195600607, 4, "Q7", 0.0609326276093044, 0.159825863222287,
      9.00668408473085, -0.877945201682839, 0.544181726171072, -0.518719624528768,
      -0.296694120701223, -1.4390195600607, 4, "Q8", 0.087133777626711,
      0.145057699138998, 8.12418027158201
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(882.33364364412, 903.36589670383, "", 7, "", 50, "")
  )
})

# Consistency test 7, file: nominal.csv, model: generalized rating scale #######

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristic <- FALSE
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "grsm"
options$items <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-7")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-7")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-7")
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      0, 1.71888685567201, 0.606552195343404, 0.233582935887079, -0.0973570072536191,
      -0.436964734920229, 39, "Q1", 0.46770644934487, 0.0024950425032221,
      39.0483140650779, -0.329089857502384, 1.93572502735238, 0.606552195343404,
      0.233582935887079, -0.0973570072536191, -0.436964734920229,
      42, "Q2", 0.0153200300370402, 0.051527812511171, 64.1914550329539,
      -0.343756341603841, 1.08970214464898, 0.606552195343404, 0.233582935887079,
      -0.0973570072536191, -0.436964734920229, 39, "Q3", 0.141141152407108,
      0.0350260494257933, 48.5213821379525, 0.546806401971019, 2.19107281636381,
      0.606552195343404, 0.233582935887079, -0.0973570072536191, -0.436964734920229,
      33, "Q4", 0.0593383777198947, 0.0453952646734896, 46.5328142697151,
      0.205259916717452, 1.54143166649991, 0.606552195343404, 0.233582935887079,
      -0.0973570072536191, -0.436964734920229, 38, "Q5", 0.179295647442083,
      0.0321731232301219, 45.8275007490742, -0.515416716384533, 1.25290984246252,
      0.606552195343404, 0.233582935887079, -0.0973570072536191, -0.436964734920229,
      38, "Q6", 0.890155831074728, 0, 27.723512704777, 1.0025823003934,
      1.44986712476615, 0.606552195343404, 0.233582935887079, -0.0973570072536191,
      -0.436964734920229, 24, "Q7", 0.0795917236294261, 0.0464240100028973,
      34.2931812538817, -0.273842302511463, 1.67257950921752, 0.606552195343404,
      0.233582935887079, -0.0973570072536191, -0.436964734920229,
      42, "Q8", 0.0660027250031405, 0.0417416536031233, 56.5626920652824
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      3887.57242244989, 3950.2404524143, 17, 8, 11.2470758540595, 200,
      0.843435209103875
    )
  )
})

# Consistency test 8, file: nominal2.csv, model: generalized rating scale ######

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristic <- FALSE
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "grsm"
options$items <- c("Q1", "Q2", "Q4", "Q5", "Q6", "Q7", "Q8")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal2.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-8")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-8")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-8")
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      0, 0.63203652383622, 2.38888352942402, 1.86050297492833, 1.34449960220523,
      0.808397631409082, 5, "Q1", 0.376464160340315, 0.0369358363826883,
      5.33424272227574, -0.628696502409946, 2.00752130835638, 2.38888352942402,
      1.86050297492833, 1.34449960220523, 0.808397631409082, 3, "Q2",
      0.00993493154812464, 0.238461493961421, 11.35899096304, -1.31600280699783,
      1.2471991570015, 2.38888352942402, 1.86050297492833, 1.34449960220523,
      0.808397631409082, 6, "Q4", 0.208077874915887, 0.0909661871485367,
      8.43280507807673, -2.08803233761718, 1.40317913375685, 2.38888352942402,
      1.86050297492833, 1.34449960220523, 0.808397631409082, 4, "Q5",
      0.236790326207277, 0.0884566223215588, 5.53361651037762, 0.491559183496779,
      0.970362075579357, 2.38888352942402, 1.86050297492833, 1.34449960220523,
      0.808397631409082, 1, "Q6", 0.00342499274248607, 0.392947976117339,
      8.56599748480094, -1.1948345334455, 1.09462423699638, 2.38888352942402,
      1.86050297492833, 1.34449960220523, 0.808397631409082, 5, "Q7",
      0.0840699566928726, 0.138564134956785, 9.7040047765989, -2.94898232738387,
      0.87738769311918, 2.38888352942402, 1.86050297492833, 1.34449960220523,
      0.808397631409082, 3, "Q8", 0.0305736045817567, 0.200433764016219,
      8.90553298238327
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(877.649858025795, 910.154249118073, "", 7, "", 50, "")
  )
})

# Consistency test 9, file: nominal.csv, model: graded responses ###############

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristic <- FALSE
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "graded"
options$items <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-9")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-9")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-9")
})

test_that("Additional Fit Statistics table results match", {
  table <- results[["results"]][["tableFitStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(1, 0, 0, 0.0491521864477676, 1.06397252757129, 0.0197161617604593)
  )
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -0.604194152433498, -0.206870417092879, -0.00659466037794323,
      0.479776462769636, 1.78332961104991, 41, "Q1", 0.0862058832173727,
      0.03967590423507, 53.8437132178859, -0.419859426738845, 0.028220053699278,
      0.646937133334409, 0.89004561577311, 1.5719184497708, 42, "Q2",
      0.587073013076099, 0, 39.3695348556369, -0.212124978531319,
      0.117920044937326, 0.515091217128641, 0.693103127624604, 1.22934360357753,
      44, "Q3", 0.0885790318072364, 0.0387150891961739, 57.1240017991312,
      -1.10446922903179, -0.772538945492212, -0.535375088690931, -0.116347867298764,
      2.13465274301834, 33, "Q4", 0.0407502518243535, 0.0484415336605115,
      48.4100051982727, -1.13586897828473, -0.406797616364261, -0.0572822653282252,
      0.348893023750336, 1.23371896033558, 44, "Q5", 0.350705880756232,
      0.0185057285831768, 46.9985971878917, -0.0541832320383978, 0.269471704863595,
      0.45064466531072, 0.917857805074451, 1.30560396351666, 42, "Q6",
      0.283603518437596, 0.0238414655024905, 46.7508165593273, -1.27994138883517,
      -1.08823971953066, -0.867648352454267, -0.57294846969287, 1.83058721764461,
      27, "Q7", 0.187981019011489, 0.0341882548996481, 33.2801599817766,
      -0.295617247172711, -0.0120879898146029, 0.443532275401945,
      0.679590004939879, 1.74041646047131, 37, "Q8", 0.721558376995068,
      0, 31.5621731085339, -1.78035114862107, -1.12768746319218, -0.582756569416686,
      -0.175022574579923, 1.5485898634948, 42, "Q9", 0.158183079336872,
      0.0330209951460573, 51.1134471946036, -0.751059869760865, -0.408453182560909,
      -0.192847846579909, 0.0803075692103524, 2.06074627663503, 32,
      "Q10", 0.814317894191153, 0, 24.7931694261993, -1.24415496145272,
      -0.328231976643869, -0.0392721910746752, 0.190281316913499,
      1.25552606697206, 46, "Q11", 0.375711691790493, 0.0162418288649188,
      48.4147977826469, -0.29013827164651, -0.108695478623344, 0.279933199114749,
      0.496410538188101, 1.61570018728816, 38, "Q12", 0.589459743020232,
      0, 35.4180117205378, -1.64720699909772, -1.12223302797006, -0.618034315899938,
      -0.280964541105649, 1.25779005516457, 42, "Q13", 0.194642657354152,
      0.0302607300624613, 49.6535190899461, 0.312852427318902, 0.954242537160105,
      1.27661105739418, 1.82988308445489, 1.37848745943372, 36, "Q14",
      0.46515253308288, 0.00323046968777318, 36.0747630340675, -0.169062689663796,
      0.329004064148367, 0.876212099581574, 1.49755139008155, 1.35087302471048,
      46, "Q15", 0.929998205498531, 0, 32.710955912126, -0.588329138255993,
      -0.254941622073253, 0.140946661834805, 0.635022422538323, 2.06085877291967,
      39, "Q16", 0.243719255509014, 0.0271763761677547, 44.7319286271253,
      -1.01450535339653, -0.328809283236725, -0.0928205808019229,
      0.643504752868954, 1.10670489439861, 45, "Q17", 0.764685127534659,
      0, 37.8921468079053, -0.859262316033215, -0.210440027147857,
      0.11394671869581, 0.432500000871982, 1.36828131086197, 43, "Q18",
      0.449969451596791, 0.00765346547696299, 43.501230842787, -0.587495259471273,
      -0.0405834056478183, 0.805153838456572, 1.51851155211839, 1.16567496919758,
      51, "Q19", 0.926188644672622, 0, 37.1840707362458, -1.01773832967662,
      -0.546372414609224, 0.486227708340843, 1.70358587049208, 0.73330366847883,
      66, "Q20", 0.533647653299801, 0, 64.3748447596458
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      9795.71755357345, 10125.5492902283, 110, 20, 89.3713703978293,
      200, 0.925485276097458
    )
  )
})

# Consistency test 10, file: nominal2.csv, model: graded responses #############

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristic <- FALSE
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "graded"
options$items <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal2.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-10")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-10")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-10")
})

test_that("Additional Fit Statistics table results match", {
  table <- results[["results"]][["tableFitStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(1, 0, 0, 0.094090666839141, 1.19383746398112, 0.0585526328540769)
  )
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -1.02014043286887, -0.844585580626306, -0.62977679946912, -0.433126793328509,
      1.867631330654, 3, "Q1", 0.0974004607074327, 0.150090635898667,
      6.31149825071653, -2.42488035931627, -1.56165227440596, -0.650453855675374,
      -0.17111153372955, 1.3978120960816, 6, "Q2", 0.0064765352243977,
      0.201221092378138, 17.9040388372482, -0.0454644417674239, 0.307897008247326,
      0.542585124406764, "", 3.22966172163423, 3, "Q3", 0.520283570268996,
      0, 2.25970865894379, -1.32216524169913, -0.393963612972006,
      0.0227368191890033, 0.54107513227405, 0.907882149087318, 7,
      "Q4", 0.194883563370222, 0.0917940134350456, 9.89016632956207,
      -0.430339469859723, 0.240969580266229, 0.770981896805283, 1.81412433448413,
      1.0151040866788, 8, "Q5", 0.179944629926035, 0.0931589019443237,
      11.4020037564971, -3.81428298656159, -3.08984654926687, -2.00575797855489,
      -1.39585145520506, 0.788595523120846, 4, "Q6", 0.314923968901185,
      0.0614849301323875, 4.74095774014338, -0.9051913380521, -0.557398216371,
      -0.320178817509998, 0.296134401122401, 1.410021297845, 6, "Q7",
      0.396032125566463, 0.0290208478383131, 6.24760962512083, 0.3423192203644,
      0.472321890982579, 0.812697486313814, 1.01378899371166, 2.4940986732433,
      1, "Q8", 0.00515893412854251, 0.373151644927965, 7.82286535550986,
      -0.951382551537203, -0.708732926098167, -0.0145713098099168,
      0.395076097632594, 1.43871303819282, 7, "Q9", 0.0265678888446069,
      0.160586599555993, 15.8453031932362, -0.653739229183343, -0.110008251436888,
      0.0200246378751651, 0.657425136272515, 2.40697479297027, 6,
      "Q10", 0.89757053930483, 0, 2.2281008175601, -0.306386552699634,
      -0.19196218876693, 0.0484950485421717, 0.365203676417538, 2.83653842303621,
      3, "Q11", 0.00384543293020708, 0.265995210791088, 13.4007574680779,
      -0.528260822278295, 0.0685606648541623, 0.417129480219521, 1.65147509868212,
      0.830558961503448, 8, "Q12", 0.0726380125043594, 0.127466439725689,
      14.3690957564863, -1.43959669632618, -0.897843198572921, -0.665783852078827,
      -0.386445179366049, 2.00951806612455, 3, "Q13", 0.0742543019738328,
      0.163449475584586, 6.92721246712479, -0.212486974835371, 0.599266451056193,
      0.846039706298742, 1.14121733581873, 1.51849020083658, 6, "Q14",
      0.0217458895507716, 0.173155455864128, 14.8149466972811, -2.09135247514148,
      -1.85324511181026, -1.55024542098841, -1.13495720042838, 3.29788029436231,
      0, "Q15", "NaN", "NaN", "NaN", -2.19802525207288, -1.57095066936144,
      -0.655735649556159, -0.254586689449, 1.4499152933458, 5, "Q16",
      0.502409780626533, 0, 4.33389903030795, -0.0229550539138029,
      0.909581301287255, 1.31641069426675, 1.60609966186515, 1.4918318548934,
      6, "Q17", 0.0834079255704107, 0.132544827684982, 11.1650306157362,
      0.199026942722224, 0.550655363412559, 1.0620482187311, 1.60293958772492,
      1.58222344440302, 5, "Q18", 0.045254896963029, 0.160709793544832,
      11.3277712465995, -1.43655367207073, -0.498587985065642, -0.116062257671514,
      0.345859085214395, 1.42139851069723, 7, "Q19", 0.0237388124066622,
      0.163372208890307, 16.1548341727305, -0.851614409722666, 0.0632427751416569,
      0.414712710854996, 0.781959616785608, 0.807659568526133, 7,
      "Q20", 0.0307576644610146, 0.156862201062278, 15.4397722918812
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      2431.64502779718, 2620.93530533457, 111, 20, 98.3735846576397,
      50, 0.79861275704792
    )
  )
})

# Consistency test 11, file: nominal.csv, model: nominal responses #############

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristic <- FALSE
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "nominal"
options$items <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-11")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-11")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-11")
})

test_that("Additional Fit Statistics table results match", {
  table <- results[["results"]][["tableFitStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(1, 0, 0, 1.04240531157126, 0.040942205747536)
  )
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      0.426449069854773, -0.244909657321304, -0.819112799402361, 0.0422382497278452,
      0.595335137141046, -1.40345716970094, -0.493498746380614, 0.100088107248043,
      0.409376729834077, 1.38749107899943, 37, "Q1", 0.0930287041912683,
      0.04000506432704, 48.7837832800415, 0.679544647354901, -0.164396887214901,
      0.0892868903048564, -0.813902140571481, 0.209467490126625, -1.26016388726838,
      -0.323850785710379, 0.458237255860727, -0.0145972062400028,
      1.14037462335804, 41, "Q2", 0.510114615981432, 0, 40.1082956471728,
      1.17987953227237, -0.500964882747578, -0.442918777779583, -1.05166133773522,
      0.815665465990019, -0.889395104660926, -0.639056899695493, 0.665851663885782,
      0.0501237676353542, 0.812476572835283, 38, "Q3", 0.104382117633401,
      0.0385835770728404, 49.2574920780422, -0.520494812499742, -0.614357360242998,
      -0.632749546414539, 0.269561607324331, 1.49804011183295, -1.61114789200779,
      -0.579609849151735, -0.197867042463772, 0.568947642064175, 1.81967714155912,
      26, "Q4", 0.0247261856148892, 0.055556473565955, 41.969663560808,
      0.230885283546778, -0.0852611293212395, -0.600766300458193,
      -0.399347721760707, 0.854489867993361, -0.898868742725239, -0.52825752660051,
      0.407201773521575, 0.0840880444602042, 0.935836451343969, 45,
      "Q5", 0.470365003977261, 0.00206161700777375, 45.0380611202698,
      1.20614616642537, -0.456949627860716, -1.00643248026375, -0.130530782213301,
      0.387766723912398, -1.00534115371861, 0.162073535725526, 0.0510958716190693,
      -0.1781586103692, 0.970330356743216, 37, "Q6", 0.18708592510892,
      0.0317756520523434, 44.4343626624578, -0.72526832641828, -0.828093245545334,
      -0.692046027474104, 0.0436147465245582, 2.20179285291316, -1.91457943935608,
      -0.102134239008314, -0.333504406087344, 0.980067083482879, 1.37015100096886,
      20, "Q7", 0.629253752724491, 0, 17.3635884671438, 0.917513489347642,
      -0.484789508916458, 0.0176922173167036, -0.706919725101009,
      0.256503527353121, -1.17711391420483, -0.626360921827009, 0.171673879831967,
      0.0979844489178756, 1.533816507282, 36, "Q8", 0.447921148132119,
      0.00788869907741395, 36.4458269899319, -0.575506779795549, -0.508587408782637,
      -0.157225141314712, -0.129986651706775, 1.37130598159967, -0.884833070050887,
      -0.656171694113101, -0.253417405238963, 0.354317888982532, 1.44010428042042,
      39, "Q9", 0.0750688019408112, 0.041447554906164, 52.3326204075557,
      0.180174063516087, -0.472906808331679, -0.566543963258421, -0.255636828452238,
      1.11491353652625, -1.61808084281414, -0.915433650602377, 0.773265976744874,
      0.0474103176617415, 1.7128381990099, 32, "Q10", 0.76917244754375,
      0, 25.8735698023073, 0.210812879634805, 0.28677923230213, -0.636663566576358,
      -1.03897554688411, 1.17804700152353, -1.09677995918022, -0.606069631109354,
      0.165840405169211, 0.845716955273587, 0.691292229846775, 46,
      "Q11", 0.310852771555105, 0.0213979024391037, 50.1913440743748,
      1.00765605522743, -0.832471747873105, -0.0983695725771738, -0.767546103013514,
      0.690731368236364, -1.06056746549479, -0.108804297392719, -0.244649871507022,
      0.23957784246063, 1.1744437919339, 38, "Q12", 0.487952164215186,
      0, 37.5966703515128, -0.0971040805995449, -0.382287154326939,
      -0.368758680653706, -0.542477774206603, 1.39062768978679, -0.666589106935783,
      -0.0550681998341042, -0.35890285569078, 0.0121933985886666,
      1.068366763872, 37, "Q13", 0.135814403157538, 0.0359459566321613,
      46.5138191701556, 1.56635769154304, 0.256707403751008, -0.733814839378876,
      -0.296904153212399, -0.792346102702774, -1.19624319413255, -0.0762690282740961,
      0.262774724697002, -0.119609250664236, 1.12934674837388, 35,
      "Q14", 0.523152404150284, 0, 33.857471352801, 0.970520589497139,
      -0.112748141588606, -0.137076035291817, -0.554990130247096,
      -0.165706282369621, -1.29339937499322, -0.318005949539153, 0.0457485524869987,
      0.772010903413039, 0.793645868632336, 40, "Q15", 0.918516462565625,
      0, 28.2386359773782, 0.340655708898328, -0.42589111088158, -0.0968792371599074,
      -0.0256720095804979, 0.207786648723657, -1.58082807349783, -0.770959557789091,
      -0.172720999330529, 0.76063400507768, 1.76387462553977, 41,
      "Q16", 0.0947005086160322, 0.0387951389188048, 53.2798074156279,
      0.336898315146607, -0.0892464579931759, -1.05497728361162, 0.0335971948357577,
      0.77372823162243, -0.939722796418452, -0.256422681360763, -0.0813578430806924,
      0.523416735816296, 0.754086585043611, 44, "Q17", 0.60041415558283,
      0, 41.0125457220686, 0.497501295768076, -0.00280755695881607,
      -0.555706331441168, -0.761046505138111, 0.822059097770019, -1.02834596081754,
      -0.691483374562431, -0.256046172219767, 0.909611077439237, 1.0662644301605,
      41, "Q18", 0.506741495175861, 0, 40.1839115097174, 0.67093939749926,
      -0.282131577170837, 0.158263397657417, -0.518587119713158, -0.0284840982726814,
      -0.988766721403271, -0.575678205684936, 0.001691291304034, 0.825045981699294,
      0.737707654084879, 50, "Q19", 0.865704762812691, 0, 39.1574808435734,
      0.539306136750675, -0.858429948862307, -0.0115422789163597,
      0.000382068465506258, 0.330284022562485, -0.712499052566803,
      -0.116354123378022, 0.0834866327654451, 0.374503849739858, 0.370862693439523,
      57, "Q20", 0.801865006553342, 0, 47.8120282233658
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      9849.73215934867, 10377.4629379964, 50, 20, 43.7845232606549,
      200, 0.719684070934137
    )
  )
})

# Consistency test 12, file: nominal2.csv, model: nominal responses ############

options <- initIRTOptions("itemResponseTheoryPolytomous")
options$emTolerance <- 0.0001
options$explanatoryText <- FALSE
options$tableFitStatistics <- TRUE
options$tableFitStatisticsCI <- 0.95
options$tableItemStatistics <- TRUE
options$tableParameterEstimates <- FALSE
options$plotHistogramAbility <- TRUE
options$plotTestInformation <- TRUE
options$plotItemInformationItems <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
options$plotItemInformationLabels <- FALSE
options$plotItemCharacteristic <- FALSE
options$plotItemCharacteristicGroup <- FALSE
options$plotItemCharacteristicLabels <- FALSE
options$plotPriorPosterior <- FALSE
options$plotPriorPosteriorLabels <- FALSE
options$model <- "nominal"
options$items <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20")
options$covariates <- list()
options$seed <- 1
options$emIterations <- 2000
set.seed(1)
results <- runAnalysis("itemResponseTheoryPolytomous", "nominal2.csv", options)

test_that("Histogram of Latent Ability plot matches", {
  plotName <- results[["results"]][["plotHistogramAbility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-latent-ability-12")
})

test_that("Item Information Curves plot matches", {
  plotName <- results[["results"]][["plotItemInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-information-curves-12")
})

test_that("Test Information Function plot matches", {
  plotName <- results[["results"]][["plotTestInformation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-information-function-12")
})

test_that("Additional Fit Statistics table results match", {
  table <- results[["results"]][["tableFitStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(0.861225443624931, 0, 0.0647068929173772, 0.650394867593575, 0.123961175879142)
  )
})

test_that("Item Information table results match", {
  table <- results[["results"]][["tableItemStatistics"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -0.127775800851189, -0.822333004728258, -0.336523987915768, -0.607506559368792,
      1.89413935286401, -1.68446375609925, 0.895949210982845, 0.405410549512482,
      -0.582116071279846, 0.965220066883766, 1, "Q1", 0.0204334033425304,
      0.298789475062434, 5.37448236999616, -0.597538757547457, -1.38707904642339,
      0.321659350137459, 0.14003289602282, 1.52292555781057, 0.214640332646518,
      -1.63028527109706, -0.252545324790196, 0.457059695800487, 1.21113056744025,
      3, "Q2", 0.000193499719796615, 0.33731033901884, 19.7254049269237,
      0.706131776632888, -0.260187154465021, -0.409802342670098, -0.0361422794977692,
      "", -4.11971131840883, 1.25136679706652, 0.663023046020739,
      2.20532147532157, "", 2, "Q3", 0.390455611982278, 0, 1.88088197172592,
      0.308429348849331, -0.0129018852766586, -0.68345703954572, -0.451014750963881,
      0.838944326936928, -0.673848516038793, -0.244183961814204, 0.232210168537958,
      0.151234902706464, 0.534587406608574, 4, "Q4", 0.0370331926117729,
      0.178000754797005, 10.2101166668275, 0.895025021393822, -0.0842571581755814,
      -0.411595782436685, -0.0236827047303236, -0.375489376051232,
      -0.776862756722378, -0.386652132097975, -0.330538233956202,
      0.399372312328005, 1.09468081044855, 5, "Q5", 0.056364246398843,
      0.153322644476604, 10.7594191607783, -0.366232414013858, -1.34991856631675,
      0.0386525989219329, -0.419324244904923, 2.09682262631359, 0.485607022585105,
      -0.757954234652825, -0.01162990013883, -0.445753509511426, 0.729730621717976,
      1, "Q6", 0.0502331278548387, 0.240478266719622, 3.8336600414592,
      0.275530243223719, -0.578185137502791, -0.802584015498205, 0.178458507984575,
      0.926780401792702, -1.03798627712755, -0.240124986083961, 0.149796366353153,
      0.144687716080944, 0.983627180777416, 4, "Q7", 0.194594112863833,
      0.102559371811861, 6.06161125030293, 3.06549466096712, 0.460467643882553,
      1.21606735999356, -2.91741465440728, -1.82461501043596, -3.49304449786102,
      -3.04235295266569, -1.67341938112842, 3.89990183734807, 4.30891499430706,
      0, "Q8", "NaN", "NaN", "NaN", 0.39036960260815, -0.867066600767563,
      0.128974688466137, -0.30120352640682, 0.648925836100095, -0.839610410388543,
      -0.185236450135111, -0.487533332264062, 0.155088162266861, 1.35729203052086,
      2, "Q9", 0.000342760753791786, 0.377382982673973, 15.9569557299666,
      0.509397352210777, 0.267849197004034, -1.13167879504965, 0.440853828219752,
      -0.0864215823849133, -1.4980953968166, -0.905156581384452, -0.63032809375022,
      0.699891383632641, 2.33368868831863, 4, "Q10", 0.421210796493179,
      0, 3.8891844056935, 0.839150491649984, -1.45512429189441, -0.0131722721090085,
      0.225169196147994, 0.40397687620544, -2.06371733621241, -1.98312298809894,
      0.642880995359104, 0.402374482358921, 3.00158484659332, 0, "Q11",
      "NaN", "NaN", "NaN", 0.949184559679346, -0.363899360429114,
      -1.42420493234735, 0.318125997996055, 0.520793735101066, -0.919118632550891,
      -0.743426675067172, 1.04410144416687, 0.334322448530259, 0.284121414920937,
      5, "Q12", 0.0247491224135985, 0.179087572238572, 12.8577278399249,
      -0.239276409261185, -0.543301848552289, -0.960574925744855,
      -0.0645973965881249, 1.80775058014645, -0.792992492235483, -0.97065640771885,
      -0.630011136341734, 0.640850327624953, 1.75280970867111, 0,
      "Q13", "NaN", "NaN", "NaN", 0.979742353991651, 0.533391620840791,
      -0.730278067866549, -0.929044767137, 0.146188860171107, -1.29170607126302,
      0.278716748749894, -0.591230555654906, 0.643532051016549, 0.960687827151484,
      4, "Q14", 0.00490898060536642, 0.235843476059733, 14.9019404591879,
      2.11783128962152, -15.7788036125258, -17.5567581639854, 12.4332223428197,
      18.7845081440699, 1.15294814348897, -11.5525897756869, -13.0721990841714,
      8.73095127488666, 14.7408894414827, 0, "Q15", "NaN", "NaN",
      "NaN", -0.490072801555337, -1.95977575079719, 0.551591818998134,
      0.149893877233023, 1.74836285612137, -0.37173174725345, -1.9315574345215,
      0.0461625492593204, 0.83561710794869, 1.42150952456695, 2, "Q16",
      0.0349116801752341, 0.219225623700508, 6.70986766051394, 1.40870148674351,
      0.776844701727224, -0.229125586817017, -0.880253292593084, -1.07616730906064,
      -1.34261388256935, 0.388675824210382, -0.826379273896893, -0.128643779679796,
      1.90896111193566, 3, "Q17", 0.010765549707629, 0.235969253934827,
      11.1851788539762, 1.93444294137124, -0.118041346348128, 0.522993351450851,
      0.175426970074183, -2.51482191654814, -1.77364906317985, 0.73278339616551,
      -1.38472452958368, -0.843381070417702, 3.26897126701572, 2,
      "Q18", 0.0836398395693419, 0.173865730656124, 4.9624706450656,
      -1.09667812039712, 0.454473970695061, -0.197019670939239, -0.0188796549681425,
      0.858103475609445, -2.12127716558844, 0.0367987675539059, 0.288487011876433,
      0.251056874711552, 1.54493451144655, 3, "Q19", 0.000657599477182315,
      0.310279176154643, 17.152155571815, 0.889658448488172, 0.0948774879412778,
      -0.920127320274627, -0.883329424741039, 0.818920808586217, -0.359196021149659,
      -0.306993554445993, -0.410978432133529, 0.430142578720574, 0.647025429008606,
      4, "Q20", 0.00250706066773324, 0.251704480785261, 16.417608546886
    )
  )
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      2496.11691149247, 2798.21654635012, 52, 20, 62.6684301131213,
      50, 0.147671706203592
    )
  )
})

################################################################################
