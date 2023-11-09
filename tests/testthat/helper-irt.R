initIRTOptions <- function(analysis) {
  options <- c(
    jaspTools::analysisOptions(analysis),
    irtOptions(analysis)
  )
  return(options)
}

irtOptions <- function(analysis) {
  path <- testthat::test_path("..", "..", "inst", "qml", "common")
  files <- list.files(path, full.names = TRUE)
  if (analysis %in% c("itemResponseTheoryDichotomous", "itemResponseTheoryBayesianDichotomous")) {
    files <- c(files, list.files(testthat::test_path("..", "..", "inst", "qml", "common", "dichotomous"), full.names = TRUE))
  } else if (analysis %in% c("itemResponseTheoryPolytomous", "itemResponseTheoryBayesianPolytomous")) {
    files <- c(files, list.files(testthat::test_path("..", "..", "inst", "qml", "common", "polytomous"), full.names = TRUE))
  }
  if (analysis %in% c("itemResponseTheoryDichotomous", "itemResponseTheoryPolytomous")) {
    files <- c(files, list.files(testthat::test_path("..", "..", "inst", "qml", "common", "classical"), full.names = TRUE))
  } else if (analysis %in% c("itemResponseTheoryBayesianDichotomous", "itemResponseTheoryBayesianPolytomous")) {
    files <- c(files, list.files(testthat::test_path("..", "..", "inst", "qml", "common", "bayesian"), full.names = TRUE))
  }
  files <- files[-which(sapply(files, function(x) dir.exists(x)))]
  options <- lapply(files, jaspTools:::readQML) |>
    lapply(function(x) {
      x$plotWidth <- NULL
      x$plotHeight <- NULL
      return(x)
    }) |>
    (function(x) {
      do.call(c, x)
    })()
  return(options)
}
