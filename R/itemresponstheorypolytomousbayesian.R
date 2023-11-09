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

itemResponseTheoryPolytomousBayesian <- function(jaspResults, dataset, options, ...) {
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

# Calculate probability p(Response = r | theta, a, b) with scaling constant D
# https://www.sciencedirect.com/topics/psychology/graded-response-model
.irtCalculateProbabilityPolytomous <- function(options, item, theta, a, b, t = NULL, D = 1.702) {
  if (options[["model"]] == "nominal") {
    ncats <- ncol(b)
  } else if (options[["model"]] %in% c("rsm", "grsm")) {
    ncats <- ncol(t)
  } else {
    ncats <- ncol(b) + 1
  }
  m <- ncats + 1
  p <- matrix(nrow = length(theta), ncol = ncats)
  if (options[["model"]] %in% c("Rasch", "gpcm", "rsm", "grsm")) {
    for (k in seq_len(ncats)) {
      num <- 0
      for (j in seq_len(k)) {
        if (k == 1) {
          num <- switch(options[["model"]],
            "Rasch" = num + theta,
            "gpcm" = num + theta,
            "rsm" = num + (theta - b[item, 1]),
            "grsm" = num + (theta - b[item, 1])
          )
        } else {
          num <- switch(options[["model"]],
            "Rasch" = num + (theta - b[item, k - 1]),
            "gpcm" = num + (D * a[item, 1]) * (theta - b[item, k - 1]),
            "rsm" = num + (theta - (b[item, 1] + t[item, k - 1])),
            "grsm" = num + (D * a[item, 1]) * (theta - (b[item, 1] + t[item, k - 1]))
          )
        }
      }
      num <- exp(num)
      denom <- 0
      for (ii in seq_len(ncats)) {
        smallsum <- 0
        for (j in seq_len(ii)) {
          if (k == 1) {
            smallsum <- switch(options[["model"]],
              "Rasch" = smallsum + theta,
              "gpcm" = smallsum + theta,
              "rsm" = smallsum + (theta - b[item, 1]),
              "grsm" = smallsum + (theta - b[item, 1])
            )
          } else {
            smallsum <- switch(options[["model"]],
              "Rasch" = smallsum + (theta - b[item, k - 1]),
              "gpcm" = smallsum + (D * a[item, 1]) * (theta - b[item, k - 1]),
              "rsm" = smallsum + (theta - (b[item, 1] + t[item, k - 1])),
              "grsm" = smallsum + (D * a[item, 1]) * (theta - (b[item, 1] + t[item, k - 1]))
            )
          }
        }
        denom <- denom + exp(smallsum)
      }
      p[, k] <- num / denom
    }
  } else if (options[["model"]] == "graded") {
    for (k in seq_len(ncats)) {
      if (k == 1) {
        p[, k] <- 1 - exp((D * a[item, 1]) * (theta - b[item, k])) / (1 + exp((D * a[item, 1]) * (theta - b[item, k])))
      } else if (k == ncats) {
        p[, k] <- exp((D * a[item, 1]) * (theta - b[item, k - 1])) / (1 + exp((D * a[item, 1]) * (theta - b[item, k - 1])))
      } else {
        p[, k] <- exp((D * a[item, 1]) * (theta - b[item, k - 1])) / (1 + exp((D * a[item, 1]) * (theta - b[item, k - 1]))) - exp((D * a[item, 1]) * (theta - b[item, k])) / (1 + exp((D * a[item, 1]) * (theta - b[item, k])))
      }
    }
  } else if (options[["model"]] == "nominal") {
    denominator <- 0
    for (k in seq_len(ncats)) {
      denominator <- denominator + exp((D * a[item, k]) * theta + b[item, k])
    }
    for (k in seq_len(ncats)) {
      numerator <- exp((D * a[item, k]) * theta + b[item, k])
      p[, k] <- numerator / denominator
    }
  }
  return(p)
}

# Nering, M. L., & Ostini, R. (Eds.). (2011). Handbook of polytomous item response theory models. Taylor & Francis, pp. 14-16
.irtCalculateItemInformationPolytomous <- function(options, item, theta, p_mat, a = NULL, b = NULL, D = 1.702) {
  if (options[["model"]] == "Rasch" || options[["model"]] == "rsm") {
    k <- matrix(seq_len(ncol(p_mat)), ncol = ncol(p_mat), nrow = length(theta), byrow = TRUE)
    info <- rowSums(k^2 * p_mat) - rowSums(k * p_mat)^2
  } else if (options[["model"]] == "gpcm" || options[["model"]] == "grsm") {
    k <- matrix(seq_len(ncol(p_mat)), ncol = ncol(p_mat), nrow = length(theta), byrow = TRUE)
    info <- D^2 * as.numeric(a[item, 1])^2 * (rowSums(k^2 * p_mat) - rowSums(k * p_mat)^2)
  } else if (options[["model"]] == "graded") {
    p_mat_deriv <- matrix(nrow = length(theta), ncol = ncol(p_mat))
    for (k in seq_len(ncol(p_mat))) {
      .partial_deriv <- approxfun(theta[-1], diff(p_mat[, k]) / diff(theta))
      p_mat_deriv[, k] <- .partial_deriv(theta)
    }
    info <- rowSums(p_mat_deriv^2 / p_mat)
  } else if (options[["model"]] == "nominal") {
    p_mat_deriv <- matrix(nrow = length(theta), ncol = ncol(p_mat))
    p_mat_second_deriv <- matrix(nrow = length(theta), ncol = ncol(p_mat))
    for (k in seq_len(ncol(p_mat))) {
      .partial_deriv <- approxfun(theta[-1], diff(p_mat[, k]) / diff(theta))
      .partial_second_deriv <- approxfun(theta[-1], diff(.partial_deriv(theta)) / diff(theta))
      p_mat_deriv[, k] <- .partial_deriv(theta)
      p_mat_second_deriv[, k] <- .partial_second_deriv(theta)
    }
    info <- rowSums(p_mat_deriv^2 / p_mat - p_mat_second_deriv)
  }
  return(info)
}
