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

itemResponseTheoryPolytomous <- function(jaspResults, dataset, options, ...) {
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

  # Create the histogram of latent ability
  .irtIRTHistogram(dataset, options, jaspResults, ready, position = 7)

  # Create the test information function
  .irtIRTTestInfoCurve(dataset, options, jaspResults, ready, position = 9)

  # Create the item information curves
  .irtIRTItemInfoCurve(dataset, options, jaspResults, ready, position = 11)

  # Create the item information curves
  .irtIRTItemCharCurve(dataset, options, jaspResults, ready, position = 13)
}
