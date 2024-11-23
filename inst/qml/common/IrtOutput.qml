//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Column
{
	property	bool	bayesian:		false
	property	bool	dichotomous:	true
	property	string	modeltype:		"Rasch"

	spacing:				20 * preferencesModel.uiScale

	Group
	{
		title:				qsTr("Display")

		CheckBox
		{
			name:			"explanatoryText"
			text:			qsTr("Explanatory text")
			info:			qsTr("Include explanatory text in the report to provide additional information or context.")
		}
	}

	Group
	{
		title:				qsTr("Tables")

		CheckBox
		{
			name:			"tableFitStatistics"
			text:			qsTr("Additional fit statistics")
			info:			qsTr("Generate a table of additional fit statistics to assess the model's goodness of fit.")
			visible:		!bayesian

			CIField
			{
				name:		"tableFitStatisticsCI"
				text:		qsTr("Confidence interval")
				info:		qsTr("Specify the confidence interval for the RMSEA, which helps in assessing the uncertainty of fit indices.")
			}
		}

		CheckBox
		{
			name:			"tableItemStatistics"
			text:			qsTr("Item information")
			info:			qsTr("Generate a table of item-level information to provide insights into individual item performance and characteristics.")
		}

		CheckBox
		{
			name:			"tableParameterEstimates"
			text:			qsTr("Parameter estimates")
			visible:		bayesian
			info:			qsTr("Generate a table of all parameters that are estimated.")
		}

		CheckBox
		{
			name:			"tableDifAnalysis"
			text:			qsTr("Differential item functioning (DIF)")
			visible:		!bayesian
			info:			qsTr("Generate a table showing the output of a likelihood-ratio test for Differential Item Functioning (DIF).")

			DropDown
			{
				name: 					"groupingVariable"
				label: 					qsTr("Grouping variable")
				showVariableTypeIcon: 	true
				addEmptyValue: 			true
				source: 				[ { model: columnsModel, use: "type=nominal"} ]
			}

			Group
			{
				title:		qsTr("Parameters")

				CheckBox
				{
					name:		"tableDifAnalysisDiscrimination"
					text:		qsTr("Discrimination")
					enabled:	dichotomous ? (modeltype == "2PL" || modeltype == "3PL" || modeltype == "4PL") : (modeltype == "gpcm" || modeltype == "grsm" || modeltype == "graded" || modeltype == "nominal") // TODO
					checked:	true
				}

				CheckBox
				{
					name:		"tableDifAnalysisDifficulty"
					text:		qsTr("Difficulty")
					checked:	true
				}

				CheckBox
				{
					name:		"tableDifAnalysisGuess"
					text:		qsTr("Guessing")
					enabled:	dichotomous ? (modeltype == "3PL" || modeltype == "4PL") : false
					visible:	dichotomous
					checked:	true
				}

				CheckBox
				{
					name:		"tableDifAnalysisSlip"
					text:		qsTr("Slip")
					enabled:	dichotomous ? modeltype == "4PL" : false
					visible:	dichotomous
					checked:	true
				}

				CheckBox
				{
					name:		"tableDifAnalysisThreshold"
					text:		qsTr("Threshold")
					enabled:	dichotomous ? modeltype == "4PL" : (modeltype == "rsm" || modeltype == "grsm")
					visible:	!dichotomous
					checked:	true
				}
			}
		}
	}

	Group
	{
		title:				qsTr("Plots")

		CheckBox
		{
			name:			"plotHistogramAbility"
			text:			qsTr("Histogram of latent ability")
			info:			qsTr("Create a histogram of the latent ability scores, providing an overview of the distribution of individuals' abilities.")
		}

		CheckBox
		{
			name:			"plotTestInformation"
			text:			qsTr("Test information function")
			info:			qsTr("Generate a plot of the test information function, showing the information of the test at different levels of the latent ability.")
		}

		CheckBox
		{
			name:			"plotPriorPosterior"
			text:			qsTr("Prior and posterior distributions")
			visible:		bayesian
			info:			qsTr("Generate prior and posterior plots in Bayesian analyses to visualize the prior and posterior distribution of model parameters.")

			CheckBox
			{
				name:		"plotPriorPosteriorLabels"
				text:		qsTr("Display labels")
				info:		qsTr("Display labels on the posterior distributions to provide a clear representation which posterior belongs to which item.")
			}
		}
	}
}
