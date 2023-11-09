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

Section
{
	property	int		ncovs:			0
	property	string	modeltype:		"Rasch"

	title:							qsTr("Advanced Options")
	columns:						3

	Group
	{
		title:						qsTr("Prior Distributions")

		Row
		{
			spacing:				10 * preferencesModel.uiScale
			visible:				ncovs == 0

			DropDown
			{
				id:					priorTheta
				name:				"priorTheta"
				indexDefaultValue:	0
				label:				qsTr("\u03B8's")
				values:
					[
					{ label: qsTr("Normal"),		value: "priorThetaNormal"},
					{ label: qsTr("Student-t"), 	value: "priorThetaStudent"},
					{ label: qsTr("Cauchy"), 		value: "priorThetaCauchy"},
					{ label: qsTr("Hierarchical"),	value: "priorThetaHierarchical"}
				]
				info:				qsTr("Choose the prior distribution for the latent ability (\u03B8). The prior distribution specifies the expected properties of \u03B8 before observing data.")
			}

			DoubleField
			{
				name:				"priorThetaNormalMean"
				text:				"\u03BC"
				defaultValue:		0
				min:				-9999999
				visible:			priorTheta.value == "priorThetaNormal"
				info:				qsTr("Specify the mean (\u03BC) of the normal prior distribution for \u03B8.")
			}

			DoubleField
			{
				name:				"priorThetaNormalSd"
				text:				"\u03C3"
				defaultValue:		1
				min:				0.001
				visible:			priorTheta.value == "priorThetaNormal"
				info:				qsTr("Specify the standard deviation (\u03C3) of the normal prior distribution for \u03B8.")
			}

			DoubleField
			{
				name:				"priorThetaStudentLocation"
				text:				qsTr("Location")
				defaultValue:		0
				min:				-9999999
				visible:			priorTheta.value == "priorThetaStudent"
				info:				qsTr("Specify the location parameter (mean) for the Student-t prior distribution for \u03B8.")
			}

			DoubleField
			{
				name:				"priorThetaStudentDf"
				text:				qsTr("df")
				defaultValue:		1
				min:				0.001
				visible:			priorTheta.value == "priorThetaStudent"
				info:				qsTr("Specify the degrees of freedom (df) for the Student-t prior distribution for \u03B8.")
			}

			DoubleField
			{
				name:				"priorThetaCauchyLocation"
				text:				qsTr("Location")
				defaultValue:		0
				min:				-9999999
				visible:			priorTheta.value == "priorThetaCauchy"
				info:				qsTr("Specify the location parameter of the Cauchy prior distribution for \u03B8.")
			}

			DoubleField
			{
				name:				"priorThetaCauchyScale"
				text:				qsTr("Scale")
				defaultValue:		1
				min:				0.001
				visible:			priorTheta.value == "priorThetaCauchy"
				info:				qsTr("Specify the scale parameter of the Cauchy prior distribution for \u03B8.")
			}

			DoubleField
			{
				name:				"priorThetaHierarchicalSd"
				text:				qsTr("Heterogeneity (\u03C3)")
				defaultValue:		1
				min:				0.000001
				decimals:			6
				visible:			priorTheta.value == "priorThetaHierarchical"
				info:				qsTr("Specify the standard deviation (\u03C3) for the hierarchical prior distribution for \u03B8 to account for heterogeneity.")
			}
		}

		Row
		{
			spacing:				10 * preferencesModel.uiScale
			visible:				ncovs > 0

			DropDown
			{
				id:					priorZeta
				name:				"priorZeta"
				indexDefaultValue:	0
				label:				qsTr("\u03B2's")
				values:
					[
					{ label: qsTr("Normal"),		value: "priorZetaNormal"},
					{ label: qsTr("Student-t"), 	value: "priorZetaStudent"},
					{ label: qsTr("Cauchy"), 		value: "priorZetaCauchy"},
					{ label: qsTr("Hierarchical"),	value: "priorZetaHierarchical"}
				]
				info:				qsTr("Choose the prior distribution for the latent regression coefficients \u03B2. The prior distribution specifies the expected properties of \u03B2 before observing data.")
			}

			DoubleField
			{
				name:				"priorZetaNormalMean"
				text:				"\u03BC"
				defaultValue:		0
				min:				-9999999
				visible:			priorZeta.value == "priorZetaNormal"
				info:				qsTr("Specify the mean (\u03BC) of the normal prior distribution for each \u03B2.")
			}

			DoubleField
			{
				name:				"priorZetaNormalSd"
				text:				"\u03C3"
				defaultValue:		1
				min:				0.001
				visible:			priorZeta.value == "priorZetaNormal"
				info:				qsTr("Specify the standard deviation (\u03C3) of the normal prior distribution for each \u03B2.")
			}

			DoubleField
			{
				name:				"priorZetaStudentLocation"
				text:				qsTr("Location")
				defaultValue:		0
				min:				-9999999
				visible:			priorZeta.value == "priorZetaStudent"
				info:				qsTr("Specify the location parameter (mean) for the Student-t prior distribution for each \u03B2.")
			}

			DoubleField
			{
				name:				"priorZetaStudentDf"
				text:				qsTr("df")
				defaultValue:		1
				min:				0.001
				visible:			priorZeta.value == "priorZetaStudent"
				info:				qsTr("Specify the degrees of freedom (df) for the Student-t prior distribution for each \u03B2.")
			}

			DoubleField
			{
				name:				"priorZetaCauchyLocation"
				text:				qsTr("Location")
				defaultValue:		0
				min:				-9999999
				visible:			priorZeta.value == "priorZetaCauchy"
				info:				qsTr("Specify the location parameter of the Cauchy prior distribution for each \u03B2.")
			}

			DoubleField
			{
				name:				"priorZetaCauchyScale"
				text:				qsTr("Scale")
				defaultValue:		1
				min:				0.001
				visible:			priorZeta.value == "priorZetaCauchy"
				info:				qsTr("Specify the scale parameter of the Cauchy prior distribution for each \u03B2.")
			}

			DoubleField
			{
				name:				"priorZetaHierarchicalSd"
				text:				qsTr("Heterogeneity (\u03C3)")
				defaultValue:		1
				min:				0.000001
				decimals:			6
				visible:			priorZeta.value == "priorZetaHierarchical"
				info:				qsTr("Specify the standard deviation (\u03C3) for the hierarchical prior distribution for each \u03B2 to account for heterogeneity.")
			}
		}

		Row
		{
			spacing:				10 * preferencesModel.uiScale
			visible:				modeltype != "Rasch" && modeltype != "rsm"

			DropDown
			{
				id:					priorAlpha
				name:				"priorAlpha"
				label:				qsTr("a's")
				indexDefaultValue:	0
				values:
					[
					{ label: qsTr("Lognormal"),			value: "priorAlphaLogNormal"},
					{ label: qsTr("Normal T[0, )"),		value: "priorAlphaNormal"},
					{ label: qsTr("Student-t T[0, )"),	value: "priorAlphaStudent"},
					{ label: qsTr("Cauchy T[0, )"), 	value: "priorAlphaCauchy"},
					{ label: qsTr("Hierarchical"),		value: "priorAlphaHierarchical"}
				]
				info:				qsTr("Choose the prior distribution for the discrimination parameters a in Rasch, 3-PL and 4-PL models.")
			}

			DoubleField
			{
				name:				"priorAlphaLogNormalMean"
				text:				"\u03BC"
				defaultValue:		0
				visible:			priorAlpha.value == "priorAlphaLogNormal"
				info:				qsTr("Specify the mean (\u03BC) of the lognormal prior distribution for the discrimination parameters a.")
			}

			DoubleField
			{
				name:				"priorAlphaLogNormalSd"
				text:				"\u03C3"
				defaultValue:		1
				min:				0.001
				visible:			priorAlpha.value == "priorAlphaLogNormal"
				info:				qsTr("Specify the standard deviation (\u03C3) of the lognormal prior distribution for the discrimination parameters a.")
			}

			DoubleField
			{
				name:				"priorAlphaNormalMean"
				text:				"\u03BC"
				defaultValue:		0
				visible:			priorAlpha.value == "priorAlphaNormal"
				info:				qsTr("Specify the mean (\u03BC) of the normal prior distribution for the discrimination parameters a.")
			}

			DoubleField
			{
				name:				"priorAlphaNormalSd"
				text:				"\u03C3"
				defaultValue:		1
				min:				0.001
				visible:			priorAlpha.value == "priorAlphaNormal"
				info:				qsTr("Specify the standard deviation (\u03C3) of the normal prior distribution for the discrimination parameters a.")
			}

			DoubleField
			{
				name:				"priorAlphaStudentLocation"
				text:				qsTr("Location")
				defaultValue:		0
				min:				0
				visible:			priorAlpha.value == "priorAlphaStudent"
				info:				qsTr("Specify the location parameter (mean) for the Student-t prior distribution for the discrimination parameters a.")
			}

			DoubleField
			{
				name:				"priorAlphaStudentDf"
				text:				qsTr("df")
				defaultValue:		1
				min:				0.001
				visible:			priorAlpha.value == "priorAlphaStudent"
				info:				qsTr("Specify the degrees of freedom (df) for the Student-t prior distribution for the discrimination parameters a.")
			}

			DoubleField
			{
				name:				"priorAlphaCauchyLocation"
				text:				qsTr("Location")
				defaultValue:		0
				min:				-9999999
				visible:			priorAlpha.value == "priorAlphaCauchy"
				info:				qsTr("Specify the location parameter of the Cauchy prior distribution for the discrimination parameters a.")
			}

			DoubleField
			{
				name:				"priorAlphaCauchyScale"
				text:				qsTr("Scale")
				defaultValue:		1
				min:				0.001
				visible:			priorAlpha.value == "priorAlphaCauchy"
				info:				qsTr("Specify the scale parameter of the Cauchy prior distribution for the discrimination parameters a.")
			}

			DoubleField
			{
				name:				"priorAlphaHierarchicalSd"
				text:				qsTr("Heterogeneity (\u03C3)")
				defaultValue:		1
				min:				0.000001
				decimals:			6
				visible:			priorAlpha.value == "priorAlphaHierarchical"
				info:				qsTr("Specify the standard deviation (\u03C3) for the hierarchical prior distribution for discrimination parameters a to account for heterogeneity.")
			}
		}

		Row
		{
			spacing:				10 * preferencesModel.uiScale

			DropDown
			{
				id:					priorBeta
				name:				"priorBeta"
				label:				qsTr("b's")
				indexDefaultValue:	0
				values:
					[
					{ label: qsTr("Normal"),		value: "priorBetaNormal"},
					{ label: qsTr("Student-t"), 	value: "priorBetaStudent"},
					{ label: qsTr("Cauchy"), 		value: "priorBetaCauchy"},
					{ label: qsTr("Hierarchical"),	value: "priorBetaHierarchical"}
				]
				info:				qsTr("Choose the prior distribution for the difficulty parameters b.")
			}

			DoubleField
			{
				name:				"priorBetaNormalMean"
				text:				"\u03BC"
				defaultValue:		0
				min:				-9999999
				visible:			priorBeta.value == "priorBetaNormal"
				info:				qsTr("Specify the mean (\u03BC) of the normal prior distribution for the difficulty parameters b.")
			}

			DoubleField
			{
				name:				"priorBetaNormalSd"
				text:				"\u03C3"
				defaultValue:		1
				min:				0.001
				visible:			priorBeta.value == "priorBetaNormal"
				info:				qsTr("Specify the standard deviation (\u03C3) of the normal prior distribution for the difficulty parameters b.")
			}

			DoubleField
			{
				name:				"priorBetaStudentLocation"
				text:				qsTr("Location")
				defaultValue:		0
				min:				-9999999
				visible:			priorBeta.value == "priorBetaStudent"
				info:				qsTr("Specify the location parameter (mean) for the Student-t prior distribution for the difficulty parameters b.")
			}

			DoubleField
			{
				name:				"priorBetaStudentDf"
				text:				qsTr("df")
				defaultValue:		1
				min:				0.001
				visible:			priorBeta.value == "priorBetaStudent"
				info:				qsTr("Specify the degrees of freedom (df) for the Student-t prior distribution for the difficulty parameters b.")
			}

			DoubleField
			{
				name:				"priorBetaCauchyLocation"
				text:				qsTr("Location")
				defaultValue:		0
				min:				-9999999
				visible:			priorBeta.value == "priorBetaCauchy"
				info:				qsTr("Specify the location parameter of the Cauchy prior distribution for the difficulty parameters b.")
			}

			DoubleField
			{
				name:				"priorBetaCauchyScale"
				text:				qsTr("Scale")
				defaultValue:		1
				min:				0.001
				visible:			priorBeta.value == "priorBetaCauchy"
				info:				qsTr("Specify the scale parameter of the Cauchy prior distribution for the difficulty parameters b.")
			}

			DoubleField
			{
				name:				"priorBetaHierarchicalSd"
				text:				qsTr("Heterogeneity (\u03C3)")
				defaultValue:		1
				min:				0.000001
				decimals:			6
				visible:			priorBeta.value == "priorBetaHierarchical"
				info:				qsTr("Specify the standard deviation (\u03C3) for the hierarchical prior distribution for difficulty parameters b to account for heterogeneity.")
			}
		}

		Row
		{
			spacing:				10 * preferencesModel.uiScale
			visible:				modeltype == "3PL" || modeltype == "4PL"

			DropDown
			{
				id:					priorGamma
				name:				"priorGamma"
				label:				qsTr("c's")
				indexDefaultValue:	0
				values:
					[
					{ label: qsTr("Normal T[0, 1]"),	value: "priorGammaNormal"},
					{ label: qsTr("Student-t T[0, 1]"), value: "priorGammaStudent"},
					{ label: qsTr("Uniform"),			value: "priorGammaUniform"},
					{ label: qsTr("Beta"),				value: "priorGammaBeta"},
					{ label: qsTr("Cauchy T[0, 1]"),	value: "priorGammaCauchy"},
					{ label: qsTr("Hierarchical"),		value: "priorGammaHierarchical"}
				]
				info:				qsTr("Choose the prior distribution for the guessing parameters c in 3-PL and 4-PL models.")
			}

			DoubleField
			{
				name:				"priorGammaNormalMean"
				text:				"\u03BC"
				defaultValue:		0
				visible:			priorGamma.value == "priorGammaNormal"
				info:				qsTr("Specify the mean (\u03BC) of the normal prior distribution for the guessing parameters c.")
			}

			DoubleField
			{
				name:				"priorGammaNormalSd"
				text:				"\u03C3"
				defaultValue:		0.1
				min:				0.001
				visible:			priorGamma.value == "priorGammaNormal"
				info:				qsTr("Specify the standard deviation (\u03C3) of the normal prior distribution for the guessing parameters c.")
			}

			DoubleField
			{
				name:				"priorGammaStudentLocation"
				text:				qsTr("Location")
				defaultValue:		0
				visible:			priorGamma.value == "priorGammaStudent"
				info:				qsTr("Specify the location parameter (mean) for the Student-t prior distribution for the guessing parameters c.")
			}

			DoubleField
			{
				name:				"priorGammaStudentDf"
				text:				qsTr("df")
				defaultValue:		1
				min:				0.001
				visible:			priorGamma.value == "priorGammaStudent"
				info:				qsTr("Specify the degrees of freedom (df) for the Student-t prior distribution for the guessing parameters c.")
			}

			DoubleField
			{
				id:					priorGammaUniformMin
				name:				"priorGammaUniformMin"
				text:				qsTr("Min.")
				defaultValue:		0
				max:				priorGammaUniformMax.value
				min:				0
				visible:			priorGamma.value == "priorGammaUniform"
				info:				qsTr("Specify the minimum value for the uniform prior distribution for the guessing parameters c.")
			}

			DoubleField
			{
				id:					priorGammaUniformMax
				name:				"priorGammaUniformMax"
				text:				qsTr("Max.")
				defaultValue:		0.25
				min:				priorGammaUniformMin.value
				max:				1
				visible:			priorGamma.value == "priorGammaUniform"
				info:				qsTr("Specify the maximum value for the uniform prior distribution for the guessing parameters c.")
			}

			DoubleField
			{
				name:				"priorGammaBetaAlpha"
				text:				"\u03B1"
				defaultValue:		1
				min:				0.5
				visible:			priorGamma.value == "priorGammaBeta"
				info:				qsTr("Specify the shape parameter (\u03B1) of the beta prior distribution for the guessing parameters c.")
			}

			DoubleField
			{
				name:				"priorGammaBetaBeta"
				text:				"\u03B2"
				defaultValue:		1
				min:				0.5
				visible:			priorGamma.value == "priorGammaBeta"
				info:				qsTr("Specify the shape parameter (\u03B2) of the beta prior distribution for the guessing parameters c.")
			}

			DoubleField
			{
				name:				"priorGammaCauchyLocation"
				text:				qsTr("Location")
				defaultValue:		0
				min:				-9999999
				visible:			priorGamma.value == "priorGammaCauchy"
				info:				qsTr("Specify the location parameter of the Cauchy prior distribution for the guessing parameters c.")
			}

			DoubleField
			{
				name:				"priorGammaCauchyScale"
				text:				qsTr("Scale")
				defaultValue:		1
				min:				0.001
				visible:			priorGamma.value == "priorGammaCauchy"
				info:				qsTr("Specify the scale parameter of the Cauchy prior distribution for the guessing parameters c.")
			}

			DoubleField
			{
				name:				"priorGammaHierarchicalSd"
				text:				qsTr("Heterogeneity (\u03C3)")
				defaultValue:		0.1
				min:				0.000001
				decimals:			6
				visible:			priorGamma.value == "priorGammaHierarchical"
				info:				qsTr("Specify the standard deviation (\u03C3) for the hierarchical prior distribution for the guessing parameters c to account for heterogeneity.")
			}
		}

		Row
		{
			spacing: 				10 * preferencesModel.uiScale
			visible: 				modeltype == "4PL"

			DropDown
			{
				id:					priorDelta
				name:				"priorDelta"
				label:				qsTr("d's")
				indexDefaultValue:	0
				values:
					[
					{ label: qsTr("Normal T[0, 1]"),	value: "priorDeltaNormal"},
					{ label: qsTr("Student-t T[0, 1]"), value: "priorDeltaStudent"},
					{ label: qsTr("Uniform"),			value: "priorDeltaUniform"},
					{ label: qsTr("Beta"),				value: "priorDeltaBeta"},
					{ label: qsTr("Cauchy T[0, 1]"),	value: "priorDeltaCauchy"},
					{ label: qsTr("Hierarchical"),		value: "priorDeltaHierarchical"}
				]
				info:				qsTr("Choose the prior distribution for the slip parameters d in the 4-PL model.")
			}

			DoubleField
			{
				name:				"priorDeltaNormalMean"
				text:				"\u03BC"
				defaultValue:		1
				visible:			priorDelta.value == "priorDeltaNormal"
				info:				qsTr("Specify the mean (\u03BC) of the normal prior distribution for the slip parameters d.")
			}

			DoubleField
			{
				name:				"priorDeltaNormalSd"
				text:				"\u03C3"
				defaultValue:		0.1
				min:				0.001
				visible:			priorDelta.value == "priorDeltaNormal"
				info:				qsTr("Specify the standard deviation (\u03C3) of the normal prior distribution for the slip parameters d.")
			}

			DoubleField
			{
				name:				"priorDeltaStudentLocation"
				text:				qsTr("Location")
				defaultValue:		1
				visible:			priorDelta.value == "priorDeltaStudent"
				info:				qsTr("Specify the location parameter (mean) for the Student-t prior distribution for the slip parameters d.")
			}

			DoubleField
			{
				name:				"priorDeltaStudentDf"
				text:				qsTr("df")
				defaultValue:		1
				min:				0.001
				visible:			priorDelta.value == "priorDeltaStudent"
				info:				qsTr("Specify the degrees of freedom (df) for the Student-t prior distribution for the slip parameters d.")
			}

			DoubleField
			{
				id:					priorDeltaUniformMin
				name:				"priorDeltaUniformMin"
				text:				qsTr("Min.")
				defaultValue:		0.75
				max:				priorDeltaUniformMax.value
				min:				0
				visible:			priorDelta.value == "priorDeltaUniform"
				info:				qsTr("Specify the minimum value for the uniform prior distribution for the slip parameters d.")
			}

			DoubleField
			{
				id:					priorDeltaUniformMax
				name:				"priorDeltaUniformMax"
				text:				qsTr("Max.")
				defaultValue:		1
				min:				priorDeltaUniformMin.value
				max:				1
				visible:			priorDelta.value == "priorDeltaUniform"
				info:				qsTr("Specify the maximum value for the uniform prior distribution for the slip parameters d.")
			}

			DoubleField
			{
				name:				"priorDeltaBetaAlpha"
				text:				"\u03B1"
				defaultValue:		1
				min:				0.5
				visible:			priorDelta.value == "priorDeltaBeta"
				info:				qsTr("Specify the shape parameter (\u03B1) of the beta prior distribution for the slip parameters d.")
			}

			DoubleField
			{
				name:				"priorDeltaBetaBeta"
				text:				"\u03B2"
				defaultValue:		1
				min:				0.5
				visible:			priorDelta.value == "priorDeltaBeta"
				info:				qsTr("Specify the shape parameter (\u03B2) of the beta prior distribution for the slip parameters d.")
			}

			DoubleField
			{
				name:				"priorDeltaCauchyLocation"
				text:				qsTr("Location")
				defaultValue:		1
				min:				-9999999
				visible:			priorDelta.value == "priorDeltaCauchy"
				info:				qsTr("Specify the location parameter of the Cauchy prior distribution for the slip parameters d.")
			}

			DoubleField
			{
				name:				"priorDeltaCauchyScale"
				text:				qsTr("Scale")
				defaultValue:		1
				min:				0.001
				visible:			priorDelta.value == "priorDeltaCauchy"
				info:				qsTr("Specify the scale parameter of the Cauchy prior distribution for the slip parameters d.")
			}

			DoubleField
			{
				name:				"priorDeltaHierarchicalSd"
				text:				qsTr("Heterogeneity (\u03C3)")
				defaultValue:		0.1
				min:				0.000001
				decimals:			6
				visible:			priorDelta.value == "priorDeltaHierarchical"
				info:				qsTr("Specify the standard deviation (\u03C3) for the hierarchical prior distribution for the slip parameters d to account for heterogeneity.")
			}
		}

		Row
		{
			spacing: 				10 * preferencesModel.uiScale
			visible: 				modeltype == "rsm" || modeltype == "grsm"

			DropDown
			{
				id:					priorThreshold
				name:				"priorThreshold"
				label:				qsTr("Thresholds")
				indexDefaultValue:	0
				values:
					[
					{ label: qsTr("Normal"),	value: "priorThresholdNormal"},
					{ label: qsTr("Student-t"), value: "priorThresholdStudent"},
					{ label: qsTr("Cauchy"),	value: "priorThresholdCauchy"},
					{ label: qsTr("Uniform"),	value: "priorThresholdUniform"}
				]
				info:				qsTr("Choose the prior distribution for the threshold parameters in the (generalized) rating scale model.")
			}

			DoubleField
			{
				name:				"priorThresholdNormalMean"
				text:				"\u03BC"
				defaultValue:		0
				visible:			priorThreshold.value == "priorThresholdNormal"
				info:				qsTr("Specify the mean (\u03BC) of the normal prior distribution for the threshold parameterss.")
			}

			DoubleField
			{
				name:				"priorThresholdNormalSd"
				text:				"\u03C3"
				defaultValue:		5
				min:				0.001
				visible:			priorThreshold.value == "priorThresholdNormal"
				info:				qsTr("Specify the standard deviation (\u03C3) of the normal prior distribution for the threshold parameters.")
			}

			DoubleField
			{
				name:				"priorThresholdStudentLocation"
				text:				qsTr("Location")
				defaultValue:		0
				visible:			priorThreshold.value == "priorThresholdStudent"
				info:				qsTr("Specify the location parameter (mean) for the Student-t prior distribution for the threshold parameters.")
			}

			DoubleField
			{
				name:				"priorThresholdStudentDf"
				text:				qsTr("df")
				defaultValue:		1
				min:				0.001
				visible:			priorThreshold.value == "priorThresholdStudent"
				info:				qsTr("Specify the degrees of freedom (df) for the Student-t prior distribution for the threshold parameters.")
			}

			DoubleField
			{
				name:				"priorThresholdCauchyLocation"
				text:				qsTr("Location")
				defaultValue:		0
				min:				-9999999
				visible:			priorThreshold.value == "priorThresholdCauchy"
				info:				qsTr("Specify the location parameter of the Cauchy prior distribution for the threshold parameters.")
			}

			DoubleField
			{
				name:				"priorThresholdCauchyScale"
				text:				qsTr("Scale")
				defaultValue:		5
				min:				0.001
				visible:			priorThreshold.value == "priorThresholdCauchy"
				info:				qsTr("Specify the scale parameter of the Cauchy prior distribution for the threshold parameters.")
			}

			DoubleField
			{
				id:					priorThresholdUniformMin
				name:				"priorThresholdUniformMin"
				text:				qsTr("Min.")
				defaultValue:		-10
				max:				priorThresholdUniformMax.value
				visible:			priorThreshold.value == "priorThresholdUniform"
				negativeValues:		true
				info:				qsTr("Specify the minimum value for the uniform prior distribution for the threshold parameters.")
			}

			DoubleField
			{
				id:					priorThresholdUniformMax
				name:				"priorThresholdUniformMax"
				text:				qsTr("Max.")
				defaultValue:		10
				min:				priorThresholdUniformMin.value
				visible:			priorThreshold.value == "priorThresholdUniform"
				negativeValues:		true
				info:				qsTr("Specify the maximum value for the uniform prior distribution for the threshold parameters.")
			}
		}

		DoubleField
		{
			name:					"priorScaling"
			text:					qsTr("Scaling constant")
			defaultValue:			1.702
			decimals:				3
			min:					0.001
			enabled:				modeltype != "Rasch" && modeltype != "rsm"
			info:					qsTr("Specify the scaling constant for the model. Default is 1.702 (Camilli, 1994).")
		}
	}

	Group
	{
		title:						qsTr("MCMC Options")

		IntegerField
		{
			id:						samples
			name:					"samples"
			text:					qsTr("Samples")
			defaultValue:			2000
			min:					500
			info:					qsTr("Specify the number of samples to be collected by each chain in the MCMC process after the burn-in phase. A larger number improves the precision of parameter estimates.")
		}

		IntegerField
		{
			name:					"burnin"
			text:					qsTr("Warmup")
			defaultValue:			1000
			min:					200
			max:					samples.value - 300
			info:					qsTr("Specify the number of warmup iterations in each chain of the MCMC process. These iterations are discarded to ensure that the chain reaches the target distribution.")
		}

		IntegerField
		{
			name:					"chains"
			text:					qsTr("Chains")
			defaultValue:			4
			min:					1
			info:					qsTr("Specify the number of independent MCMC chains to run in parallel. Running multiple chains helps assess convergence and improves results.")
		}
	}

	Group
	{
		title:						qsTr("NUTS Algoritm")

		IntegerField
		{
			name:					"seed"
			text:					qsTr("Seed")
			defaultValue:			1
			min:					1
			max:					99999999
			info:					qsTr("Specify the random seed for reproducibility.")
		}

		DoubleField
		{
			name:					"nutsAdaptDelta"
			text:					qsTr("Adapt delta")
			defaultValue:			0.8
			min:					0.001
			max:					0.999
			decimals:				3
			info:					qsTr("Specify is the target average proposal acceptance probability during Stan's adaptation period.")
		}

		IntegerField
		{
			name:					"nutsMaxTreedepth"
			text:					qsTr("Max. treedepth")
			defaultValue:			10
			min:					5
			info:					qsTr("Specify the max value, in exponents of 2, of what the binary tree size in the NUTS algorithm should have.")
		}
	}
}