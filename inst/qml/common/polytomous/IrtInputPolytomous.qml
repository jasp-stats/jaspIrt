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

RadioButtonGroup
{
	title:			qsTr("Item Response Function")
	name:			"model"
	info:			qsTr("Select the Item Response Theory (IRT) model for the analysis.")

	RadioButton
	{
		name:		"Rasch"
		text:		qsTr("Partial credit")
		checked:	true
		info:		qsTr("The partial credit model is tailored to address polytomous items, like Likert-type scales, by modeling the probability of selecting specific item categories based on the latent ability. This model excels in analyzing responses to items with ordered categories.")
	}

	RadioButton
	{
		name:		"gpcm"
		text:		qsTr("Generalized partial credit")
		info:		qsTr("The Generalized Partial Credit Model (GPCM) is tailored for polytomous items, often seen in Likert-type scales and other multi-category assessments. It characterizes the likelihood of endorsing or selecting various item categories based on the underlying latent ability. This model is particularly well-suited for examining responses to items with ordered response categories.")
	}

	RadioButton
	{
		name:		"rsm"
		text:		qsTr("Rating scale")
		info:		qsTr("The Rating Scale Model (RSM) is a type of item response theory (IRT) model used in the field of psychometrics to analyze responses to ordered categorical items, such as Likert scale items. The RSM is specifically designed for situations where responses are measured on a fixed or ordinal scale. The model estimates the probability of a person endorsing a particular category or response option for an item.")
	}

	RadioButton
	{
		name:		"grsm"
		text:		qsTr("Generalized rating scale")
		info:		qsTr("The Generalized Rating Scale Model (GRSM) is a type of item response theory (IRT) model used in the field of psychometrics to analyze responses to ordered categorical items, such as Likert scale items.")
	}

	RadioButton
	{
		name:		"graded"
		text:		qsTr("Graded response")
		info:		qsTr("The graded gesponse model is designed for polytomous (multiple-category) items, such as Likert-type scales. It models the probability of responding to different item categories as a function of the latent ability. This model is ideal for analyzing responses to items with ordered response categories.")
	}

	RadioButton
	{
		name:		"nominal"
		text:		qsTr("Nominal response")
		info:		qsTr("The nominal response model, tailored for items with multiple categories, like nominal or ordinal data, offers a framework for estimating the probabilities of selecting specific response categories based on the underlying latent ability. This model is especially suitable for examining responses to items with non-ordered or unordered response categories.")
	}
}
