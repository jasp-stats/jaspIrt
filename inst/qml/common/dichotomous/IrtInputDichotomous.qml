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
		text:		qsTr("Rasch")
		checked:	true
		info:		qsTr("The Rasch model is a fundamental IRT model that assumes equal discrimination for all items and that the probability of a correct response depends only on the person's ability and the item's difficulty. It is often used as a baseline model for IRT analysis. Formula: P(X = 1) = 1 / (1 + exp(-[(θ - b)])).")
	}

	RadioButton
	{
		name:		"2PL"
		text:		qsTr("2-Parameter logistic")
		info:		qsTr("The 2-Parameter Logistic IRT model extends the Rasch model by introducing item discrimination parameters. It considers the ability of items to discriminate between individuals with different abilities in addition to item difficulty. This model is useful when items have varying levels of discrimination. Formula: P(X = 1) = 1 / (1 + exp(-[a*(θ - b)])).")
	}

	RadioButton
	{
		name:		"3PL"
		text:		qsTr("3-Parameter logistic")
		info:		qsTr("The 3-Parameter Logistic IRT model further extends the 2PL model by adding a guessing parameter. It accounts for the probability of guessing the correct response for items, making it suitable for multiple-choice items or when guessing may be a factor. It includes item difficulty, discrimination, and guessing parameters. Formula: P(X = 1) = c + (1 - c) / (1 + exp(-[a*(θ - b)])).")
	}

	RadioButton
	{
		name:		"4PL"
		text:		qsTr("4-Parameter logistic")
		info:		qsTr("The 4-Parameter Logistic IRT model is an extension of the 3PL model that includes a parameter for the upper asymptote of the item characteristic curve. It is suitable for items with a strong upper limit, such as speeded tests or tests with a performance ceiling. Formula: P(X = 1) = c + (d - c) / (1 + exp(-[a*(θ - b)])).")
	}
}
