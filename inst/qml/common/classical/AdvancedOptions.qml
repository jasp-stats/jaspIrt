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
	title:					qsTr("Advanced Options")

	DoubleField
	{
		name:					"priorScaling"
		text:					qsTr("Scaling constant")
		defaultValue:			1.702
		decimals:				3
		min:					0.001
		enabled:				false
		info:					qsTr("Specify the scaling constant for the model. Default is 1.702 (Camilli, 1994).")
	}

	Group
	{
		title:				qsTr("EM Algoritm")

		IntegerField
		{
			name:				"seed"
			text:				qsTr("Seed")
			defaultValue:		Math.floor(Math.random() * 1000000) // Init with random integer in [1,...,999999]
			min:				-999999
			max:				999999
			info:				qsTr("Specify the random seed for reproducibility.")
		}

		IntegerField
		{
			name:			"emIterations"
			text:			qsTr("Max. iterations")
			defaultValue:	2000
			min:			500
			info:			qsTr("Specify the maximum number of iterations of the EM algoritm.")
		}

		DoubleField
		{
			name:			"emTolerance"
			text:			qsTr("Tolerance")
			defaultValue:	0.0001
			decimals:		6
			min:			0.000001
			info:			qsTr("Specify the tolerance for the EM algoritm.")
		}
	}
}