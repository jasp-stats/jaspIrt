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
	title: qsTr("Item Characteristic Curves")
	property	bool	dichotomous:	true

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "plotItemCharacteristicVariables"; source: "items" }
		AssignedVariablesList {	name: "plotItemCharacteristicItems" }
	}

	Group
	{
		CheckBox
		{
			name:				"plotItemCharacteristicGroup"
			text:				qsTr("Group into one figure")
			checked:			dichotomous
			visible:			false
			info:				qsTr("Group item characteristic curves into one figure for easy comparison and interpretation.")
		}

		CheckBox
		{
			name:				"plotItemCharacteristicLabels"
			text:				qsTr("Display labels")
			info:				qsTr("Display labels on the item characteristic curves to provide a clear representation which item belongs to which curve.")
		}

		Group
		{
			visible:			!dichotomous
			title:				qsTr("X-axis range")
			columns:			2
			info:				qsTr("Control the limits of the x-axis in the item characteristic curve plots.")

			DoubleField
			{
				id:				plotItemCharacteristicFromX
				name:			"plotItemCharacteristicFromX"
				text:			qsTr("From")
				negativeValues:	true
				defaultValue:	-3
				max:			plotItemCharacteristicToX.value
				info:			qsTr("Specify the lower limit for the x-axis in the item characteristic curve plots.")
			}

			DoubleField
			{
				id:				plotItemCharacteristicToX
				name:			"plotItemCharacteristicToX"
				text:			qsTr("to")
				negativeValues:	true
				defaultValue:	3
				min:			plotItemCharacteristicFromX.value
				info:			qsTr("Specify the upper limit for the x-axis in the item characteristic curve plots.")
			}
		}
	}
}
