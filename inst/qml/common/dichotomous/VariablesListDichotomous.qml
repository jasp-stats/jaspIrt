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

VariablesForm
{
	readonly	property 	int		nitems:			items.count
	readonly	property 	int		ncovs:			covariates.count

	AvailableVariablesList
	{
		name: 				"variablesList"
		info:				qsTr("Select the variables to be analyzed.")
	}

	AssignedVariablesList
	{
		id:					items
		name: 				"items"
		title: 				qsTr("Item Responses")
		allowedColumns: 	["nominal"]
		info:				qsTr("Select the variables representing item responses for the analysis.")
	}

	AssignedVariablesList
	{
		id:					covariates
		name: 				"covariates"
		title: 				qsTr("Person Covariates")
		allowedColumns:		["nominal", "ordinal", "scale"]
		implicitHeight:		125 * preferencesModel.uiScale
		info:				qsTr("Select the variables representing the person-level covariates for the analysis.")
	}
}
