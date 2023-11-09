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

import "./common" as COMMON
import "./common/polytomous" as POLY
import "./common/classical" as CLASSICAL

Form
{
	info: qsTr("Item Response Theory (IRT) is a statistical framework used to analyze and model how individuals respond to items or questions on tests or assessments. It focuses on understanding the relationship between a person's latent ability and their responses to specific items, taking into account item difficulty and discrimination parameters. IRT provides a way to estimate and compare individuals' latent abilities and helps create assessments that are more precise and informative.")

	CheckBox { name: "bayesian"; checked: false; visible: false } // Invisible option
	CheckBox { name: "dichotomous"; checked: false; visible: false } // Invisible option
	POLY.VariablesListPolytomous { }
	POLY.IrtInputPolytomous { }
	COMMON.IrtOutput { bayesian: false; dichotomous: false }
	COMMON.ItemInfoCurves { }
	COMMON.ItemCharCurves { dichotomous: false }
	CLASSICAL.AdvancedOptions { }
	COMMON.DownloadReport { }
}
