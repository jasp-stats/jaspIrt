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
import "./common/bayesian" as BAYES

Form
{
	info: qsTr("Bayesian Item Response Theory (IRT) extends traditional IRT by incorporating Bayesian statistical methods. It allows for the modeling of uncertainty in parameter estimates, including latent abilities and item characteristics, and provides a more robust framework for handling complex assessments. Bayesian IRT is particularly valuable when dealing with limited data or complex item structures, as it produces posterior distributions that offer richer information for decision-making and analysis.")

	CheckBox { name: "bayesian"; checked: true; visible: false } // Invisible option
	CheckBox { name: "dichotomous"; checked: false; visible: false } // Invisible option
	POLY.VariablesListPolytomous { id: vars }
	POLY.IrtInputPolytomous { id: model }
	COMMON.IrtOutput { bayesian: true; dichotomous: false }
	COMMON.ItemInfoCurves { }
	COMMON.ItemCharCurves { dichotomous: false }
	BAYES.AdvancedOptions { ncovs: vars.ncovs; modeltype: model.value }
}
