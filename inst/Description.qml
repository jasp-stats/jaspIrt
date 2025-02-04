import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name			: "jaspIrt"
	title			: qsTr("Item Response Theory")
	description		: qsTr("Analyze psychometric tests using Item Response Theory (IRT).")
	icon			: "irt-classical"
	requiresData	: true
	version			: "0.18.1"
	author			: "Koen Derks"
	maintainer		: "Koen Derks <k.derks@nyenrode.nl>"
	website			: "https://github.com/jasp-stats/jaspIrt"
	license			: "GPL (>= 3)"
	hasWrappers		: false
	preloadData		: true

	GroupTitle
	{
		title:		qsTr("Classical")
		icon:		"irt-classical.svg"
	}
	Analysis
	{
		menu:		qsTr("Dichotomous Items")
		title:		qsTr("Dichotomous Item Response Theory")
		func:		"itemResponseTheoryDichotomous"
	}
	Analysis
	{
		menu:		qsTr("Polytomous Items")
		title:		qsTr("Polytomous Item Response Theory")
		func:		"itemResponseTheoryPolytomous"
	}

	GroupTitle
	{
		title:		qsTr("Bayesian")
		icon:		"irt-bayesian.svg"
	}
	Analysis
	{
		menu:		qsTr("Dichotomous Items")
		title:		qsTr("Bayesian Dichotomous Item Response Theory")
		func:		"itemResponseTheoryDichotomousBayesian"
	}
	Analysis
	{
		menu:		qsTr("Polytomous Items")
		title:		qsTr("Bayesian Polytomous Item Response Theory")
		func:		"itemResponseTheoryPolytomousBayesian"
	}
}
