#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory@If[$Notebooks, NotebookDirectory[], Directory[]]


Get@"DocumentationTools`"


Get@"PacletTools`"


SetOptions[DocumentationBuild`DocumentationBuildNotebooks, "BuildSearchIndex" -> False]


PacletBuild["Source/RubiSteps", "Build/"]


SetOptions[DocumentationBuild`DocumentationBuildNotebooks, "BuildSearchIndex" -> True]


build = PacletBuild["Source/RubiSteps", "Build/"]


oldStyle = Options[$FrontEnd, DefaultStyleDefinitions]
SetOptions[$FrontEnd, DefaultStyleDefinitions -> "Default.nb"]


PacletDocumentationBuild["Source/RubiSteps", "docs/", "HTML"]


SetOptions[$FrontEnd, oldStyle]


paclet = RenameFile[#, FileNameJoin@{"Paclets", FileNameTake@#}, OverwriteTarget -> True]&@build["PacletArchive"]


(*paclet = CreatePacletArchive["RubiSteps", "Paclets"]*)


PacletManager`BuildPacletSiteFiles@Directory[]


PacletInstall[paclet, ForceVersionInstall -> True]


(* ::Text:: *)
(*RubiSteps/ref/ShowIntSteps*)


<< RubiSteps`


ResetDirectory[]
