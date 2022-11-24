#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory@If[$Notebooks, NotebookDirectory[], Directory[]]


Get@"DocumentationTools`"


Get@"PacletTools`"


SetOptions[DocumentationBuild`DocumentationBuildNotebooks, "BuildSearchIndex" -> False]


PacletBuild["Source/RubiSteps", "Build/"]


SetOptions[DocumentationBuild`DocumentationBuildNotebooks, "BuildSearchIndex" -> True]


build = PacletBuild["Source/RubiSteps", "Build/"]


PacletDocumentationBuild["Source/RubiSteps", "Build/HTML/", "HTML"]


paclet = RenameFile[#, FileNameJoin@{"Paclets", FileNameTake@#}, OverwriteTarget -> True]&@build["PacletArchive"]


(*paclet = CreatePacletArchive["RubiSteps", "Paclets"]*)


PacletManager`BuildPacletSiteFiles@Directory[]


PacletInstall[paclet, ForceVersionInstall -> True]


(* ::Text:: *)
(*RubiSteps/ref/ShowIntSteps*)


<< RubiSteps`


ResetDirectory[]
