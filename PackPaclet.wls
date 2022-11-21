#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory@If[$Notebooks, NotebookDirectory[], Directory[]]


paclet = CreatePacletArchive["RubiSteps", "Paclets"]


PacletManager`BuildPacletSiteFiles@Directory[]


PacletInstall["Paclets/RubiSteps-0.0.1.paclet", ForceVersionInstall -> True]


<< RubiSteps`


ResetDirectory[]
