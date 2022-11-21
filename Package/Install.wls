#!/usr/bin/env wolframscript
(* ::Package:: *)

CopyFile[
	FileNameJoin@{If[$Notebooks, NotebookDirectory[], Directory[]], "RubiSteps.wl"},
	FileNameJoin@{$UserBaseDirectory, "Applications", "RubiSteps.wl"}
, OverwriteTarget -> True]
