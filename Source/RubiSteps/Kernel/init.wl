(* ::Package:: *)

RubiSteps`GeneralMessage`Loader::RubiLoadFailMsg = RubiSteps`Message`Loader::RubiLoadFailMsg::English =
	"Cannot load Rubi`. RubiSteps` modifies Rubi`, so it's essential to be loadable when loading RubiSteps`.";


RubiSteps`GeneralMessage`General::Failure = "``";


WithCleanup[SetDirectory@DirectoryName@$InputFileName,
	Enclose[
		(
			Confirm[Needs@"Rubi`", RubiSteps`GeneralMessage`Loader::RubiLoadFailMsg];
			Get@"RubiSteps.wl"
		),
		Message[RubiSteps`GeneralMessage`General::Failure, #]&
	]
, ResetDirectory[]]
