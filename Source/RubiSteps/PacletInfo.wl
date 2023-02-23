(* ::Package:: *)

Paclet[
	Name -> "RubiSteps",
	Version -> "0.0.3",(* Fix a fatal bracket bug *)
	WolframVersion -> "12.2+",(* Enclose is introduced in 12.2, although not necessary. *)
	Description -> "Enhancement for Rubi steps.",
	Creator -> "asdasd1dsadsa",
	URL -> "https://github.com/asdasd1dsadsa/RubiSteps",
	Root -> ".",
	Loading -> Manual,
	Extensions -> {
		{
			"Documentation",
			"Root" -> "Documentation",
			"Language" -> All,
			MainPage -> "Guides/RubiSteps"
		},
		{
			"Kernel",
			Root -> "Kernel",
			Context -> "RubiSteps`"
		}
	}
]
