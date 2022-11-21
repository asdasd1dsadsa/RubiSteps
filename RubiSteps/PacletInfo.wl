(* ::Package:: *)

Paclet[
	Name -> "RubiSteps",
	Version -> "0.0.1",(* API change and paclet-ize *)
	WolframVersion -> "12.2+",(* Enclose is introduced in 12.2, although not necessary. *)
	Description -> "Enhancement for Rubi steps.",
	Root -> ".",
	Loading -> Manual,
	Extensions -> {
		{
			"Kernel",
			Root -> ".",
			Context -> "RubiSteps`"
		}
	}
]
