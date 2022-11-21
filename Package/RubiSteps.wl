(* ::Package:: *)

(* ::Title:: *)
(*RubiSteps*)


(* ::Chapter:: *)
(*Begin*)


BeginPackage["RubiSteps`", {"Rubi`"}]


ClearAll@"`*"


ShowIntSteps
RiffleTeXForm


SubstitutionInformation


Begin["`Private`"]


ClearAll@"`*"


(* ::Chapter:: *)
(*Definition*)


ShowIntSteps::usage = "ShowIntSteps[Int[\!\(\*StyleBox[\"expr\", \"TI\"]\), \!\(\*StyleBox[\"var\", \"TI\"]\)], \!\(\*StyleBox[\"opts\", \"TI\"]\)]\n\rUse ShowIntSteps[\"OptionList\"] to see the list of option keys and values."


RiffleTeXForm::usage = "RiffleTeXForm[\!\(\*StyleBox[\"header\", \"TI\"]\)][\!\(\*StyleBox[\"list\", \"TI\"]\)] constructs an aligned equation."


SubstitutionInformation::usage = "SubstitutionInformation represents substitution rules for expressions. SubstitutionInformation is also an option of ShowIntSteps."


(* ::Section:: *)
(*ShowIntSteps*)


(* ::Subsection:: *)
(*Evaluation*)


evaluateOnce = ReleaseHold@ReplaceAll[HoldComplete@#, Defer[h:Rubi`Int|Rubi`Subst|Rubi`Dist] :> h] &;


sowAndReturn = (
	Sow[evaluateOnce@HoldForm@#, RubiStepTag];
	evaluateOnce@#
) &;


(* ::Text:: *)
(*Create new symbols as substituted variables and forbid substitutions to be performed.*)


ForbidSubstitution[newVariableGenerator_][input_] := 
	input //. Defer[Rubi`Subst][expr_, src_, tar_] :> (NewSubstitution[expr /. src -> #, #, tar]&@newVariableGenerator@src) //With[
	{
		equivalentClasses = Map[First, GatherBy[
			Cases[input, NewSubstitution[_, src_, tar_] :> {src, tar}, Infinity]
		, Last], {2}]
	},
	# /. Catenate[Thread[Rest@#->First@#] & /@ equivalentClasses]
] &


(* ::Text:: *)
(*For "SubstitutionInformation" -> Automatic , only new substitutions will be displayed.*)


FromNewToOld[input_] := input /. NewSubstitution -> OldSubstitution


PerformSubstitution[input_] := input //.
	(NewSubstitution|OldSubstitution)[expr_?(FreeQ[NewSubstitution|OldSubstitution]), src_, tar_] :> (expr /. src -> tar)
(* The `FreeQ` works for `GeneratedParameters -> Identity`. *)


(* ::Subsection:: *)
(*Output Conversion*)


CatenateReapWithSimplify[compfunc_][{result_, sow_}] := Flatten[sow, 1] // Append[
	If[compfunc === None,
		result,
		Quiet[
			Simplify[result, ComplexityFunction -> compfunc, TimeConstraint -> 10]
		, Simplify::time]
	] // HoldForm@#& (* This `HoldForm` works for `DeleteDuplicates` *)
]


ReplaceGeneratedSymbol::toomuchvar = "Method \"SingleLetter\" does not have enough free single-letter symbols as `1` symbols are needed. Original unique symbols is returned instead.";


ReplaceGeneratedSymbol[input_, toBeReplace_, appearedSymbols_, method_] := Switch[method,
	Automatic|"SingleLetter",
		With[{
			generatedParameters = Quiet[
				Take[#, Length@toBeReplace]&@Cases[Symbol /@ #, _Symbol]&@Complement[
					CharacterRange["a", "z"],
					SymbolName/@appearedSymbols,
					{"d", "e", "i", "j", "o"} (* Escape \[DifferentialD], \[ExponentialE], \[ImaginaryI], \[ImaginaryJ], O *)
				]
			, Take::take]
		}, If[ListQ@generatedParameters,
			input /. MapThread[Rule, {toBeReplace, generatedParameters}],
			Message[ReplaceGeneratedSymbol::toomuchvar, Length@toBeReplace];input
		] ],
	Identity|"Identity"|None|False,
		input,
	Unique|"Unique",
		ReplaceAll[input,
			s_Symbol :> RuleCondition[(* Undocumented function `RuleCondition` used for invoking evaluation before replacing*)
				If[Length@#===1,
					s,
					Subscript[
						ToExpression@StringRiffle[Most@#, "$"],
						ToExpression@Last@#
					]
				] &@StringSplit[SymbolName@s, "$"]
			]
		],
	_,
		input /. MapThread[Rule, {toBeReplace, Array[method, Length@toBeReplace]}]
]


ExtractSubstitutionInformation[steps_List] := With[{substRules = Cases[#, NewSubstitution[_, src_, tar_] :> {src, tar}, Infinity]},
	SubstitutionInformation[# //. (NewSubstitution|OldSubstitution)[expr_, _, _] :> expr, substRules]
] & /@ steps


RiffleTeXForm[header_][steps_] := StringRiffle[{"\\begin{aligned}",
	Convert`TeX`ExpressionToTeX@header,
	Sequence@@(StringJoin["=&", Convert`TeX`ExpressionToTeX@#, "\\\\"]& /@ steps),
	"\\end{aligned}"
}, "\n"]


(* ::Subsection:: *)
(*Output Display*)


replaceDistAndInt = # /. {Rubi`Dist -> Times, Rubi`Int -> Integrate} &;


$SubstitutionInformationSeperator = {",", "  "};

SubstitutionInformation /: MakeBoxes[SubstitutionInformation[expr_, {}], form_] := MakeBoxes[expr, form]

SubstitutionInformation /: MakeBoxes[SubstitutionInformation[expr_, rules:{{_,_}..}], form_] := With[
	{
		info = RowBox@Riffle[MakeBoxes[#, form]&/@#, "\[LongEqual]"]&/@rules //Riffle[#, Unevaluated@Sequence[",", " "]]&
	},
	FormBox[
		RowBox@{MakeBoxes[expr, form], Sequence@@$SubstitutionInformationSeperator, RowBox@info}
	, form]
]


(* ::Subsection:: *)
(*Main Function*)


ShowIntSteps::nogenvar = "While turning off variable generation is needed, FormatType -> InputForm and \"SubstitutionInformation\" -> None is essential. Or mathematically wrong expressions will be returned.";


SetAttributes[ShowIntSteps, HoldFirst]

ShowIntSteps[Rubi`Int[expr_, var_Symbol], OptionsPattern[]] := Module[{
	generatedUniqueSymbols = {}
}, With[{
	symbolGenerationMethod = Switch[OptionValue/@{GeneratedParameters, FormatType, SubstitutionInformation},
		{Identity|"Identity"|None|False, InputForm|"Original"|None|False, None|False},
			Identity,
		{Identity|"Identity"|None|False, __},
			Message[ShowIntSteps::nogenvar];Identity, (* Force it not to be `Identity`, or let it be? *)
		_,
			OptionValue@GeneratedParameters
	]
}, With[{
	newVariableGenerator = Switch[symbolGenerationMethod,
		Identity|"Identity"|None|False,
			Identity,
		Unique|"Unique",
			(AppendTo[generatedUniqueSymbols, #];#)&@Unique[var] &,
		_, (* Includes `Automatic|"SingleLetter"` *)
			(AppendTo[generatedUniqueSymbols, #];#)&@Unique[tmp] &
	]
}, Block[{Rubi`Private`$ShowSteps = True},
	(* Evaluation *)
	Reap[
		FixedPoint[ (* Do not perform substitution until there's no other evaluation can be performed. *)
			FixedPoint[
				sowAndReturn@*Switch[OptionValue@SubstitutionInformation,
					True|All, ForbidSubstitution[newVariableGenerator],
					False|None, FromNewToOld@*ForbidSubstitution[newVariableGenerator],
					_, ForbidSubstitution[newVariableGenerator]@*FromNewToOld (* Includes `Automatic` *)
				],
				#
			] & /* PerformSubstitution,
			Rubi`Int[expr, var]
		]
	, RubiStepTag] //CatenateReapWithSimplify[OptionValue@ComplexityFunction]
] // (* ReplaceGeneratedSymbol *) Function[{input},
	With[{appearedSymbols = Append[var]@Cases[input, _Symbol, Infinity, Heads -> True]},
		ReplaceGeneratedSymbol[input, Intersection[generatedUniqueSymbols, appearedSymbols], appearedSymbols, symbolGenerationMethod]
	]
] // (* Convert to some form *) Switch[OptionValue@FormatType,
	InputForm|"Original"|None|False,
		DeleteDuplicates@ReplaceAll[#, NewSubstitution|OldSubstitution :> Rubi`Subst] &,
	StandardForm|"Replaced"|Automatic|True,
		replaceDistAndInt@*DeleteDuplicates@*ExtractSubstitutionInformation,
	TraditionalForm|"Traditional"|"TraditionalForm",
		TraditionalForm /@ DeleteDuplicates@ExtractSubstitutionInformation@# &,
	TeXForm|"TeX"|"TeXForm",
		RiffleTeXForm[HoldForm@Rubi`Int[expr, var]]@*DeleteDuplicates@*ExtractSubstitutionInformation
]
] ] ]

Options[ShowIntSteps] = {
	ComplexityFunction -> StringLength@*ToString,
	GeneratedParameters -> Automatic,
	SubstitutionInformation -> Automatic,
	FormatType -> StandardForm
};


(* ::Text:: *)
(*Provide option list.*)


ShowIntSteps["OptionList"] := {
	FormatType -> InputForm|StandardForm|TraditionalForm|TeXForm,
	GeneratedParameters -> Automatic|Unique|Identity|_,
	SubstitutionInformation -> All|Automatic|None,
	ComplexityFunction -> None|_
}


(* ::Chapter:: *)
(*End*)


End[]


EndPackage[]


(* ::Chapter:: *)
(*Modification*)


#=.&@*First /@ FormatValues@Rubi`Dist;
MakeBoxes[HoldPattern@Rubi`Dist[l_, int_Rubi`Int, _], form:TraditionalForm] ^:= RowBox@{MakeBoxes[l, form], MakeBoxes[int, form]};
MakeBoxes[HoldPattern@Rubi`Dist[l_, r_, _], form_] ^:= MakeBoxes[Times[l, r], form]


#=.&@*First /@ FormatValues@Rubi`Subst;
MakeBoxes[HoldPattern@Rubi`Subst[expr_, key_, val_], form:TraditionalForm] ^:= RowBox@{
	UnderscriptBox["Subs", 
		RowBox@{
			MakeBoxes[key, form], "\[RightArrow]", MakeBoxes[val, form]
		}
	],
	RowBox@{
		"(", MakeBoxes[expr, form], ")"
	}
}
