BeginPackage["Spotlight`"];

MDFind::usage                 = "";
OnlyInDirectory::usage        = "";
DefaultOnlyInDirectory::usage = "";

Begin["`Private`"];

MDFind::arg = "Option value `1` for OnlyInDirectory must be either a String or Automatic.";

Options[MDFind] = {OnlyInDirectory :> DefaultOnlyInDirectory};

DefaultOnlyInDirectory = Automatic;

MDFind[key_String, value_String, opts : OptionsPattern[]] :=
Module[{command, query, onlyInDirectory},
	command         = "!mdfind";
	query           = "\"" <> key <> " == '" <> value <> "'\"";
	onlyInDirectory = OptionValue[OnlyInDirectory];
	If[!SameQ[Automatic, onlyInDirectory],
		If[!SameQ[String, Head[onlyInDirectory]],
			Message[MDFind::arg, onlyInDirectory]
		];
		command = command <> " -onlyin \"" <> ExpandFileName[onlyInDirectory] <> "\"";
	];
   command = command <> " " <> query;
   Print[command];
   ReadList[command, Record]
];

End[];

EndPackage[];
