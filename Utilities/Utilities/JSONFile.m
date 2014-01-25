(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["Utilities`JSONFile`",
	{
		"Utilities`TextFile`"
	}
]

JSONFile::usage  = "";
JSONFileQ::usage = "";

JSONData::usage  = "";
JSONDataQ::usage = "";

BooleanQ::usage  = "";


Begin["`Private`"]
Needs["Calendar`"];
Needs["Utilities`Logging`"];

BooleanQ[___] := False;
BooleanQ[True] = BooleanQ[False] = True;

JSONDataPattern = PatternSequence[_String -> _]..;
Protect[JSONDataPattern];

JSONDataQ[___]                                  := False;
JSONDataQ[object:List[pattern:JSONDataPattern]] :=
JSONDataQ[object                              ]  = True;

JSONFilePattern =
PatternSequence[
	FilePath      -> _String,
	FileDate      -> _?DateQ,
	FileByteCount -> _Integer,
	FileHash      -> _Integer,
	JSONData      -> Automatic | _?JSONDataQ
];
Protect[JSONFilePattern];

JSONFileImportedPattern =
PatternSequence[
	FilePath      -> _String,
	FileDate      -> _?DateQ,
	FileByteCount -> _Integer,
	FileHash      -> _Integer,
	JSONData      -> _List
];
Protect[JSONFileImportedPattern];

JSONFileQ[___]                                      := False;
JSONFileQ[object:JSONFile[pattern:JSONFilePattern]] :=
JSONFileQ[object                                  ]  = True;

JSONFileImportedQ[___]                                              := False;
JSONFileImportedQ[object:JSONFile[pattern:JSONFileImportedPattern]] :=
JSONFileImportedQ[object                                          ]  = True;

JSONFile::import = "`1`: Import File `2`.";
Off[JSONFile::import];

JSONFile::cache = "`1`: Cache hit for JSONFile `2`.";
Off[JSONFile::cache];

JSONFile[filePath_String /; FileExistsQ[filePath]] :=
JSONFile[filePath                                ]  =
JSONFile[
	FilePath      ->               filePath,
	FileDate      ->      FileDate[filePath],
	FileByteCount -> FileByteCount[filePath],
	FileHash      ->      FileHash[filePath],
	JSONData      -> Automatic
];

JSONFile /:      FilePath[object:JSONFile[pattern:JSONFilePattern]] := FilePath[object] = FilePath      /. {pattern};
JSONFile /:      FileDate[object:JSONFile[pattern:JSONFilePattern]] :=                    FileDate      /. {pattern};
JSONFile /: FileByteCount[object:JSONFile[pattern:JSONFilePattern]] :=                    FileByteCount /. {pattern};
JSONFile /:      FileHash[object:JSONFile[pattern:JSONFilePattern]] :=                    FileHash      /. {pattern};
JSONFile /:      JSONData[object:JSONFile[pattern:JSONFilePattern]] :=
Module[{filePath, jsonData},
	filePath = FilePath[object];
	
	If[
		JSONFileImportedQ[JSONFile[filePath]],
		
		(* True *)
		DebugMessage[JSONFile::cache, filePath];
		jsonData = JSONData /. List @@ JSONFile[filePath],

		(* False *)
		DebugMessage[JSONFile::import, filePath];
		jsonData = Import[filePath, {"JSON", "Data"}];
	
		JSONFile[filePath] =
		JSONFile[
			FilePath      ->               filePath,
			FileDate      ->      FileDate[filePath],
			FileByteCount -> FileByteCount[filePath],
			FileHash      ->      FileHash[filePath],
			JSONData      -> jsonData
		];
	];
	
	JSONData[object] = jsonData
];


End[]

EndPackage[]
