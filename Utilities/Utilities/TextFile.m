(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["Utilities`TextFile`"]

TextFile::usage           = "";
TextFileQ::usage          = "";
FilePath::usage           = "";
Lines::usage              = "";

TextFileLine::usage       = "";
TextFileLineQ::usage      = "";
LineNumber::usage         = "";
LineText::usage           = "";

SurroundingContext::usage = "";

FileQ::usage              = "";
FileLineCount::usage      = "";


Begin["`Private`"]
Needs["Calendar`"];
Needs["Utilities`Logging`"];

FileQ[___]             := False;
FileQ[filePath_String] := FileExistsQ[filePath] && MatchQ[FileType[filePath], File];

TextFile::fileNotFound = "File does not exist at path `1`.";

FileLineCount[filePath_ /; FileQ[filePath] || Message[TextFile::fileNotFound, filePath]] := Length[Import[filePath, {"Text", "Lines"}]];


TextFileLinePattern =
PatternSequence[
	FilePath   -> _String,
	LineNumber -> _Integer,
	LineText   -> _String
];
Protect[TextFileLinePattern];

TextFileLineQ[___]                                              := False;
TextFileLineQ[object:TextFileLine[pattern:TextFileLinePattern]] :=
TextFileLineQ[object                                          ]  = True;

TextFileLine[
	filePath_String    /; FileQ[filePath],
	lineNumber_Integer /; NonNegative[lineNumber],
	lineText_String
] :=
TextFileLine[
	filePath,
	lineNumber,
	lineText
] =
TextFileLine[
	FilePath   -> filePath,
	LineNumber -> lineNumber,
	LineText   -> lineText
];

TextFileLine /:   TextFile[object:TextFileLine[pattern:TextFileLinePattern]] := TextFile[FilePath[object]];
TextFileLine /:   FilePath[object:TextFileLine[pattern:TextFileLinePattern]] := FilePath   /. {pattern};
TextFileLine /: LineNumber[object:TextFileLine[pattern:TextFileLinePattern]] := LineNumber /. {pattern};
TextFileLine /:   LineText[object:TextFileLine[pattern:TextFileLinePattern]] := LineText   /. {pattern};


TextFilePattern =
PatternSequence[
	FilePath      -> _String,
	FileExtension -> _String,
	FileDate      -> _?DateQ,
	FileLineCount -> _Integer,
	FileByteCount -> _Integer,
	FileHash      -> _Integer,
	Lines         -> Automatic | _List
];
Protect[TextFilePattern];

TextFileImportedPattern =
PatternSequence[
	FilePath      -> _String,
	FileExtension -> _String,
	FileDate      -> _?DateQ,
	FileLineCount -> _Integer,
	FileByteCount -> _Integer,
	FileHash      -> _Integer,
	Lines         -> _List
];
Protect[TextFileImportedPattern];

TextFileQ[___]                                      := False;
TextFileQ[object:TextFile[pattern:TextFilePattern]] :=
TextFileQ[object                                  ]  = True;

TextFileImportedQ[___]                                              := False;
TextFileImportedQ[object:TextFile[pattern:TextFileImportedPattern]] :=
TextFileImportedQ[object                                          ]  = True;

TextFile::import = "`1`: Import File `2`.";
Off[TextFile::import];

TextFile::cache = "`1`: Cache hit for TextFile `2`.";
Off[TextFile::cache];

TextFile[filePath_String /; FileQ[filePath]] :=
TextFile[filePath                          ]  =
TextFile[
	FilePath      ->               filePath,
	FileExtension -> FileExtension[filePath],
	FileDate      ->      FileDate[filePath],
	FileLineCount -> FileLineCount[filePath],
	FileByteCount -> FileByteCount[filePath],
	FileHash      ->      FileHash[filePath],
	Lines         -> Automatic
];

TextFile /:      FilePath[object:TextFile[pattern:TextFilePattern]] := FilePath      /. {pattern};
TextFile /: FileExtension[object:TextFile[pattern:TextFilePattern]] := FileExtension /. {pattern};
TextFile /:      FileDate[object:TextFile[pattern:TextFilePattern]] := FileDate      /. {pattern};
TextFile /: FileLineCount[object:TextFile[pattern:TextFilePattern]] := FileLineCount /. {pattern};
TextFile /: FileByteCount[object:TextFile[pattern:TextFilePattern]] := FileByteCount /. {pattern};
TextFile /:      FileHash[object:TextFile[pattern:TextFilePattern]] := FileHash      /. {pattern};
TextFile /:         Lines[object:TextFile[pattern:TextFilePattern]] :=
Module[{filePath, lines},
	filePath = FilePath[object];
	
	If[
		TextFileImportedQ[TextFile[filePath]],
		
		(* True *)
		DebugMessage[TextFile::cache, filePath];
		lines = Lines /. List @@ TextFile[filePath],

		(* False *)
		DebugMessage[TextFile::import, filePath];
		lines = Import[filePath, {"Text", "Lines"}];
		lines = Transpose[{Range[Length[lines]], lines}] /. {lineNumber_Integer, text_String} :> TextFileLine[filePath, lineNumber, text];
	
		TextFile[filePath] =
		TextFile[
			FilePath      ->               filePath,
			FileExtension -> FileExtension[filePath],
			FileDate      ->      FileDate[filePath],
			FileLineCount -> FileLineCount[filePath],
			FileByteCount -> FileByteCount[filePath],
			FileHash      ->      FileHash[filePath],
			Lines         -> lines
		];
	];
	
	Lines[object] = lines
];


SurroundingContext[textFileLine_?TextFileLineQ, lineCount_Integer] :=
Module[{lineNumber, textFile, lineNumberFirst, lineNumberLast},
	lineNumber = LineNumber[textFileLine];
	If[
		lineCount > 0,
		lineNumberFirst = lineNumber + 1;
		lineNumberLast  = lineNumberFirst + lineCount - 1,
		If[
			lineCount < 0,
			lineNumberFirst = lineNumber + lineCount;
			lineNumberLast  = lineNumberFirst + Abs[lineCount] - 1,
			lineNumberFirst = lineNumber;
			lineNumberLast  = lineNumber
		]
	];
	
	textFile = TextFile[FilePath[textFileLine]];
	
	(*Take[Select[Lines[textFile], LineNumber[#] >= lineNumberFirst &], Abs[lineCount]]*)

	SurroundingContext[textFileLine, lineCount] =
	Select[Lines[textFile], lineNumberFirst <= LineNumber[#] <= lineNumberLast &]
];


End[]

EndPackage[]
