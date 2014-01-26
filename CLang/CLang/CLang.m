(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["CLang`",
	{
		"Utilities`TextFile`",
		"Utilities`TimeInterval`"
	}
]

(* Debug: Begin *)
HeaderIncludeLineList::usage             = "";
HeaderIncludeDataList::usage             = "";

HeaderIncludeUserPattern::usage          = "";
HeaderIncludeSystemPattern::usage        = "";
HeaderIncludeLineDirectivePattern::usage = "";

HeaderIncludeLineUserPattern::usage      = "";
HeaderIncludeLineSystemPattern::usage    = "";
HeaderIncludeLinePattern::usage          = "";
(* Debug: End *)

SourceFile::usage                        = "";
SourceFileQ::usage                       = "";
HeaderIncludeList::usage                 = "";
HeaderIncludeListUser::usage             = "";
HeaderIncludeListSystem::usage           = "";
HeaderFileQ::usage                       = "";
CSourceFileQ::usage                      = "";
CPlusPlusSourceFileQ::usage              = "";
ObjectiveCSourceFileQ::usage             = "";
ObjectiveCPlusPlusSourceFileQ::usage     = "";
ImplementationFileQ::usage               = "";

SourceFileLine::usage                    = "";
SourceFileLineQ::usage                   = "";

(*
HeaderFile::usage                        = "";
HeaderFileQ::usage                       = "";
*)

HeaderFilePathQ::usage                   = "";
CSourceFilePathQ::usage                  = "";
CPlusPlusSourceFilePathQ::usage          = "";
ObjectiveCSourceFilePathQ::usage         = "";
ObjectiveCPlusPlusSourceFilePathQ::usage = "";
ImplementationFilePathQ::usage           = "";
SourceFilePathQ::usage                   = "";

HeaderIncludeLineQ::usage                = "";
HeaderIncludeLineUserQ::usage            = "";
HeaderIncludeLineSystemQ::usage          = "";
HeaderIncludeUserQ::usage                = "";
HeaderIncludeSystemQ::usage              = "";

(*
HeaderIncludeLine::usage                 = "";
HeaderIncludeLineUser::usage             = "";
HeaderIncludeLineSystem::usage           = "";
*)

(*
HeaderIncludeList::usage                 = "";
HeaderIncludePathList::usage             = "";
*)

Begin["`Private`"]
Needs["Calendar`"];

(* Predicates: SourceFile type based on file extension. *)

HeaderFilePathQ[___]                                                 := False;
HeaderFilePathQ[filePath_String]                                     := StringMatchQ[FileExtension[filePath]  , "h"];
(*
HeaderFilePathQ[textFile_TextFile]                                   := StringMatchQ[FileExtension[textFile]  , "h"];
HeaderFilePathQ[filePath_?FileQ]                                     := StringMatchQ[FileExtension[filePath]  , "h"];
HeaderFilePathQ[sourceFile_?SourceFileQ]                             := StringMatchQ[FileExtension[sourceFile], "h"];
*)

CSourceFilePathQ[___]                                                := False;
CSourceFilePathQ[filePath_String]                                    := StringMatchQ[FileExtension[filePath]  , "c"];
(*
CSourceFilePathQ[textFile_TextFile]                                  := StringMatchQ[FileExtension[textFile]  , "c"];
CSourceFilePathQ[filePath_?FileQ]                                    := StringMatchQ[FileExtension[filePath]  , "c"];
CSourceFilePathQ[sourceFile_?SourceFileQ]                            := StringMatchQ[FileExtension[sourceFile], "c"];
*)

CPlusPlusSourceFilePathQ[___]                                        := False;
CPlusPlusSourceFilePathQ[filePath_String]                            := StringMatchQ[FileExtension[filePath]  , "cpp"];
(*
CPlusPlusSourceFilePathQ[textFile_TextFile]                          := StringMatchQ[FileExtension[textFile]  , "cpp"];
CPlusPlusSourceFilePathQ[filePath_?FileQ]                            := StringMatchQ[FileExtension[filePath]  , "cpp"];
CPlusPlusSourceFilePathQ[sourceFile_?SourceFileQ]                    := StringMatchQ[FileExtension[sourceFile], "cpp"];
*)

ObjectiveCSourceFilePathQ[___]                                       := False;
ObjectiveCSourceFilePathQ[filePath_String]                           := StringMatchQ[FileExtension[filePath]  , "m"];
(*
ObjectiveCSourceFilePathQ[textFile_TextFile]                         := StringMatchQ[FileExtension[textFile]  , "m"];
ObjectiveCSourceFilePathQ[filePath_?FileQ]                           := StringMatchQ[FileExtension[filePath]  , "m"];
ObjectiveCSourceFilePathQ[sourceFile_?SourceFileQ]                   := StringMatchQ[FileExtension[sourceFile], "m"];
*)

ObjectiveCPlusPlusSourceFilePathQ[___]                               := False;
ObjectiveCPlusPlusSourceFilePathQ[filePath_String]                   := StringMatchQ[FileExtension[filePath]  , "mm"];
(*
ObjectiveCPlusPlusSourceFilePathQ[textFile_TextFile]                 := StringMatchQ[FileExtension[textFile]  , "mm"];
ObjectiveCPlusPlusSourceFilePathQ[filePath_?FileQ]                   := StringMatchQ[FileExtension[filePath]  , "mm"];
ObjectiveCPlusPlusSourceFilePathQ[sourceFile_?SourceFileQ]           := StringMatchQ[FileExtension[sourceFile], "mm"];
*)

ImplementationFilePathQ[___]                                         := False;
ImplementationFilePathQ[filePath_String]                             := CSourceFilePathQ[filePath] || CPlusPlusSourceFilePathQ[filePath] || ObjectiveCSourceFilePathQ[filePath] || ObjectiveCPlusPlusSourceFilePathQ[filePath];

SourceFilePathQ[___]                                                 := False;
SourceFilePathQ[filePath_?HeaderFilePathQ]                           := True;
SourceFilePathQ[filePath_?ImplementationFilePathQ]                   := True;

(* Patterns: Header File properties. *)

HeaderIncludeLineDirectivePattern = "#" ~~ ("include" | "import");
HeaderIncludeUserPattern          = "\"" ~~ headerIncludeUserFilePath   : Except["\""] .. ~~ "\"";
HeaderIncludeSystemPattern        = "<"  ~~ headerIncludeSystemFilePath : Except[">"]  .. ~~ ">";

HeaderIncludeLineUserPattern      = WhitespaceCharacter... ~~ HeaderIncludeLineDirectivePattern ~~ Whitespace ~~ HeaderIncludeUserPattern   ~~ ___;
HeaderIncludeLineSystemPattern    = WhitespaceCharacter... ~~ HeaderIncludeLineDirectivePattern ~~ Whitespace ~~ HeaderIncludeSystemPattern ~~ ___;
HeaderIncludeLinePattern          = WhitespaceCharacter... ~~ HeaderIncludeLineDirectivePattern ~~ Whitespace ~~ (HeaderIncludeUserPattern | HeaderIncludeSystemPattern) ~~ ___;

Protect[HeaderIncludeLineDirectivePattern];
Protect[HeaderIncludeUserPattern];
Protect[HeaderIncludeSystemPattern];

Protect[HeaderIncludeLineUserPattern];
Protect[HeaderIncludeLineSystemPattern];
Protect[HeaderIncludeLinePattern];

(* Predicates: Header File properties. *)

HeaderIncludeLineQ[___]                                                    := False;
HeaderIncludeLineQ[string_String]                                          := HeaderIncludeLineUserQ[string] || HeaderIncludeLineSystemQ[string];
HeaderIncludeLineQ[sourceFileLine_SourceFileLine]                          := HeaderIncludeLineQ[LineText[sourceFileLine]];
(*
HeaderIncludeLineQ[headerIncludeLine_?HeaderIncludeLineUserQ]              := True;
HeaderIncludeLineQ[headerIncludeLine_?HeaderIncludeLineSystemQ]            := True;
*)

HeaderIncludeLineUserQ[___]                                                := False;
HeaderIncludeLineUserQ[string_String]                                      := StringMatchQ[string, HeaderIncludeLineUserPattern];
HeaderIncludeLineUserQ[sourceFileLine_SourceFileLine]                      := HeaderIncludeLineUserQ[LineText[sourceFileLine]];
(*
HeaderIncludeLineUserQ[textFileLine_?TextFileLineQ]                        := HeaderIncludeLineUserQ[LineText[textFileLine]];
*)

HeaderIncludeLineSystemQ[___]                                              := False;
HeaderIncludeLineSystemQ[string_String]                                    := StringMatchQ[string, HeaderIncludeLineSystemPattern];
HeaderIncludeLineSystemQ[sourceFileLine_SourceFileLine]                    := HeaderIncludeLineSystemQ[LineText[sourceFileLine]];
(*
HeaderIncludeLineSystemQ[textFileLine_?TextFileLineQ]                      := HeaderIncludeLineSystemQ[LineText[textFileLine]];
*)

HeaderIncludeUserQ[___]                                                    := False;
HeaderIncludeUserQ[string_String]                                          := StringMatchQ[string, HeaderIncludeUserPattern];

HeaderIncludeSystemQ[___]                                                  := False;
HeaderIncludeSystemQ[string_String]                                        := StringMatchQ[string, HeaderIncludeSystemPattern];

(* Functions: Header file properties *)

HeaderIncludeLine[headerIncludeLine_?HeaderIncludeLineUserQ]               := HeaderIncludeLineUser[headerIncludeLine];
HeaderIncludeLine[headerIncludeLine_?HeaderIncludeLineSystemQ]             := HeaderIncludeLineSystem[headerIncludeLine];

HeaderIncludeLineUser[headerIncludeLineUser_?HeaderIncludeLineUserQ]       := Flatten[StringCases[headerIncludeLineUser, HeaderIncludeLineUserPattern :> {headerIncludeUserFilePath, "User"}], 1];
(*
HeaderIncludeLineUser[textFileLine_?TextFileLineQ]                         := HeaderIncludeLineUser[LineText[textFileLine]];
HeaderIncludeLineUser[sourceFileLine_?SourceFileLineQ]                     := HeaderIncludeLineUser[LineText[sourceFileLine]];
*)

HeaderIncludeLineSystem[headerIncludeLineSystem_?HeaderIncludeLineSystemQ] := Flatten[StringCases[headerIncludeLineSystem, HeaderIncludeLineSystemPattern :> {headerIncludeSystemFilePath, "System"}], 1];
(*
HeaderIncludeLineSystem[textFileLine_?TextFileLineQ]                       := HeaderIncludeLineSystem[LineText[textFileLine]];
HeaderIncludeLineSystem[sourceFileLine_?SourceFileLineQ]                   := HeaderIncludeLineSystem[LineText[sourceFileLine]];
*)

(*
HeaderIncludeList[textFile_?TextFileQ]                                     := Select[Lines[textFile], HeaderIncludeLineQ];
HeaderIncludePathList[textFile_?TextFileQ]                                 := Map[HeaderIncludeLine, HeaderIncludeList[textFile]];
*)

(* SourceFileLine *)

SourceFileLineSourceQ[___]                       := False;
SourceFileLineSourceQ[textFileLine_TextFileLine] :=
SourceFileLineSourceQ[textFileLine             ]  = SourceFilePathQ[FilePath[textFileLine]];

SourceFileLinePattern =
PatternSequence[
	TextFileLine -> _TextFileLine
];
Protect[SourceFileLinePattern];

SourceFileLineQ[___]                                                  := False;
SourceFileLineQ[object:SourceFileLine[pattern:SourceFileLinePattern]] :=
SourceFileLineQ[object                                              ]  = True;

(*
SourceFileLine[textFileLine_?TextFileLineQ /; SourceFileLineSourceQ[textFileLine]] :=
Module[{object},
	object = SourceFileLine[TextFileLine -> textFileLine];
	SourceFileLine[textFileLine] = object
];
*)

SourceFileLine[
	textFileLine_TextFileLine /; SourceFileLineSourceQ[textFileLine]
] :=
SourceFileLine[
	textFileLine
] =
SourceFileLine[
	TextFileLine -> textFileLine
];

SourceFileLine /:                   Format[object:SourceFileLine[pattern:SourceFileLinePattern]] :=                 LineText[object];
SourceFileLine /:               SourceFile[object:SourceFileLine[pattern:SourceFileLinePattern]] :=               SourceFile[TextFile[object]];
SourceFileLine /:                 TextFile[object:SourceFileLine[pattern:SourceFileLinePattern]] :=                 TextFile[TextFileLine[object]];
SourceFileLine /:                 FilePath[object:SourceFileLine[pattern:SourceFileLinePattern]] :=                 FilePath[TextFileLine[object]];
SourceFileLine /:               LineNumber[object:SourceFileLine[pattern:SourceFileLinePattern]] :=               LineNumber[TextFileLine[object]];
SourceFileLine /:                 LineText[object:SourceFileLine[pattern:SourceFileLinePattern]] :=                 LineText[TextFileLine[object]];
(*
SourceFileLine /:   HeaderIncludeLineUserQ[object:SourceFileLine[pattern:SourceFileLinePattern]] :=   HeaderIncludeLineUserQ[LineText[object]];
SourceFileLine /: HeaderIncludeLineSystemQ[object:SourceFileLine[pattern:SourceFileLinePattern]] := HeaderIncludeLineSystemQ[LineText[object]];
SourceFileLine /:       HeaderIncludeLineQ[object:SourceFileLine[pattern:SourceFileLinePattern]] :=       HeaderIncludeLineQ[LineText[object]];
*)
SourceFileLine /:             TextFileLine[object:SourceFileLine[pattern:SourceFileLinePattern]] := TextFileLine /. {pattern};

(* SourceFile *)

SourceFilePattern =
PatternSequence[
	TextFile -> _TextFile,
	Lines    -> Automatic | _List
];
Protect[SourceFilePattern];

SourceFileParsedPattern =
PatternSequence[
	TextFile -> _TextFile,
	Lines    -> _List
];
Protect[SourceFileParsedPattern];

SourceFileQ[___]                                          := False;
SourceFileQ[object:SourceFile[pattern:SourceFilePattern]] :=
SourceFileQ[object                                      ]  = True;

SourceFileParsedQ[___]                                                := False;
SourceFileParsedQ[object:SourceFile[pattern:SourceFileParsedPattern]] :=
SourceFileParsedQ[object                                            ]  = True;

SourceFile[         filePath_String /; SourceFilePathQ[filePath]] :=
SourceFile[         filePath                                    ]  =
SourceFile[TextFile[filePath                                   ]];

SourceFile[textFile_TextFile /; SourceFilePathQ[FilePath[textFile]]] :=
SourceFile[textFile                                                ]  =
SourceFile[
	TextFile -> textFile,
	Lines    -> Automatic
];

                  HeaderFileQ[sourceFile_SourceFile] :=                   HeaderFilePathQ[FilePath[sourceFile]];
                 CSourceFileQ[sourceFile_SourceFile] :=                  CSourceFilePathQ[FilePath[sourceFile]];
         CPlusPlusSourceFileQ[sourceFile_SourceFile] :=          CPlusPlusSourceFilePathQ[FilePath[sourceFile]];
        ObjectiveCSourceFileQ[sourceFile_SourceFile] :=         ObjectiveCSourceFilePathQ[FilePath[sourceFile]];
ObjectiveCPlusPlusSourceFileQ[sourceFile_SourceFile] := ObjectiveCPlusPlusSourceFilePathQ[FilePath[sourceFile]];

ImplementationFileQ[___]                                       := False;
ImplementationFileQ[sourceFile_?CSourceFileQ]                  := True;
ImplementationFileQ[sourceFile_?CPlusPlusSourceFileQ]          := True;
ImplementationFileQ[sourceFile_?ObjectiveCSourceFileQ]         := True;
ImplementationFileQ[sourceFile_?ObjectiveCPlusPlusSourceFileQ] := True;

HeaderIncludeLineList[sourceFile_SourceFile]   := Select[Lines[sourceFile], HeaderIncludeLineQ];
HeaderIncludeDataList[sourceFile_SourceFile]   := Map[HeaderIncludeLine[LineText[#]]&, HeaderIncludeLineList[sourceFile]];

HeaderIncludeList[sourceFile_SourceFile]       := HeaderIncludeDataList[sourceFile][[All, 1]];
HeaderIncludeListUser[sourceFile_SourceFile]   := Select[HeaderIncludeDataList[sourceFile], StringMatchQ[#[[2]], "User"]&][[All, 1]];
HeaderIncludeListSystem[sourceFile_SourceFile] := Select[HeaderIncludeDataList[sourceFile], StringMatchQ[#[[2]], "System"]&][[All, 1]];

SourceFile /:        FileNameTake[object:SourceFile[pattern:SourceFilePattern], n_Integer:-1, opts:OptionsPattern[]] := FileNameTake[FilePath[object], n, Sequence @@ FilterRules[{opts}, Options[FileNameTake]]];

SourceFile /:              Format[object:SourceFile[pattern:SourceFilePattern]] :=      FilePath[object];
SourceFile /:            FilePath[object:SourceFile[pattern:SourceFilePattern]] :=      FilePath[TextFile[object]];
SourceFile /:       FileExtension[object:SourceFile[pattern:SourceFilePattern]] := FileExtension[TextFile[object]];
SourceFile /:            FileDate[object:SourceFile[pattern:SourceFilePattern]] :=      FileDate[TextFile[object]];
SourceFile /:       FileLineCount[object:SourceFile[pattern:SourceFilePattern]] := FileLineCount[TextFile[object]];
SourceFile /:       FileByteCount[object:SourceFile[pattern:SourceFilePattern]] := FileByteCount[TextFile[object]];
SourceFile /:            FileHash[object:SourceFile[pattern:SourceFilePattern]] :=      FileHash[TextFile[object]];
SourceFile /:            TextFile[object:SourceFile[pattern:SourceFilePattern]] := TextFile /. {pattern};
SourceFile /:               Lines[object:SourceFile[pattern:SourceFilePattern]] :=
Module[{textFile, lines},
	textFile = TextFile[object];
	
	If[
		SourceFileParsedQ[SourceFile[textFile]],
		
		(* True *)
		lines = Lines /. List @@ SourceFile[textFile],

		(* False *)
		lines = Map[SourceFileLine, Select[Lines[textFile], SourceFileLineSourceQ]];
	
		SourceFile[FilePath[textFile]] =
		SourceFile[textFile]           =
		SourceFile[
			TextFile -> textFile,
			Lines    -> lines
		];
	];
	
	Lines[object] = lines
];


End[]

EndPackage[]
