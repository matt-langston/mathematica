(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["WWDC`WWDC2013SessionVideoLibrary`",
	{
		"WWDC`WWDC2013`",
		"Utilities`TextFile`"
	}
]

DownloadFormatList::usage                    = "";
ExportDownloadScript::usage                  = "";
CurlCommand::usage                           = "";

SessionVideoDownloadUrlData::usage           = "";
SessionVideoDownloadUrlDataQ::usage          = "";

SessionVideoDownloadUrl::usage               = "";
SessionVideoDownloadUrlPrefix::usage         = "";
SessionVideoDownloadUrlToken::usage          = "";
SessionVideoDownloadUrlSessionID1::usage     = "";
SessionVideoDownloadUrlSessionID2::usage     = "";
SessionVideoDownloadUrlDownloadFormat::usage = "";
SessionVideoDownloadUrlFileName::usage       = "";
SessionVideoDownloadUrlRest::usage           = "";

DefaultSessionVideoDownloadUrlPrefix::usage  = "";


SessionVideoLibrary::usage                   = "";
SessionVideoLibraryQ::usage                  = "";

SessionVideoData::usage                      = "";
SessionVideoDataQ::usage                     = "";
SessionVideoDataListQ::usage                 = "";
SessionVideoDownloadUrlDataList::usage       = "";

(*
SessionVideoHyperlinkPattern::usage         = "";
sessionVideoDownloadUrlDataMap::usage       = "";
sessionVideoDownloadUrlDataList::usage      = "";
*)

Begin["`Private`"]
Needs["Calendar`"];

DownloadFormatList[sessionVideoData_SessionVideoData] := Map[SessionVideoDownloadUrlDownloadFormat, SessionVideoDownloadUrlDataList[sessionVideoData]];

ExportDownloadScript[exportDownloadScriptPath_String, sessionVideoLibrary_SessionVideoLibrary, downloadFormat_String, downloadDir_String /; DirectoryQ[downloadDir]] :=
Module[{curlCommandList},
	curlCommandList = Map[CurlCommand[#, downloadFormat, downloadDir] &, Select[Lines[sessionVideoLibrary], MemberQ[DownloadFormatList[#], downloadFormat] &]];
	Export[exportDownloadScriptPath, curlCommandList, "Text"]
];

CurlCommand::arg = "Download format `1` not available for Session `2` (available formats are `3`)";
CurlCommand[sessionVideoData_SessionVideoData, downloadFormat_String, downloadDir_String /; DirectoryQ[downloadDir]] :=
Module[{sessionID, sessionVideoDownloadUrlData, sessionVideoDownloadUrl, sessionTitle, outputFileName, curlCommand},
	sessionID = SessionID[sessionVideoData];
	Check[
		MemberQ[DownloadFormatList[sessionVideoData], downloadFormat] || Message[CurlCommand::arg, downloadFormat, sessionID, DownloadFormatList[sessionVideoData]];
		sessionVideoDownloadUrlData = Select[SessionVideoDownloadUrlDataList[sessionVideoData], StringMatchQ[SessionVideoDownloadUrlDownloadFormat[#], downloadFormat] &][[1]];
		sessionVideoDownloadUrl = SessionVideoDownloadUrl[sessionVideoDownloadUrlData];
		sessionTitle = SessionTitle[sessionVideoData];
		outputFileName = StringReplace["Session " <> ToString[sessionID] <> " - " <> sessionTitle <> If[StringMatchQ[downloadFormat, "PDF"], ".pdf", ".mov"], "\[CloseCurlyQuote]" -> "'"];
		curlCommand = "curl \"" <> sessionVideoDownloadUrl <> "\" -o \"" <> FileNameJoin[{downloadDir, outputFileName}] <> "\"";
		curlCommand
		, $Failed, CurlCommand::arg
	]
];

(*
DefaultSessionVideoDownloadUrlPrefix = Automatic;
Options[SessionVideoHyperlinkPattern] = {SessionVideoDownloadUrlPrefix :> DefaultSessionVideoDownloadUrlPrefix};
*)
DefaultSessionVideoDownloadUrlPrefix = "http://devstreaming.apple.com/videos/wwdc/2013/";
Options[SessionVideoHyperlinkPattern] = {SessionVideoDownloadUrlPrefix -> DefaultSessionVideoDownloadUrlPrefix};

(* "http://devstreaming.apple.com/videos/wwdc/2013/400xex2xbskwa5bkxr17zihju9uf/400/400-HD.mov?dl=1" *)
SessionVideoHyperlinkPattern[opts:OptionsPattern[]] :=
sessionVideoDownloadUrl:(
sessionVideoDownloadPrefix:OptionValue[SessionVideoDownloadUrlPrefix]
~~ sessionVideoDownloadUrlToken:Except["/"]..
~~ "/" ~~ sessionVideoDownloadUrlSessionID1:DigitCharacter..
~~ "/" ~~ sessionVideoDownloadUrlFileName:(sessionVideoDownloadUrlSessionID2:DigitCharacter.. ~~ ("-" ~~ sessionVideoDownloadUrlDownloadFormat:("HD" | "SD") ~~ ".mov") | ".pdf")
~~ sessionVideoDownloadUrlVideoRest:___
);

SessionVideoDownloadUrlDataPattern =
PatternSequence[
	SessionVideoDownloadUrl               -> _String,
	SessionVideoDownloadUrlPrefix         -> _String,
	SessionVideoDownloadUrlToken          -> _String,
	SessionVideoDownloadUrlSessionID1     -> _Integer,
	SessionVideoDownloadUrlSessionID2     -> _Integer,
	SessionVideoDownloadUrlDownloadFormat -> _String,
	SessionVideoDownloadUrlFileName       -> _String,
	SessionVideoDownloadUrlRest           -> _String,
	TextFile                              -> _TextFile
];
Protect[SessionVideoDownloadUrlDataPattern];

SessionVideoDownloadUrlDataQ[___]                                                                            := False;
SessionVideoDownloadUrlDataQ[object:SessionVideoDownloadUrlData[pattern:SessionVideoDownloadUrlDataPattern]] :=
SessionVideoDownloadUrlDataQ[object                                                                        ]  = True;

SessionVideoDownloadUrlData[
	sessionVideoDownloadUrl_String,
	sessionVideoDownloadPrefix_String,
	sessionVideoDownloadUrlToken_String,
	sessionVideoDownloadUrlSessionID1_Integer,
	sessionVideoDownloadUrlSessionID2_Integer,
	sessionVideoDownloadUrlDownloadFormat_String,
	sessionVideoDownloadUrlFileName_String,
	sessionVideoDownloadUrlRest_String,
	textFile_TextFile
] :=
SessionVideoDownloadUrlData[
	sessionVideoDownloadUrl,
	sessionVideoDownloadPrefix,
	sessionVideoDownloadUrlToken,
	sessionVideoDownloadUrlSessionID1,
	sessionVideoDownloadUrlSessionID2,
	sessionVideoDownloadUrlDownloadFormat,
	sessionVideoDownloadUrlFileName,
	sessionVideoDownloadUrlRest,
	textFile
] =
SessionVideoDownloadUrlData[
	SessionVideoDownloadUrl               -> sessionVideoDownloadUrl,
	SessionVideoDownloadUrlPrefix         -> sessionVideoDownloadPrefix,
	SessionVideoDownloadUrlToken          -> sessionVideoDownloadUrlToken,
	SessionVideoDownloadUrlSessionID1     -> sessionVideoDownloadUrlSessionID1,
	SessionVideoDownloadUrlSessionID2     -> sessionVideoDownloadUrlSessionID2,
	SessionVideoDownloadUrlDownloadFormat -> sessionVideoDownloadUrlDownloadFormat,
	SessionVideoDownloadUrlFileName       -> sessionVideoDownloadUrlFileName,
	SessionVideoDownloadUrlRest           -> sessionVideoDownloadUrlRest,
	TextFile                              -> textFile
];

SessionVideoDownloadUrlData /:               SessionVideoDownloadUrl[object:SessionVideoDownloadUrlData[pattern:SessionVideoDownloadUrlDataPattern]] := SessionVideoDownloadUrl               /. {pattern};
SessionVideoDownloadUrlData /:         SessionVideoDownloadUrlPrefix[object:SessionVideoDownloadUrlData[pattern:SessionVideoDownloadUrlDataPattern]] := SessionVideoDownloadUrlPrefix         /. {pattern};
SessionVideoDownloadUrlData /:          SessionVideoDownloadUrlToken[object:SessionVideoDownloadUrlData[pattern:SessionVideoDownloadUrlDataPattern]] := SessionVideoDownloadUrlToken          /. {pattern};
SessionVideoDownloadUrlData /:     SessionVideoDownloadUrlSessionID1[object:SessionVideoDownloadUrlData[pattern:SessionVideoDownloadUrlDataPattern]] := SessionVideoDownloadUrlSessionID1     /. {pattern};
SessionVideoDownloadUrlData /:     SessionVideoDownloadUrlSessionID2[object:SessionVideoDownloadUrlData[pattern:SessionVideoDownloadUrlDataPattern]] := SessionVideoDownloadUrlSessionID2     /. {pattern};
SessionVideoDownloadUrlData /: SessionVideoDownloadUrlDownloadFormat[object:SessionVideoDownloadUrlData[pattern:SessionVideoDownloadUrlDataPattern]] := SessionVideoDownloadUrlDownloadFormat /. {pattern};
SessionVideoDownloadUrlData /:       SessionVideoDownloadUrlFileName[object:SessionVideoDownloadUrlData[pattern:SessionVideoDownloadUrlDataPattern]] := SessionVideoDownloadUrlFileName       /. {pattern};
SessionVideoDownloadUrlData /:           SessionVideoDownloadUrlRest[object:SessionVideoDownloadUrlData[pattern:SessionVideoDownloadUrlDataPattern]] := SessionVideoDownloadUrlRest           /. {pattern};
SessionVideoDownloadUrlData /:                              TextFile[object:SessionVideoDownloadUrlData[pattern:SessionVideoDownloadUrlDataPattern]] := TextFile                              /. {pattern};


SessionVideoDataPattern =
PatternSequence[
	SessionID                       -> _Integer,
	SessionTitle                    -> _String,
	SessionTrack                    -> _String,
	SessionVideoDownloadUrlDataList -> sessionVideoDownloadUrlDataList_List /; VectorQ[sessionVideoDownloadUrlDataList, SessionVideoDownloadUrlDataQ],
	TextFile                        -> _TextFile
];
Protect[SessionVideoDataPattern];

SessionVideoDataQ[___]                                                      := False;
SessionVideoDataQ[object:SessionVideoData[pattern:SessionVideoDataPattern]] :=
SessionVideoDataQ[object                                                  ]  = True;

SessionVideoDataListQ[___]                                                                           := False;
SessionVideoDataListQ[sessionVideoDataList_List /; VectorQ[sessionVideoDataList, SessionVideoDataQ]] := True;

SessionVideoData[
	sessionID_Integer,
	sessionTitle_String,
	sessionTrack_String,
	sessionVideoDownloadUrlDataList_List /; VectorQ[sessionVideoDownloadUrlDataList, SessionVideoDownloadUrlDataQ],
	textFile_TextFile
] :=
SessionVideoData[
	sessionID,
	sessionTitle,
	sessionTrack,
	sessionVideoDownloadUrlDataList,
	textFile
] =
SessionVideoData[
	SessionID                       -> sessionID,
	SessionTitle                    -> sessionTitle,
	SessionTrack                    -> sessionTrack,
	SessionVideoDownloadUrlDataList -> sessionVideoDownloadUrlDataList,
	TextFile                        -> textFile
];

SessionVideoData /:                        SessionSummary[object:SessionVideoData[pattern:SessionVideoDataPattern]] := {SessionID[object], SessionTrack[object], SessionTitle[object]};
SessionVideoData /:                             SessionID[object:SessionVideoData[pattern:SessionVideoDataPattern]] := SessionID                       /. {pattern};
SessionVideoData /:                          SessionTitle[object:SessionVideoData[pattern:SessionVideoDataPattern]] := SessionTitle                    /. {pattern};
SessionVideoData /:                          SessionTrack[object:SessionVideoData[pattern:SessionVideoDataPattern]] := SessionTrack                    /. {pattern};
SessionVideoData /:       SessionVideoDownloadUrlDataList[object:SessionVideoData[pattern:SessionVideoDataPattern]] := SessionVideoDownloadUrlDataList /. {pattern};
SessionVideoData /:                              TextFile[object:SessionVideoData[pattern:SessionVideoDataPattern]] := TextFile                        /. {pattern};

SessionSummary[sessionVideoDataList_?SessionVideoDataListQ] := Map[SessionSummary, sessionVideoDataList];

SessionSummaryGrid[sessionVideoDataList_?SessionVideoDataListQ] :=
Grid[Join[{{"SessionID", "SessionTitle", "SessionTrack"}}, SessionSummary[sessionVideoDataList]],
	Alignment -> {{Center, Left, Left}, Automatic, {{1, 1}, {1, -1}} -> Center},
	Dividers -> {None, 2 -> True}, Frame -> True, ItemSize -> Full
];

(*
SessionVideoData /:               SessionVideoDownloadUrl[object:SessionVideoData[pattern:SessionVideoDataPattern]] :=               SessionVideoDownloadUrl[SessionVideoDownloadUrlData[object]];
SessionVideoData /:         SessionVideoDownloadUrlPrefix[object:SessionVideoData[pattern:SessionVideoDataPattern]] :=         SessionVideoDownloadUrlPrefix[SessionVideoDownloadUrlData[object]];
SessionVideoData /:          SessionVideoDownloadUrlToken[object:SessionVideoData[pattern:SessionVideoDataPattern]] :=          SessionVideoDownloadUrlToken[SessionVideoDownloadUrlData[object]];
SessionVideoData /:     SessionVideoDownloadUrlSessionID1[object:SessionVideoData[pattern:SessionVideoDataPattern]] :=     SessionVideoDownloadUrlSessionID1[SessionVideoDownloadUrlData[object]];
SessionVideoData /:     SessionVideoDownloadUrlSessionID2[object:SessionVideoData[pattern:SessionVideoDataPattern]] :=     SessionVideoDownloadUrlSessionID2[SessionVideoDownloadUrlData[object]];
SessionVideoData /: SessionVideoDownloadUrlDownloadFormat[object:SessionVideoData[pattern:SessionVideoDataPattern]] := SessionVideoDownloadUrlDownloadFormat[SessionVideoDownloadUrlData[object]];
SessionVideoData /:       SessionVideoDownloadUrlFileName[object:SessionVideoData[pattern:SessionVideoDataPattern]] :=       SessionVideoDownloadUrlFileName[SessionVideoDownloadUrlData[object]];
SessionVideoData /:           SessionVideoDownloadUrlRest[object:SessionVideoData[pattern:SessionVideoDataPattern]] :=           SessionVideoDownloadUrlRest[SessionVideoDownloadUrlData[object]];
*)

(* WWDC2013SessionVideoData *)

SessionVideoLibraryPattern =
PatternSequence[
	TextFile -> _TextFile,
	Lines    -> Automatic | _List
];
Protect[SessionVideoLibraryPattern];

SessionVideoLibraryParsedPattern =
PatternSequence[
	TextFile -> _TextFile,
	Lines    -> _List
];
Protect[SessionVideoLibraryParsedPattern];

SessionVideoLibraryQ[___]                                                            := False;
SessionVideoLibraryQ[object:SessionVideoLibrary[pattern:SessionVideoLibraryPattern]] :=
SessionVideoLibraryQ[object                                                        ]  = True;

SessionVideoLibraryParsedQ[___]                                                                  := False;
SessionVideoLibraryParsedQ[object:SessionVideoLibrary[pattern:SessionVideoLibraryParsedPattern]] :=
SessionVideoLibraryParsedQ[object                                                              ]  = True;

SessionVideoLibrary[         filePath_String /; FileQ[filePath]] :=
SessionVideoLibrary[         filePath                          ]  =
SessionVideoLibrary[TextFile[filePath                         ]];

SessionVideoLibrary[textFile_TextFile] :=
SessionVideoLibrary[textFile         ]  =
SessionVideoLibrary[
	TextFile -> textFile,
	Lines    -> Automatic
];

SessionVideoLibrary /:            FilePath[object:SessionVideoLibrary[pattern:SessionVideoLibraryPattern]] :=      FilePath[TextFile[object]];
SessionVideoLibrary /:       FileExtension[object:SessionVideoLibrary[pattern:SessionVideoLibraryPattern]] := FileExtension[TextFile[object]];
SessionVideoLibrary /:            FileDate[object:SessionVideoLibrary[pattern:SessionVideoLibraryPattern]] :=      FileDate[TextFile[object]];
SessionVideoLibrary /:       FileLineCount[object:SessionVideoLibrary[pattern:SessionVideoLibraryPattern]] := FileLineCount[TextFile[object]];
SessionVideoLibrary /:       FileByteCount[object:SessionVideoLibrary[pattern:SessionVideoLibraryPattern]] := FileByteCount[TextFile[object]];
SessionVideoLibrary /:            FileHash[object:SessionVideoLibrary[pattern:SessionVideoLibraryPattern]] :=      FileHash[TextFile[object]];
SessionVideoLibrary /:            TextFile[object:SessionVideoLibrary[pattern:SessionVideoLibraryPattern]] := TextFile /. {pattern};
SessionVideoLibrary /:               Lines[object:SessionVideoLibrary[pattern:SessionVideoLibraryPattern]] :=
Module[{textFile, lines, sessionVideoDownloadUrlRawData, sessionVideoDownloadUrlDataList, sessionVideoLibraryDataList},
	textFile = TextFile[object];
	If[
		SessionVideoLibraryParsedQ[SessionVideoLibrary[textFile]],
		
		(* True *)
		lines = Lines /. List @@ SessionVideoLibrary[textFile],

		(* False *)
		sessionVideoDownloadUrlRawData = Import[FilePath[textFile], {"XHTML", "Hyperlinks"}];
		sessionVideoDownloadUrlDataList = Flatten[StringCases[sessionVideoDownloadUrlRawData, SessionVideoHyperlinkPattern[] :>
			SessionVideoDownloadUrlData[
				sessionVideoDownloadUrl,
				sessionVideoDownloadPrefix,
				sessionVideoDownloadUrlToken,
				ToExpression[sessionVideoDownloadUrlSessionID1],
				ToExpression[sessionVideoDownloadUrlSessionID2],
				If[StringLength[sessionVideoDownloadUrlDownloadFormat] > 0, sessionVideoDownloadUrlDownloadFormat, "PDF"],
				sessionVideoDownloadUrlFileName,
				sessionVideoDownloadUrlVideoRest,
				textFile
				]], 1];
		sessionVideoLibraryDataList = Import[FilePath[textFile], {"XHTML", "Data"}];
		(*
		#[[3]] == SessionID
		#[[1]] == SessionTitle
		#[[2]] == SessionTrack
		*)
		lines = Map[SessionVideoData[ToExpression[#[[3]]], #[[1]], #[[2]], Cases[sessionVideoDownloadUrlDataList, SessionVideoDownloadUrlData[___, SessionVideoDownloadUrlSessionID1 -> ToExpression[#[[3]]], ___]], textFile]&, sessionVideoLibraryDataList[[2, 3, All, 1]]];

		SessionVideoLibrary[FilePath[textFile]] =
		SessionVideoLibrary[textFile]           =
		SessionVideoLibrary[
			TextFile -> textFile,
			Lines    -> lines
		];
	];
	
	Lines[object] = lines
];


End[]

EndPackage[]
