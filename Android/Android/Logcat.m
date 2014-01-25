(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["Android`Logcat`",
	{
		"Utilities`TextFile`",
		"Utilities`TimeInterval`"
	}
]

LogcatData::usage  = "";
LogcatDataQ::usage = "";

LogcatLine::usage  = "";
LogcatLineQ::usage = "";
PID::usage         = "";
TID::usage         = "";
LogLevel::usage    = "";
LogTag::usage      = "";
LogMessage::usage  = "";


Begin["`Private`"]
Needs["Calendar`"];

LogcatLineRegexpPattern = "^((\\d{2})-(\\d{2}) (\\d{2}):(\\d{2}):((\\d{2})\\.(\\d{3})))\\s+(\\d+)\\s+(\\d+)\\s+([VDIWEFS])\\s+([^: ]+)\\s*:\\s*(.+)$";
Protect[LogcatLineRegexpPattern];

(* Regexp captures for "04-22 08:53:06.590   157   157 I Vold    : Vold 2.1 (the revenge) firing up" *)
(* $1  is the whole timestamp   e.g. "04-22 08:53:06.590"               *)
(* $2  is the month             e.g. 04                                 *)
(* $3  is the day               e.g. 22                                 *)
(* $4  is the hour              e.g. 08                                 *)
(* $5  is the minute            e.g. 53                                 *)
(* $6  is the fractional second e.g. 06.590                             *)
(* $7  is the second            e.g. 06                                 *)
(* $8  is the millisecond       e.g. 590                                *)
(* $9  is the PID               e.g. 157                                *)
(* $10 is the TID               e.g. 157                                *)
(* $11 is the log level         e.g. "I"                                *)
(* $12 is the log tag           e.g. "Vold"                             *)
(* $13 is the log message       e.g. "Vold 2.1 (the revenge) firing up" *)

LogcatLineRegexp = RegularExpression["(?s)" <> LogcatLineRegexpPattern];
Protect[LogcatLineRegexp];

LogcatLineSourceQ[___]                         := False;
LogcatLineSourceQ[textFileLine_?TextFileLineQ] :=
LogcatLineSourceQ[textFileLine               ]  = StringMatchQ[LineText[textFileLine], LogcatLineRegexp];

LogcatLinePattern =
PatternSequence[
	Timestamp    -> _?DateQ,
	PID          -> _Integer,
	TID          -> _Integer,
	LogLevel     -> _String,
	LogTag       -> _String,
	LogMessage   -> _String,
	TextFileLine -> _?TextFileLineQ
];
Protect[LogcatLinePattern];

LogcatLineQ[___]                                          := False;
LogcatLineQ[object:LogcatLine[pattern:LogcatLinePattern]] :=
LogcatLineQ[object                                      ]  = True;

LogcatLine[textFileLine_?TextFileLineQ /; LogcatLineSourceQ[textFileLine]] :=
Module[{data, object},
	data = StringCases[LineText[textFileLine], LogcatLineRegexp :> {
		Timestamp  -> DateList[{
								DateList[][[1]], (*current year*)
								ToExpression["$2"],
								ToExpression["$3"],
								ToExpression["$4"],
								ToExpression["$5"],
								ToExpression["$6"]
							   }],
		PID        -> ToExpression[ "$9"],
		TID        -> ToExpression["$10"],
		LogLevel   ->              "$11",
		LogTag     ->              "$12",
		LogMessage ->              "$13"}];
		
	object = data /. rest : {{__}} :> LogcatLine[Sequence @@ Flatten[rest, 1], TextFileLine -> textFileLine];
	LogcatLine[textFileLine] = object
];

LogcatLine[
	timestamp_?DateQ,
	pid_Integer,
	tid_Integer,
	logLevel_String,
	logTag_String,
	logMessage_String,
	textFileLine_?TextFileLineQ
] :=
LogcatLine[
	timestamp,
	pid,
	tid,
	logLevel,
	logTag,
	logMessage,
	textFileLine
] =
LogcatLine[
	Timestamp    -> timestamp,
	PID          -> pid,
	TID          -> tid,
	LogLevel     -> logLevel,
	LogTag       -> logTag,
	LogMessage   -> logMessage,
	TextFileLine -> textFileLine
];

LogcatLine /:   LineNumber[object:LogcatLine[pattern:LogcatLinePattern]] :=   LineNumber[object] = LineNumber[TextFileLine[object]];
LogcatLine /:    Timestamp[object:LogcatLine[pattern:LogcatLinePattern]] :=    Timestamp[object] = Timestamp    /. {pattern};
LogcatLine /:          PID[object:LogcatLine[pattern:LogcatLinePattern]] :=          PID[object] = PID          /. {pattern};
LogcatLine /:          TID[object:LogcatLine[pattern:LogcatLinePattern]] :=          TID[object] = TID          /. {pattern};
LogcatLine /:     LogLevel[object:LogcatLine[pattern:LogcatLinePattern]] :=     LogLevel[object] = LogLevel     /. {pattern};
LogcatLine /:       LogTag[object:LogcatLine[pattern:LogcatLinePattern]] :=       LogTag[object] = LogTag       /. {pattern};
LogcatLine /:   LogMessage[object:LogcatLine[pattern:LogcatLinePattern]] :=   LogMessage[object] = LogMessage   /. {pattern};
LogcatLine /: TextFileLine[object:LogcatLine[pattern:LogcatLinePattern]] := TextFileLine[object] = TextFileLine /. {pattern};


LogcatDataPattern =
PatternSequence[
	Lines    -> Automatic | _List,
	TextFile -> _?TextFileQ
];
Protect[LogcatDataPattern];

LogcatDataParsedPattern =
PatternSequence[
	Lines    -> _List      ,
	TextFile -> _?TextFileQ
];
Protect[LogcatDataParsedPattern];

LogcatDataQ[___]                                          := False;
LogcatDataQ[object:LogcatData[pattern:LogcatDataPattern]] :=
LogcatDataQ[object                                      ]  = True;

LogcatDataParsedQ[___]                                                := False;
LogcatDataParsedQ[object:LogcatData[pattern:LogcatDataParsedPattern]] :=
LogcatDataParsedQ[object                                            ]  = True;

LogcatData[         filePath_String /; FileQ[filePath] ] :=
LogcatData[         filePath                           ]  =
LogcatData[TextFile[filePath                          ]];

LogcatData[textFile_?TextFileQ] :=
LogcatData[textFile           ]  =
LogcatData[
	Lines    -> Automatic,
	TextFile -> textFile
];


LogcatData /:   FilePath[object:LogcatData[pattern:LogcatDataPattern]] := FilePath[TextFile[object]];
LogcatData /:   TextFile[object:LogcatData[pattern:LogcatDataPattern]] := TextFile /. {pattern};
LogcatData /:      Lines[object:LogcatData[pattern:LogcatDataPattern]] :=
Module[{textFile, lines},
	textFile = TextFile[object];
	
	If[
		LogcatDataParsedQ[LogcatData[textFile]],
		
		(* True *)
		lines = Lines /. List @@ LogcatData[textFile],

		(* False *)
		lines = Map[LogcatLine, Select[Lines[textFile], LogcatLineSourceQ]];
	
		LogcatData[FilePath[textFile]] =
		LogcatData[textFile]           =
		LogcatData[
			Lines    -> lines   ,
			TextFile -> textFile
		];
	];
	
	Lines[object] = lines
];


End[]

EndPackage[]
