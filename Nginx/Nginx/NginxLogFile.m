(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["Nginx`NginxLogFile`",
	{
		"Utilities`TextFile`",
		"Utilities`TimeInterval`"
	}
];

NginxLogFileData::usage  = "";
NginxLogFileDataQ::usage = "";

NginxLogFileLine::usage  = "";
NginxLogFileLineQ::usage = "";
RemoteAddress::usage     = "";
RemoteUser::usage        = "";
TimeLocal::usage         = "";
Request::usage           = "";
Status::usage            = "";
BodyBytesSent::usage     = "";
HttpReferer::usage       = "";
HttpUserAgent::usage     = "";


Begin["`Private`"];
Needs["Calendar`"];

(*
127.0.0.1 - - [07/Jan/2014:21:11:07 +0000] "GET / HTTP/1.1" 200 151 "-" "curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1 zlib/1.2.3.4 libidn/1.23 librtmp/2.3"
208.66.29.148 - - [07/Jan/2014:21:13:54 +0000] "GET / HTTP/1.1" 200 151 "-" "curl/7.30.0"
208.66.29.148 - - [07/Jan/2014:21:13:55 +0000] "GET / HTTP/1.1" 200 151 "-" "curl/7.30.0"
*)

NginxLogFilePattern =
StartOfString ~~
remoteAddress:Except[WhitespaceCharacter].. ~~ WhitespaceCharacter ~~
"-" ~~ WhitespaceCharacter ~~ 
remoteUser:Except[WhitespaceCharacter].. ~~ WhitespaceCharacter ~~
"[" ~~ timeLocal:Except[WhitespaceCharacter].. ~~ Whitespace ~~ timeZone:Except["]"].. ~~ "]" ~~
WhitespaceCharacter ~~
"\"" ~~ request:Except["\""].. ~~ "\"" ~~
WhitespaceCharacter ~~ 
status:DigitCharacter.. ~~
WhitespaceCharacter ~~ 
bodyBytesSent:DigitCharacter.. ~~
WhitespaceCharacter ~~
"\"" ~~ httpReferer:Except["\""] .. ~~ "\"" ~~
WhitespaceCharacter ~~
"\"" ~~ httpUserAgent:Except["\""].. ~~ "\"" ~~
EndOfString;
Protect[NginxLogFilePattern];

NginxLogFileLineSourceQ[___]                         := False;
NginxLogFileLineSourceQ[textFileLine_?TextFileLineQ] :=
NginxLogFileLineSourceQ[textFileLine               ]  = StringMatchQ[LineText[textFileLine], NginxLogFilePattern];

NginxLogFileLinePattern =
PatternSequence[
	RemoteAddress -> _String,
	RemoteUser    -> _String,
	TimeLocal     -> _?DateQ,
	Request       -> _String,
	Status        -> _Integer,
	BodyBytesSent -> _Integer,
	HttpReferer   -> _String,
	HttpUserAgent -> _String,
	TextFileLine  -> _?TextFileLineQ
];
Protect[NginxLogFileLinePattern];

NginxLogFileLineQ[___]                                                      := False;
NginxLogFileLineQ[object:NginxLogFileLine[pattern:NginxLogFileLinePattern]] :=
NginxLogFileLineQ[object                                                  ]  = True;

NginxLogFileLine[textFileLine_?TextFileLineQ /; NginxLogFileLineSourceQ[textFileLine]] :=
Module[{data, object},
	data = StringCases[LineText[textFileLine], NginxLogFilePattern :> {
		RemoteAddress -> remoteAddress,
		RemoteUser    -> remoteUser,
		TimeLocal     -> DatePlus[DateList[timeLocal], {$TimeZone, "Hour"}],
		Request       -> request,
		Status        -> ToExpression[status],
		BodyBytesSent -> ToExpression[bodyBytesSent],
		HttpReferer   -> httpReferer,
		HttpUserAgent -> httpUserAgent}];
		
	object = data /. rest : {{__}} :> NginxLogFileLine[Sequence @@ Flatten[rest, 1], TextFileLine -> textFileLine];
	NginxLogFileLine[textFileLine] = object
];

NginxLogFileLine[
	remoteAddress_String,
	remoteUser_String,
	timeLocal_?DateQ,
	request_String,
	status_Integer,
	bodyBytesSent_Integer,
	httpReferer_String,
	httpUserAgent_String,
	textFileLine_?TextFileLineQ
] :=
NginxLogFileLine[
	remoteAddress,
	remoteUser,
	timeLocal,
	request,
	status,
	bodyBytesSent,
	httpReferer,
	httpUserAgent,
	textFileLine
] =
NginxLogFileLine[
	RemoteAddress -> remoteAddress,
	RemoteUser    -> remoteUser,
	TimeLocal     -> timeLocal,
	Request       -> request,
	Status        -> status,
	BodyBytesSent -> bodyBytesSent,
	HttpReferer   -> httpReferer,
	HttpUserAgent -> httpUserAgent,
	TextFileLine  -> textFileLine
];

NginxLogFileLine /:    LineNumber[object:NginxLogFileLine[pattern:NginxLogFileLinePattern]] :=    LineNumber[object] = LineNumber[TextFileLine[object]];
NginxLogFileLine /: RemoteAddress[object:NginxLogFileLine[pattern:NginxLogFileLinePattern]] := RemoteAddress[object] = RemoteAddress /. {pattern};
NginxLogFileLine /:    RemoteUser[object:NginxLogFileLine[pattern:NginxLogFileLinePattern]] :=    RemoteUser[object] = RemoteUser    /. {pattern};
NginxLogFileLine /:     TimeLocal[object:NginxLogFileLine[pattern:NginxLogFileLinePattern]] :=     TimeLocal[object] = TimeLocal     /. {pattern};
NginxLogFileLine /:       Request[object:NginxLogFileLine[pattern:NginxLogFileLinePattern]] :=       Request[object] = Request       /. {pattern};
NginxLogFileLine /:        Status[object:NginxLogFileLine[pattern:NginxLogFileLinePattern]] :=        Status[object] = Status        /. {pattern};
NginxLogFileLine /: BodyBytesSent[object:NginxLogFileLine[pattern:NginxLogFileLinePattern]] := BodyBytesSent[object] = BodyBytesSent /. {pattern};
NginxLogFileLine /:   HttpReferer[object:NginxLogFileLine[pattern:NginxLogFileLinePattern]] :=   HttpReferer[object] = HttpReferer   /. {pattern};
NginxLogFileLine /: HttpUserAgent[object:NginxLogFileLine[pattern:NginxLogFileLinePattern]] := HttpUserAgent[object] = HttpUserAgent /. {pattern};
NginxLogFileLine /:  TextFileLine[object:NginxLogFileLine[pattern:NginxLogFileLinePattern]] :=  TextFileLine[object] = TextFileLine  /. {pattern};


NginxLogFileDataPattern =
PatternSequence[
	Lines    -> Automatic | _List,
	TextFile -> _?TextFileQ
];
Protect[NginxLogFileDataPattern];

NginxLogFileDataParsedPattern =
PatternSequence[
	Lines    -> _List      ,
	TextFile -> _?TextFileQ
];
Protect[NginxLogFileDataParsedPattern];

NginxLogFileDataQ[___]                                                      := False;
NginxLogFileDataQ[object:NginxLogFileData[pattern:NginxLogFileDataPattern]] :=
NginxLogFileDataQ[object                                                  ]  = True;

NginxLogFileDataParsedQ[___]                                                            := False;
NginxLogFileDataParsedQ[object:NginxLogFileData[pattern:NginxLogFileDataParsedPattern]] :=
NginxLogFileDataParsedQ[object                                                        ]  = True;

NginxLogFileData[         filePath_String /; FileQ[filePath] ] :=
NginxLogFileData[         filePath                           ]  =
NginxLogFileData[TextFile[filePath                          ]];

NginxLogFileData[textFile_?TextFileQ] :=
NginxLogFileData[textFile           ]  =
NginxLogFileData[
	Lines    -> Automatic,
	TextFile -> textFile
];


NginxLogFileData /:   FilePath[object:NginxLogFileData[pattern:NginxLogFileDataPattern]] := FilePath[TextFile[object]];
NginxLogFileData /:   TextFile[object:NginxLogFileData[pattern:NginxLogFileDataPattern]] := TextFile /. {pattern};
NginxLogFileData /:      Lines[object:NginxLogFileData[pattern:NginxLogFileDataPattern]] :=
Module[{textFile, lines},
	textFile = TextFile[object];
	
	If[
		NginxLogFileDataParsedQ[NginxLogFileData[textFile]],
		
		(* True *)
		lines = Lines /. List @@ NginxLogFileData[textFile],

		(* False *)
		lines = Map[NginxLogFileLine, Select[Lines[textFile], NginxLogFileLineSourceQ]];
	
		NginxLogFileData[FilePath[textFile]] =
		NginxLogFileData[textFile]           =
		NginxLogFileData[
			Lines    -> lines   ,
			TextFile -> textFile
		];
	];
	
	Lines[object] = lines
];


End[];

EndPackage[];
