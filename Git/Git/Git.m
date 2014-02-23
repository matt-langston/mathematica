BeginPackage["Git`",
	{
		"Utilities`TimeInterval`"
	}
];

GitLogTagData::usage = "";
GitCommand::usage    = "";
GitRepository::usage = "";

Begin["`Private`"];

GitCommand::arg = "Option value `1` for GitRepository must be a directory containing a git repository.";

Options[GitCommand] = {GitRepository :> None};

GitCommand[gitCommand_String, opts:OptionsPattern[]] :=
Module[{gitRepository, gitOutput},
    gitRepository  = OptionValue[GitRepository];

    If[!StringQ[gitRepository] || !DirectoryQ[gitRepository],
        Message[GitCommand::arg, gitRepository]; Abort[]
    ];

    SetDirectory[gitRepository];
    gitOutput = ReadList["!" <> gitCommand, Record];
    ResetDirectory[];
    gitOutput
];

(*
git --no-pager log --tags --simplify-by-decoration --pretty="format:%ai %d"

2013-12-20 10:53:18 -0800  (tag: 3_2_0_GA)
2013-11-22 16:44:57 -0800 
2013-08-30 13:30:07 -0700  (origin/timob-12740, timob-12740)
2013-09-27 13:45:10 -0700  (origin/timob-15263, timob-15263)
2013-09-09 23:20:23 -0700  (cb1kenobi-timob-15109)
2013-09-18 11:57:46 -0700  (tag: 3_1_3_GA)
2013-09-09 19:17:55 -0700 
2013-09-09 18:40:27 -0700  (srahim-timob-15078-31X)
2013-09-08 12:01:01 -0700  (srahim-splashScreenFix-3_1_X)
2013-08-30 13:30:07 -0700  (origin/timob-12740-3_1_X, timob-12740-3_1_X)
*)

GitTagLogLineStringPattern =
StartOfString ~~
timestamp:(dateTime:(date:Except[WhitespaceCharacter].. ~~ Whitespace ~~ time:Except[WhitespaceCharacter]..) ~~ Whitespace ~~ timezone:Except[WhitespaceCharacter]..) ~~
tag___ ~~
EndOfString;
Protect[GitTagLogLineStringPattern];

Options[GitLogTagData] = {GitRepository :> None};

GitLogTagData[opts:OptionsPattern[]] := Module[{gitOutput},
    gitOutput = GitCommand["git --no-pager log --tags --simplify-by-decoration --pretty=\"format:%ai %d\""];
    Flatten[StringCases[gitOutput, GitTagLogLineStringPattern :> {DateList[dateTime], timezone, tag}], 1]
];

(*   
GitTagLogLineSourceQ[___]           := False;
GitTagLogLineSourceQ[string_String] := StringMatchQ[string, GitTagLogLineStringPattern];

GitTagLogLinePattern =
PatternSequence[
    Timestamp -> _?DateQ,
    GitTag    -> _String
];
Protect[GitTagLogLinePattern];

GitTagLogLineQ[___]                                                := False;
GitTagLogLineQ[object:GitTagLogLine[pattern:GitTagLogLinePattern]] :=
GitTagLogLineQ[object                                            ]  = True;

GitTagLogLine[gitTagLogLine_?GitTagLogLineSourceQ] :=
Module[{data, object},
    data = StringCases[gitTagLogLine, GitTagLogLineStringPattern :> {
            Timestamp -> DateList[dateTime],
            GitTag    -> tag
        }
    ];
        
    object = data /. rest : {{__}} :> GitTagLogLine[Sequence @@ Flatten[rest, 1]];
    object
];

GitTagLogLine[
    timestamp_?DateQ,
    gitTag_String
] :=
GitTagLogLine[
    timestamp,
    gitTag
] =
GitTagLogLine[
    Timestamp -> timestamp,
    GitTag    -> gitTag
];

GitTagLogLine /: Timestamp[object:GitTagLogLine[pattern:GitTagLogLinePattern]] := Timestamp[object] = Timestamp /. {pattern};
GitTagLogLine /:    GitTag[object:GitTagLogLine[pattern:GitTagLogLinePattern]] :=    GitTag[object] = GitTag    /. {pattern};
*)

End[];

EndPackage[];
