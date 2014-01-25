(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["WWDC`WWDC2013`"]

                  SessionDataQ::usage = "";
                      SessionQ::usage = "";
                          LabQ::usage = "";
                        FocusQ::usage = "";
                         NounQ::usage = "";
                  SessionListQ::usage = "";

            SessionDescription::usage = "";
                 SessionEndGMT::usage = "";
                 SessionEndPDT::usage = "";
                  SessionFirst::usage = "";
                  SessionFocus::usage = "";
                 SessionHexnum::usage = "";
           SessionIcalDuration::usage = "";
              SessionIcalStart::usage = "";
                     SessionID::usage = "";
                 SessionRandom::usage = "";
                 SessionRepeat::usage = "";
                   SessionRoom::usage = "";
               SessionStartGMT::usage = "";
               SessionStartPDT::usage = "";
                  SessionTitle::usage = "";
                  SessionTrack::usage = "";
                   SessionType::usage = "";
                    SessionURL::usage = "";
                  SessionTimes::usage = "";

SessionDescriptionForSessionID::usage = "";
                SelectSessions::usage = "";
                SessionSummary::usage = "";

Begin["`Private`"]
Needs["Calendar`"];
Needs["Utilities`JSONFile`"];

SessionDataPattern = PatternSequence[
	 "description"   -> _String
	, "endGMT"       -> _String
	, "endPDT"       -> _String
	, "first"        -> _?BooleanQ
	, "focus"        -> _?FocusQ
	, "hexnum"       -> _String
	, "icalDuration" -> _String
	, "icalStart"    -> _String
	, "id"           -> _Integer
	, "random"       -> _String
	, "repeat"       -> _
	, "room"         -> _String
	, "startGMT"     -> _String
	, "startPDT"     -> _String
	, "time"         -> {{"lower" -> _String, "upper" -> _String}}
	, "title"        -> _String
	, "track"        -> _String
	, "type"         -> _String
	, "url"          -> _String
];

SessionDataQ[___]                                                := False
SessionDataQ[object:List[pattern:SessionDataPattern]]            := True;

SessionQ[___]                                                    := False
SessionQ[sessionData_?SessionDataQ]                              := StringMatchQ[SessionType[sessionData], "Session"];

LabQ[___]                                                        := False
LabQ[sessionData_?SessionDataQ]                                  := StringMatchQ[SessionType[sessionData], "Lab"];

FocusQ[___]                                                      := False;
FocusQ[list_List /; VectorQ[list, StringQ]]                      := MatchQ[Complement[list, {"iOS", "OS X"}], {}];

NounQ[___]                                                       := False;
NounQ[word_String]                                               := MemberQ[WordData[word, "PartsOfSpeech"], "Noun"];

SessionListQ[___]                                                := False;
SessionListQ[sessionList_List /; VectorQ[sessionList, SessionQ]] := True;

 SessionDescription[sessionData_?SessionDataQ] :=           "description"  /. sessionData;
      SessionEndGMT[sessionData_?SessionDataQ] := DateList[ "endGMT"       /. sessionData];
      SessionEndPDT[sessionData_?SessionDataQ] := DateList[ "endPDT"       /. sessionData];
       SessionFirst[sessionData_?SessionDataQ] :=           "first"        /. sessionData;
       SessionFocus[sessionData_?SessionDataQ] := Sort[     "focus"        /. sessionData];
      SessionHexnum[sessionData_?SessionDataQ] :=           "hexnum"       /. sessionData;
SessionIcalDuration[sessionData_?SessionDataQ] :=           "icalDuration" /. sessionData;
   SessionIcalStart[sessionData_?SessionDataQ] := DateList[{"icalStart"    /. sessionData, {"Year", "Month", "Day", "T", "Hour", "Minute", "Second"}}];
          SessionID[sessionData_?SessionDataQ] :=           "id"           /. sessionData;
      SessionRandom[sessionData_?SessionDataQ] :=           "random"       /. sessionData;
      SessionRepeat[sessionData_?SessionDataQ] :=           "repeat"       /. sessionData;
        SessionRoom[sessionData_?SessionDataQ] :=           "room"         /. sessionData;
    SessionStartGMT[sessionData_?SessionDataQ] := DateList[ "startGMT"     /. sessionData];
    SessionStartPDT[sessionData_?SessionDataQ] := DateList[ "startPDT"     /. sessionData];
       SessionTitle[sessionData_?SessionDataQ] :=           "title"        /. sessionData;
       SessionTrack[sessionData_?SessionDataQ] :=           "track"        /. sessionData;
        SessionType[sessionData_?SessionDataQ] :=           "type"         /. sessionData;
         SessionURL[sessionData_?SessionDataQ] :=           "url"          /. sessionData;
       SessionTimes[sessionData_?SessionDataQ] := Module[{lower, upper},
	{{lower, upper}} = "time" /. sessionData;
	{DateList["lower" /. lower], DateList["upper" /. upper]}
];

SessionDescriptionForSessionID[sessionList_?SessionListQ, sessionID_Integer] := SessionDescription @@ Select[sessionList, MatchQ[SessionID[#], sessionID] &];

SelectSessions[sessionList_?SessionListQ, stringExpression_] :=
Select[sessionList,
	StringMatchQ[SessionTitle[#]      , ___ ~~ WordBoundary ~~ (stringExpression) ~~ ___, IgnoreCase -> True] ||
	StringMatchQ[SessionDescription[#], ___ ~~ WordBoundary ~~ (stringExpression) ~~ ___, IgnoreCase -> True] &
];

SessionSummary[session_?SessionQ]         := {SessionID[session], SessionTrack[session], SessionTitle[session]};
SessionSummary[sessionList_?SessionListQ] := Map[SessionSummary, sessionList];

End[]

EndPackage[]
