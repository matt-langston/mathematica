(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["WWDC`WWDC2014`"]

                  SessionDataQ::usage = "";
                      SessionQ::usage = "";
                          LabQ::usage = "";
                        FocusQ::usage = "";
                     KeywordsQ::usage = "";
                         NounQ::usage = "";
                  SessionListQ::usage = "";

            SessionDescription::usage = "";
                 SessionEndGMT::usage = "";
                 SessionEndPDT::usage = "";
                  SessionFavID::usage = "";
                  SessionFocus::usage = "";
                     SessionID::usage = "";
               SessionKeywords::usage = "";
                 SessionNumber::usage = "";
                   SessionRoom::usage = "";
               SessionStartGMT::usage = "";
               SessionStartPDT::usage = "";
                  SessionTitle::usage = "";
                  SessionTrack::usage = "";
                   SessionType::usage = "";
                  SessionVideo::usage = "";
                  SessionTimes::usage = "";

SessionDescriptionForSessionID::usage = "";
                SelectSessions::usage = "";
                SessionSummary::usage = "";

Begin["`Private`"]
Needs["Calendar`"];
Needs["Utilities`JSONFile`"];
Needs["Utilities`UnixTime`"];

SessionDataPattern = PatternSequence[
	 "description"   -> _String
	, "endGMT"       -> _String
	, "end_time"     -> _Integer
	, "fav_id"       -> _String
	, "focus"        -> _?FocusQ
	, "id"           -> _String
	, "keywords"     -> _?KeywordsQ
	, "number"       -> _String
	, "room"         -> _String
	, "startGMT"     -> _String
	, "start_time"   -> _Integer
	, "title"        -> _String
	, "track"        -> _String
	, "type"         -> _String
	, "video"        -> _?BooleanQ
];

SessionDataQ[___]                                                := False
SessionDataQ[object:List[pattern:SessionDataPattern]]            := True;

SessionQ[___]                                                    := False
SessionQ[sessionData_?SessionDataQ]                              := StringMatchQ[SessionType[sessionData], "Session"];

LabQ[___]                                                        := False
LabQ[sessionData_?SessionDataQ]                                  := StringMatchQ[SessionType[sessionData], "Lab"];

FocusQ[___]                                                      := False;
FocusQ[list_List /; VectorQ[list, StringQ]]                      := MatchQ[Complement[list, {"iOS", "OS X"}], {}];

KeywordsQ[___]                                                   := False;
KeywordsQ[list_List /; VectorQ[list, StringQ]]                   := True;

NounQ[___]                                                       := False;
NounQ[word_String]                                               := MemberQ[WordData[word, "PartsOfSpeech"], "Noun"];

SessionListQ[___]                                                := False;
SessionListQ[sessionList_List /; VectorQ[sessionList, SessionQ]] := True;

 SessionDescription[sessionData_?SessionDataQ] :=              "description"  /. sessionData;
      SessionEndGMT[sessionData_?SessionDataQ] := DateList[    "endGMT"       /. sessionData];
      SessionEndPDT[sessionData_?SessionDataQ] := DateList[    "end_time"     /. sessionData];
       SessionFavID[sessionData_?SessionDataQ] :=              "fav_id"       /. sessionData;
       SessionFocus[sessionData_?SessionDataQ] := Sort[        "focus"        /. sessionData];
          SessionID[sessionData_?SessionDataQ] := ToExpression["id"           /. sessionData];
    SessionKeywords[sessionData_?SessionDataQ] := Sort[        "keywords"     /. sessionData]
      SessionNumber[sessionData_?SessionDataQ] := ToExpression["number"       /. sessionData];
        SessionRoom[sessionData_?SessionDataQ] :=              "room"         /. sessionData;
    SessionStartGMT[sessionData_?SessionDataQ] := DateList[    "startGMT"     /. sessionData];
    SessionStartPDT[sessionData_?SessionDataQ] := DateList[    "start_time"   /. sessionData];
       SessionTitle[sessionData_?SessionDataQ] :=              "title"        /. sessionData;
       SessionTrack[sessionData_?SessionDataQ] :=              "track"        /. sessionData;
        SessionType[sessionData_?SessionDataQ] :=              "type"         /. sessionData;
       SessionVideo[sessionData_?SessionDataQ] :=              "video"        /. sessionData;
       SessionTimes[sessionData_?SessionDataQ] := {DateList[DateList[UnixTime[SessionStartPDT[sessionData] * 10^3]]], DateList[UnixTime[SessionEndPDT[sessionData] * 10^3]]};

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
