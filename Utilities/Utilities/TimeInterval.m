(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["Utilities`TimeInterval`", {"Units`"}];

TimestampFormatFunction::usage = "";
DefaultTimestampFormatFunction::usage = "";

Timestamp::usage = "Timestamp[date] has head Timestamp with the given \
date.";

TimeInterval::usage = "TimeInterval[{t0, t1}] represents the range of \
time between t0 and t1.";

Duration::usage = "";

TimeIntervalQ::usage = "TimeIntervalQ[expr] yields True if expr is a \
valid TimeInterval, and False otherwise.";

TimestampQ::usage = "TimestampQ[expr] yields True if expr is a \
Timestamp holding a valid date in the current calendar system, and \
False otherwise.";

AbsoluteTimeQ::usage = "AbsoluteTimeQ[expr] yields True if expr is a \
valid absolute time in the current calendar system, and False \
otherwise.";

DateStringQ::usage = "DateStringQ[expr] yields True if expr is a \
string representing a valid date in the current calendar system, and \
False otherwise.";

DateListQ::usage = "";

(*For Debug purposes only.*)
RealNumberPattern::usage = "";
RealNumberQ::usage = "";
AbsoluteTimePattern::usage = "";
DateListPattern::usage = "";
DateListElementQ::usage = "";

(*
Year::usage = "";
Month::usage = "";
Day::usage = "";
Hour::usage = "";
Minute::usage = "";
Second::usage = "";
*)

DurationPattern::usage = "";
TimeIntervalPattern::usage = "";
TimestampPattern::usage = "";

Begin["`Private`"];

DateStringISO8601 = {"Year", "-", "Month", "-", "Day", "T", "Hour24", ":", "Minute", ":", "SecondExact", "Z"};
   
RealNumberPattern = expr_?NumberQ /; Im[expr] == 0;
Protect[RealNumberPattern];

RealNumberQ[RealNumberPattern] := True;
RealNumberQ[___] := False;
Protect[RealNumberQ];


Attributes[DateListPattern] = {HoldFirst};
DateListPattern[Year  ] = _Integer;
DateListPattern[Month ] = _Integer;
DateListPattern[Day   ] = _?RealNumberQ;
DateListPattern[Hour  ] = _?RealNumberQ;
DateListPattern[Minute] = _?RealNumberQ;
DateListPattern[Second] = _?RealNumberQ;
DateListPattern =
{
	Repeated[DateListPattern[Year  ], {1, 1}],
	Repeated[DateListPattern[Month ], {0, 1}],
	Repeated[DateListPattern[Day   ], {0, 1}],
	Repeated[DateListPattern[Hour  ], {0, 1}],
	Repeated[DateListPattern[Minute], {0, 1}],
	Repeated[DateListPattern[Second], {0, 1}]
  };
Protect[DateListPattern];

Attributes[DateListElementQ] = {HoldFirst};
DateListElementQ[Month ][dateList_?DateListQ] := Length[dateList] >=  2;
DateListElementQ[Day   ][dateList_?DateListQ] := Length[dateList] >=  3;
DateListElementQ[Hour  ][dateList_?DateListQ] := Length[dateList] >=  4;
DateListElementQ[Minute][dateList_?DateListQ] := Length[dateList] >=  5;
DateListElementQ[Second][dateList_?DateListQ] := Length[dateList] === 6;

DateListElementQ[Month ][___] := False;
DateListElementQ[Day   ][___] := False;
DateListElementQ[Hour  ][___] := False;
DateListElementQ[Minute][___] := False;
DateListElementQ[Second][___] := False;
Protect[DateListElementQ];

Attributes[DateListQ] = {HoldFirst};

(*
	An expression is manifestly a DateList if its structure matches
	the pattern DateListPattern.
*)
DateListQ[expr_List /; MatchQ[HoldPattern[expr], DateListPattern]] := True;

(*
	Evaluate expressions whose structure does not match the pattern 
	DateListPattern and try again. The evaluated form of the 
	expression is sown with the tag "DateList" for potential post
	processing.
	
	For example, during the evaluation of
	DateListQ[DateList["Jun 04 2010"]], DateList["Jun 04 2010"] will
	evaluate to {2010, 6, 4, 0, 0, 0.}, which matches
	DateListPattern, which causes DateListQ to return True.
	The client of DateListQ, detecting that the string "Jun 04 2010"
	actually does encode a date/time can reap the value
	{2010, 6, 4, 0, 0, 0.} instead of evaluating
	DateList["Jun 04 2010"] again, an relatively expensive operation.
*)
DateListQ[expr_, opts:OptionsPattern[]] := Check[Quiet[MatchQ[Sow[expr, DateList], DateListPattern], {DateList::arg, DateList::str}], False];
DateListQ[___] := False;
Protect[DateListQ];

(*
	An expression is manifestly an AbsoluteTime if its structure
	matches the pattern RealNumberPattern.
*)
AbsoluteTimeQ[RealNumberPattern] := True;
AbsoluteTimeQ[___]               := False;
Protect[AbsoluteTimeQ];

(*
	Dates and times encoded as strings must be evaluated by DateList.
	Since DateListQ has attribute HoldFirst, the evaluation of
	DateList is deferred so that its value is sown for potential post
	processing.
*)
DateStringQ[ dateString_String,                                                   opts:OptionsPattern[]] := DateListQ[DateList[ dateString,                FilterRules[{opts}, Options[DateList]]], opts];
DateStringQ[{dateString_String, dateElements_ /; VectorQ[dateElements, StringQ]}, opts:OptionsPattern[]] := DateListQ[DateList[{dateString, dateElements}, FilterRules[{opts}, Options[DateList]]], opts];
DateStringQ[___] := False;
Protect[DateStringQ];


AbsoluteTimePattern = AbsoluteTime -> _?AbsoluteTimeQ;
Protect[AbsoluteTimePattern];

TimestampPattern = Repeated[DateList -> DateListPattern, {1}];
Protect[TimestampPattern];

Attributes[TimestampQ] = {};
TimestampQ[Timestamp[expr_]] /; MatchQ[ReleaseHold[expr], TimestampPattern] := True;
TimestampQ[___] := False;
Protect[TimestampQ];


(*
SimpleTimestampFormatFunction[timestamp_Timestamp?TimestampQ] := "-" <> DateString[timestamp] <> "-";
Protect[SimpleTimestampFormatFunction];

DefaultTimestampFormatFunction = Automatic;
Format[timestamp_Timestamp?TimestampQ] := With[{f = OptionValue[Timestamp, TimestampFormatFunction]},
	Switch[f,
		Automatic,
		SimpleTimestampFormatFunction[timestamp],
		_,
		f[timestamp]
	]
];
*)

Attributes[Timestamp] = {};
(*Options[Timestamp] = {TimestampFormatFunction :> DefaultTimestampFormatFunction};*)

(*Timestamp[expr_ /; MatchQ[expr, TimestampPattern]] := Timestamp[Hold[Evaluate[expr]]];*)
Timestamp[opts:OptionsPattern[] /; (*This test stops the recursion.*)FreeQ[{opts}, TimestampPattern, {1}]] :=
	Timestamp[DateList -> DateList[FilterRules[{opts}, Options[DateList]]]];
Timestamp[absoluteTime_?AbsoluteTimeQ, opts:OptionsPattern[]] := Module[{dateQ, ts},
	{dateQ, {{ts}}} = Reap[DateListQ[DateList[absoluteTime, FilterRules[{opts}, Options[DateList]]], opts], DateList];
	If[dateQ,
		(*Maintain the precision and accuracy of the argument.*)
		ts[[6]] = IntegerPart[ts[[6]]] + FractionalPart[absoluteTime];
		Timestamp[DateList -> ts]] /; dateQ
];
Timestamp[dateList_List, opts:OptionsPattern[]] := Module[{dateQ, ts},
	{dateQ, {{ts}}} = Reap[DateListQ[dateList, opts], DateList];
	If[dateQ,
		If[
			DateListElementQ[Second][dateList],
			(*Maintain the precision and accuracy of the argument.*)
			ts[[6]] = IntegerPart[ts[[6]]] + FractionalPart[dateList[[6]]];
		];
		Timestamp[DateList -> ts]] /; dateQ
];
Timestamp[dateString_String, opts:OptionsPattern[]] /; DateStringQ[dateString, opts] := Module[{dateQ, ts},
	{dateQ, {{ts}}} = Reap[DateStringQ[dateString, opts], DateList];
	If[dateQ, Timestamp[DateList -> ts]] /; dateQ
];
Timestamp[{dateString_String, dateElements_ /; VectorQ[dateElements, StringQ]}, opts:OptionsPattern[]] :=  Module[{dateQ, ts},
	{dateQ, {{ts}}} = Reap[DateStringQ[{dateString, dateElements}, opts], DateList];
	If[dateQ, Timestamp[DateList -> ts]] /; dateQ
];

(*
Timestamp /: AbsoluteTime[timestamp_Timestamp?TimestampQ] := AbsoluteTime /. Cases[List @@ timestamp[[1]], HoldPattern[Evaluate[AbsoluteTimePattern]]];
Timestamp /: DateList[timestamp_Timestamp?TimestampQ, opts:OptionsPattern[]] := DateList[AbsoluteTime[timestamp], FilterRules[{opts}, Options[AbsoluteTime]]];
Timestamp /: DateString[timestamp_Timestamp?TimestampQ, opts:OptionsPattern[]] := DateString[AbsoluteTime[timestamp], FilterRules[{opts}, Options[AbsoluteTime]]];
Timestamp /: DateString[timestamp_Timestamp?TimestampQ, dateElement_String, opts:OptionsPattern[]] := DateString[AbsoluteTime[timestamp], FilterRules[{opts}, Options[DateString]]];
Timestamp /: DateString[timestamp_Timestamp?TimestampQ, dateElements_List /; VectorQ[dateElements, StringQ], opts:OptionsPattern[]] := DateString[AbsoluteTime[timestamp], FilterRules[{opts}, Options[DateString]]];
*)

Timestamp /: AbsoluteTime[timestamp_Timestamp?TimestampQ, opts:OptionsPattern[]] := AbsoluteTime[DateList[timestamp], FilterRules[{opts}, Options[AbsoluteTime]]];
Timestamp /: DateList[timestamp_Timestamp?TimestampQ] := DateList /. Cases[timestamp, HoldPattern[Evaluate[DateList -> DateListPattern]]];
Timestamp /: DateString[timestamp_Timestamp?TimestampQ, opts:OptionsPattern[]] := DateString[DateList[timestamp], FilterRules[{opts}, Options[DateString]]];
Timestamp /: DateString[timestamp_Timestamp?TimestampQ, dateElement_String, opts:OptionsPattern[]] := DateString[DateList[timestamp], dateElement, FilterRules[{opts}, Options[DateString]]];
Timestamp /: DateString[timestamp_Timestamp?TimestampQ, dateElements_List /; VectorQ[dateElements, StringQ], opts:OptionsPattern[]] := DateString[DateList[timestamp], dateElements, FilterRules[{opts}, Options[DateString]]];

(*Protect[Timestamp];*)

DurationPattern = Repeated[Duration -> {_?RealNumberQ, _String}, 1];
Protect[DurationPattern];

(*TimeIntervalPattern = {_?TimestampQ, _?TimestampQ}..;*)
TimeIntervalPattern = Repeated[{_?TimestampQ, _?TimestampQ}, 1];
Protect[TimeIntervalPattern];

Attributes[TimeIntervalQ] = {};
(*TimeIntervalQ[TimeInterval[expr_]] /; MatchQ[{ReleaseHold[expr]}, {TimeIntervalPattern, DurationPattern}] := True;*)
TimeIntervalQ[TimeInterval[TimeIntervalPattern, DurationPattern]] := True;
TimeIntervalQ[___] := False;
Protect[TimeIntervalQ];

TimeInterval::arg = "Argument `1` cannot be interpreted as a Timestamp";

(*
Options[TimeInterval] = {Orderless, Flat};

TimeInterval[expr:{_, _}.., opts:OptionsPattern[]] := Module[{ts, timestamps, succeeded, failed, interval, durations, duration},
	Check[
		{timestamps, {succeeded, failed}} = Reap[Map[(ts = #; If[!TimestampQ[ts], ts = Timestamp[#, opts]]; Sow[#, TimestampQ[ts]]; If[TimestampQ[ts], ts, #])&, {expr}, {2}], {True, False}];
		succeeded = Flatten[succeeded];
		failed = Flatten[failed];
		
		If[
			Length[failed] > 0,
			(*Report the errors to the user.*)
			Map[Message[TimeInterval::arg, #]&, failed],
			interval = Interval @@ Map[AbsoluteTime, timestamps, {2}];
			timestamps = Map[Timestamp, List @@ interval, {2}];
			durations = Map[DateDifference[AbsoluteTime[#[[1]]], AbsoluteTime[#[[2]]], "Hour"]&, timestamps];
			duration = {Total[durations[[All, 1]]], durations[[1, 2]]};
			TimeInterval[Hold[Evaluate[Sequence @@ timestamps, Duration -> duration]]]
		] , $Failed, TimeInterval::arg
	] 
];
TimeInterval[interval_Interval, opts:OptionsPattern[]] := TimeInterval[Sequence @@ interval, opts];
TimeInterval /: Interval[timeInterval_TimeInterval] := Interval @@ Map[AbsoluteTime[#] &, Cases[timeInterval[[1]], TimeIntervalPattern], {2}];
*)

TimeInterval[{expr0_ /; !TimestampQ[expr0], expr1_ /; !TimestampQ[expr1]}] := Module[{t0 = Timestamp[expr0], t1 = Timestamp[expr1]},
	Check[
		If[!TimestampQ[t0], Message[TimeInterval::arg, expr0]];
		If[!TimestampQ[t1], Message[TimeInterval::arg, expr1]];
		If[TimestampQ[t0] && TimestampQ[t1], TimeInterval[{t0, t1}]]
		 , $Failed, TimeInterval::arg
	]
];

TimeInterval[{t0_?TimestampQ, expr1_ /; !TimestampQ[expr1]}] := Module[{t1},
	Check[
		t1 = Timestamp[expr1];
		If[!TimestampQ[t1],
			Message[TimeInterval::arg, expr1],
			TimeInterval[{t0, t1}]
		];
		 , $Failed, TimeInterval::arg
	]
];

TimeInterval[{expr0_ /; !TimestampQ[expr0], t1_?TimestampQ}] := TimeInterval[{t1, expr0}];

TimeInterval[{t0_?TimestampQ, t1_?TimestampQ}, opts:OptionsPattern[] /; (*This test stops the recursion.*) FreeQ[{opts}, DurationPattern, {1}]] := Module[{duration},
	duration = DateDifference[DateList[t0], DateList[t1], "Hour"];
	If[duration[[1]] >= 0,
		TimeInterval[{t0, t1}, Duration -> duration],
		duration[[1]] = Abs[duration[[1]]];
		TimeInterval[{t1, t0}, Duration -> duration]
	]
];

TimeInterval /: Interval[timeInterval_TimeInterval /; TimeIntervalQ[timeInterval]] := Interval @@ Map[AbsoluteTime[#] &, Cases[timeInterval, TimeIntervalPattern], {2}];

Duration[timeInterval_?TimeIntervalQ] := Duration /. Cases[timeInterval, HoldPattern[Evaluate[DurationPattern]]];

ValidCenturyInterval = Interval[{0, 99}];
Protect[ValidCenturyInterval];

TimeInterval::century = "Century `1` is not in the interval {0, 99}.";

ValidYearInterval = Interval[{0, Infinity}];
Protect[ValidYearInterval];

DateListWithYearCheck[{year_Integer, month_Integer: 1, day_: 1, hour_: 0, minute_: 0, second_: 0}] /; And @@ Map[RealNumberQ, {day, hour, minute, second}] :=
If[!IntervalMemberQ[ValidYearInterval, year],
	Message[TimeInterval::year, year],
	DateList[{year, month, day, hour, minute, second}]
];

DateListWithCeilingSecondAccuracy[{year_Integer, month_Integer: 1, day_: 1, hour_: 0, minute_: 0, second_: 0}] /; And @@ Map[RealNumberQ, {day, hour, minute, second}] := Module[{t0, uncertainty},
	Check[
		t0 = DateListWithYearCheck[{year, month, day, hour, minute, IntegerPart[second]}];
		t0[[6]] += FractionalPart[second];
		
		(*
		Mathematica is set up so that if a number x has uncertainty d, then it's true value can lie
		in an interval of size d from x-d/2 to x+d/2. Therefore we subtract the uncertainty of
		second from the ceiling of the normalized second.
		*)
		uncertainty = 10^-Accuracy[second];
		Append[t0[[;; 5]], Ceiling[t0[[6]]] - uncertainty]
		, $Failed
	]
];

TimeInterval::year = "Year `1` is not in the interval {0, Infinity}.";

TimeInterval[century_Integer] := Module[{valid, year, t0, t1},
	valid = IntervalMemberQ[ValidCenturyInterval, century];
	If[!valid,
		Message[TimeInterval::century, century],
		year = Max[Min[ValidYearInterval], century 100];
		t0 = DateList[{year}];
		(*The end of the century.*)
		t1 = DateListWithCeilingSecondAccuracy[t0 + {100, 0, 0, 0, 0, 0}];
		TimeInterval[{t0, t1}]
	]
];

TimeInterval[{year_Integer}] := Module[{t0, t1},
	Check[
		t0 = DateListWithYearCheck[{year}];
		(*The end of the year.*)
		t1 = DateListWithCeilingSecondAccuracy[t0 + {1, 0, 0, 0, 0, 0}];
		TimeInterval[{t0, t1}]
		, $Failed
	]
];

TimeInterval[{year_Integer, month_Integer}] := Module[{t0, t1},
	Check[
		t0 = DateListWithYearCheck[{year, month}];
		(*The end of the month.*)
		t1 = DateListWithCeilingSecondAccuracy[t0 + {0, 1, 0, 0, 0, 0}];
		TimeInterval[{t0, t1}]
		, $Failed
	]
];

TimeInterval[{year_Integer, month_Integer, day_?RealNumberQ}] := Module[{t0, t1},
	Check[
		t0 = DateListWithYearCheck[{year, month, day}];
		(*The end of the day.*)
		t1 = DateListWithCeilingSecondAccuracy[t0 + {0, 0, 1, 0, 0, 0}];
		TimeInterval[{t0, t1}]
		, $Failed
	]
];

TimeInterval[{year_Integer, month_Integer, day_?RealNumberQ, hour_?RealNumberQ}] := Module[{t0, t1},
	Check[
		t0 = DateListWithYearCheck[{year, month, day, hour}];
		(*The end of the hour.*)
		t1 = DateListWithCeilingSecondAccuracy[t0 + {0, 0, 0, 1, 0, 0}];
		TimeInterval[{t0, t1}]
		, $Failed
	]
];

TimeInterval[{year_Integer, month_Integer, day_?RealNumberQ, hour_?RealNumberQ, minute_?RealNumberQ}] := Module[{t0, t1},
	Check[
		t0 = DateListWithYearCheck[{year, month, day, hour, minute}];
		(*The end of the minute.*)
		t1 = DateListWithCeilingSecondAccuracy[t0 + {0, 0, 0, 0, 1, 0}];
		TimeInterval[{t0, t1}]
		, $Failed
	]
];

TimeInterval[{year_Integer, month_Integer, day_?RealNumberQ, hour_?RealNumberQ, minute_?RealNumberQ, second_?RealNumberQ}] := Module[{t0, t1},
	Check[
		t0 = DateListWithYearCheck[{year, month, day, hour, minute, second}];
		t0[[6]] = IntegerPart[t0[[6]]] + FractionalPart[second];
		(*The end of the second.*)
		t1 = DateListWithCeilingSecondAccuracy[t0];
		TimeInterval[{t0, t1}]
		, $Failed
	]
];

Protect[TimeInterval];

End[];

EndPackage[];
