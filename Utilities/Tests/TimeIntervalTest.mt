(* Mathematica Test File *)

Needs["Utilities`UnixTime`"];
Needs["Utilities`TimeInterval`"];

interval = Interval[DateList[{"May 12 2010 07:00:00", "May 12 2010 08:00:00"}]];

Test[
	StringMatchQ["January 1, 1970, 00:00:00 UTC", $UnixEpochDateString]
	,
	True
	,
	TestID -> "IntervalTests-20140103-E0T2H3"
]