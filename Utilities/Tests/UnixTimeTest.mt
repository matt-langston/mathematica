(* Mathematica Test File *)

Needs["Utilities`UnixTime`"];

Test[
	StringMatchQ["January 1, 1970, 00:00:00 UTC", $UnixEpochDateString]
	,
	True
	,
	TestID -> "UnixTimeTest-20140103-A2L0R9"
]

Test[
	MatchQ[{1969, 12, 31, 16, 0, 0}, $UnixEpoch]
	,
	True
	,
	TestID -> "UnixTimeTest-20140103-R7H6U9"
]