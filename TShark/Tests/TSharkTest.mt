(* Mathematica Test File *)

Needs["TShark`"];

Test[
	MatchQ[Length[TSharkFieldList[]], 126736]
	,
	True
	,
	TestID -> "TSharkTest-20140114-I4F9Q0"
]

Test[
	SameQ[Length[TSharkFieldList[]], Length[TSharkProtocolList[]] + Length[TSharkHeaderFieldList[]]]
	,
	True
	,
	TestID -> "TSharkTest-20140114-I2M7M0"
]