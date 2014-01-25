(* Mathematica Test File *)

Needs["Ubuntu`Top`"];

Test[
	Tally[Map[MemoryQ[Quantity[1, #]] &, {"B", "KB", "MB", "GB"}]]
	,
	{{True, 4}}
	,
	TestID -> "Top-20140122-X2F3F3"
]

Test[
	Tally[Map[MemoryQ[Quantity[1, #]] &, {"B", "kB", "mB", "gB"}]]
	,
	{{True, 4}}
	,
	TestID -> "Top-20140122-Y6W2B8"
]