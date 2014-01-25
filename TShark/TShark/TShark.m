(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["TShark`"]

TSharkPortScan::usage        = "";

TSharkFieldList::usage       = "";
TSharkProtocolList::usage    = "";
TSharkHeaderFieldList::usage = "";

(* Options *)
CapturePacketCount::usage    = "";

TSharkPath::usage            = "";
DefaultTSharkPath::usage     = "";

Begin["`Private`"]

DefaultTSharkPath      = "/usr/local/bin/tshark";

Options[TSharkFieldList] = {
	TSharkPath :> DefaultTSharkPath
}

Options[TSharkProtocolList] = {
	TSharkPath :> DefaultTSharkPath
}

Options[TSharkHeaderFieldList] = {
	TSharkPath :> DefaultTSharkPath
}

(*TSharkPortScan[opts:OptionsPattern[]] := Module[{tsharkPath, tsharkFieldList},
]
*)
TSharkFieldList[opts:OptionsPattern[]] := Module[{tsharkPath, tsharkFieldList},
	tsharkPath      = OptionValue[TSharkPath];
	tsharkFieldList = ReadList["!" <> tsharkPath <> " -G fields", Word, RecordLists -> True, WordSeparators -> "\t"];
	(* TODO: cache result *)
	tsharkFieldList
 ]
  
TSharkProtocolList[opts:OptionsPattern[]] := Select[TSharkFieldList[FilterRules[{opts}, Options[TSharkFieldList]]], StringMatchQ[#[[1]], "P"] &];

TSharkHeaderFieldList[opts:OptionsPattern[]] := Select[TSharkFieldList[FilterRules[{opts}, Options[TSharkFieldList]]], StringMatchQ[#[[1]], "F"] &];

End[]

EndPackage[]

