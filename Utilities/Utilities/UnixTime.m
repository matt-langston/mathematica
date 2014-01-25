(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["Utilities`UnixTime`", {"Units`"}];

UnixTime::usage               = "";

$UnixEpochDateString::usage   = "";
$UnixEpoch::usage             = "";
$UnixEpochAbsoluteTime::usage = "";

CurrentTimeMillis::usage      = "";


Begin["`Private`"];

$UnixEpochDateString = "January 1, 1970, 00:00:00 UTC";
Protect[$UnixEpochDateString];

(* The UNIX epoch as a DateList in the local time zone. *)

(*$UnixEpoch = DateList[$UnixEpochDateString, TimeZone -> 0];*)
$UnixEpoch = DatePlus[DateList[$UnixEpochDateString], {$TimeZone, "Hour"}];
Protect[$UnixEpoch];

(* The total number of seconds between the beginning of January 1, 1900 in the local time zone and the UNIX epoch. *)
$UnixEpochAbsoluteTime = AbsoluteTime[$UnixEpoch];
Protect[$UnixEpochAbsoluteTime];

(* I use Round to convert Real to Integer. *)
CurrentTimeMillis[] := Round[(AbsoluteTime[] - $UnixEpochAbsoluteTime) 10^3];
Protect[CurrentTimeMillis];

(* TODO: Add arguments and options to UnixTime to make it accept the same arguments and options as DateList. *)
IntegerStringPattern = DigitCharacter..;
Protect[IntegerStringPattern];

IntegerStringQ[___]           := False;
IntegerStringQ[string_String] := StringMatchQ[string, IntegerStringPattern];

UnixTime /:   DateList[UnixTime[millisecondsSinceEpoch_Integer]] :=   DateList[($UnixEpochAbsoluteTime + millisecondsSinceEpoch 10^-3) // N];
UnixTime /: DateString[UnixTime[millisecondsSinceEpoch_Integer]] := DateString[($UnixEpochAbsoluteTime + millisecondsSinceEpoch 10^-3) // N];

UnixTime /:   DateList[UnixTime[millisecondsSinceEpoch_String /; IntegerStringQ[millisecondsSinceEpoch]]] :=   DateList[UnixTime[ToExpression[millisecondsSinceEpoch]]];
UnixTime /: DateString[UnixTime[millisecondsSinceEpoch_String /; IntegerStringQ[millisecondsSinceEpoch]]] := DateString[UnixTime[ToExpression[millisecondsSinceEpoch]]];


End[];

EndPackage[];
