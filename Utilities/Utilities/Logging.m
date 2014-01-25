(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["Utilities`Logging`"];

DebugMessage::usage        = "";

DebugDateString::usage     = "";
DebugDateTimeFormat::usage = "";


Begin["`Private`"];

DebugDateTimeFormat = {"DayNameShort", " ", "Day", " ", "MonthNameShort", " ", "Year", " ", "Hour", ":", "Minute", ":", "Second", ".", "Millisecond"};

DebugDateString[] := DateString[DebugDateTimeFormat];

Attributes[DebugMessage] = {HoldFirst};
DebugMessage[messageName_MessageName, expressions___] := Message[messageName, DebugDateString[], expressions];


End[];

EndPackage[];
