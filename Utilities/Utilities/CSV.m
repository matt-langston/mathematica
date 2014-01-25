(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["Utilities`CSV`"];

MakeCsvIndex::usage = "MakeCsvIndex[] returns a lookup function that \
takes a string pattern as its only argument and returns the index of \
an element in header that matches the pattern.";

Begin["`Private`"];

MakeCsvIndex[header_List /; VectorQ[header, StringQ]] := Function[columnNamePattern, Position[StringCases[header, StartOfString ~~ # ~~ EndOfString], {#}] & /@ Flatten[{columnNamePattern}] // Flatten // Replace[#, {index_Integer} :> index] &];
Protect[MakeCsvIndex];

End[];

EndPackage[];
