(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["Utilities`Debug`"];

ProjectDirectory::usage = "ProjectDirectory[context] returns the file \
path to the Wolfram Workbench project for the given context, or the \
current context if context is unspecified.";

ContextTake::usage = "ContextTake[context] gives the last context \
element in the given context.\
\
ContextTake[context, n] gives the first n context elements in the \
given context.\
\
ContextTake[context, -n] gives the last n context elements in the \
given context.\
\
ContextTake[context, {m, n}] gives context elements m through n in \
the given context.";

ContextSplit::usage = "ContextSplit[context] splits the given context \
into a list of individual context elements.";

ContextJoin::usage = "ContextJoin[{context_1, context_2, ...}] joins \
the context_i together into a single context.";

ContextQ::usage = "ContextQ[expr] yields True if expr is a context, \
and False otherwise.";

ContextElementQ::usage = "ContextElementQ[expr] yields True if expr \
evaluates to a string that is a legal name for a context, and False \
otherwise.";

ContextSeparator::usage = "ContextSeparator is the character that \
separates individual context names (i.e. the backquote character).";

Begin["`Private`"];

ContextSeparator = "`";
Protect[ContextSeparator];

ContextStringPattern = ___ ~~ "`" ~~ EndOfString;
Protect[ContextStringPattern];

ContextQ[expr_String /; StringMatchQ[expr, ContextStringPattern]] := Module[{symbol},
	Check[symbol = Unique[expr <> "x"]; Remove[symbol]; True, False]
];
ContextQ[___] := False;
Protect[ContextQ];

ContextElementQ[expr_String /; StringFreeQ[expr, ContextSeparator]] := Module[{symbol},
	Check[symbol = Unique["`Private`" <> expr]; Remove[symbol]; True, False]
];
ContextElementQ[___] := False;
Protect[ContextElementQ];

ProjectDirectory[expr_?ContextQ] := Module[{contextFilePath = FindFile[expr], pos},
	MatchQ[contextFilePath, $Failed] && Throw[contextFilePath];
(*
	pos = StringPosition[contextFilePath, FileNameJoin[ContextSplit[expr]], 1];
	StringTake[contextFilePath, pos[[1, 1]] - 1]
*)
	pos = StringPosition[contextFilePath, FileNameJoin[ContextSplit[expr]]];
	StringTake[contextFilePath, pos[[-1, 1]] - 1]
];
ProjectDirectory[] := ProjectDirectory[ContextTake[$Context, {1, -2}]];
Protect[ProjectDirectory];

ContextTake[expr_?ContextQ]                         := ContextJoin @ Take[ContextSplit[expr], -1];
ContextTake[expr_?ContextQ, n_Integer]              := ContextJoin @ Take[ContextSplit[expr], n];
ContextTake[expr_?ContextQ, {m_Integer, n_Integer}] := ContextJoin @ Take[ContextSplit[expr], {m, n}];
Protect[ContextTake];

ContextSplit[expr_?ContextQ] := StringSplit[expr, ContextSeparator];
Protect[ContextSplit];

ContextJoin[contextList:{(_?ContextQ | _?ContextElementQ)..}] :=
	StringJoin @ Append[Riffle[Flatten@MapAt[ContextSplit, contextList, Position[contextList, _?ContextQ]], ContextSeparator], ContextSeparator];
Protect[ContextJoin];

With[{symbolName = ContextTake[$Context, {1, -2}] <> "$ProjectDirectory"},
	!NameQ[symbolName] && (Evaluate[Symbol[symbolName]] = ProjectDirectory[]);
];

End[];

EndPackage[];
