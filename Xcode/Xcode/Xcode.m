(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["Xcode`"]

XcodePath::usage                = "";
XcodeSdkPath::usage             = "";
XcodeSdkVersion::usage          = "";
XcodeSdkPlatformPath::usage     = "";

XcodeSdkPlatformVersion::usage  = "";
XcodeSdkList::usage             = "";
XcodeSdkNameQ::usage            = "";
XcodeSdkPath::usage             = "";

XcodeFrameworksPath::usage      = "";
XcodeFrameworkPathList::usage   = "";
XcodeFrameworkNameQ::usage      = "";
XcodeFrameworkHeaderList::usage = "";

Begin["`Private`"]
Needs["Utilities`Logging`"];

XcodePath[]               := ReadList["!xcode-select --print-path"        , String][[1]];
XcodeSdkPath[]            := ReadList["!xcrun --show-sdk-path"            , String][[1]];
XcodeSdkVersion[]         := ReadList["!xcrun --show-sdk-version"         , String][[1]];
XcodeSdkPlatformPath[]    := ReadList["!xcrun --show-sdk-platform-path"   , String][[1]];
XcodeSdkPlatformVersion[] := ReadList["!xcrun --show-sdk-platform-version", String][[1]];

XcodeSdkList[] := Module[{showsdksPattern, sdkDescription, sdkName},
	showsdksPattern = Whitespace ~~ sdkDescription : Shortest[__] ~~ Whitespace ~~ "-sdk" ~~ Whitespace ~~ sdkName : Except[WhitespaceCharacter] .. ~~ ___;
	Flatten[Select[StringCases[ReadList["!xcodebuild -showsdks", String], showsdksPattern :> {sdkName, sdkDescription}], !MatchQ[#, {}] &], 1]
];
   
XcodeSdkNameQ[___]                 := False;
XcodeSdkNameQ[xcodeSdkName_String] := MemberQ[XcodeSdkList[][[All, 1]], xcodeSdkName];

XcodeSdkPath[xcodeSdkName_?XcodeSdkNameQ] :=
XcodeSdkPath[xcodeSdkName]                 =
ReadList["!xcrun -sdk " <> xcodeSdkName <> " --show-sdk-path", String][[1]];

XcodeFrameworksPath[]                            := FileNameJoin[{XcodeSdkPath[]            , "/System/Library/Frameworks"}];
XcodeFrameworksPath[xcodeSdkName_?XcodeSdkNameQ] := FileNameJoin[{XcodeSdkPath[xcodeSdkName], "/System/Library/Frameworks"}];

XcodeFrameworkPathList[]                            := FileNames["*.framework", {XcodeFrameworksPath[]}            ];
XcodeFrameworkPathList[xcodeSdkName_?XcodeSdkNameQ] :=
XcodeFrameworkPathList[xcodeSdkName]                 = FileNames["*.framework", {XcodeFrameworksPath[xcodeSdkName]}];

XcodeFrameworkNameQ[___]                                                    := False;
XcodeFrameworkNameQ[xcodeFrameworkName_String]                              := MemberQ[Map[FileBaseName, XcodeFrameworkPathList[]            ], xcodeFrameworkName];
XcodeFrameworkNameQ[xcodeFrameworkName_String, xcodeSdkName_?XcodeSdkNameQ] := MemberQ[Map[FileBaseName, XcodeFrameworkPathList[xcodeSdkName]], xcodeFrameworkName];

XcodeFrameworkHeaderList[xcodeFrameworkName_?XcodeFrameworkNameQ] :=
XcodeFrameworkHeaderList[xcodeFrameworkName] =
FileNames["*", FileNameJoin[{XcodeFrameworksPath[], xcodeFrameworkName <> ".framework", "Headers"}]];

XcodeFrameworkHeaderList[xcodeFrameworkName_String, xcodeSdkName_?XcodeSdkNameQ] /; XcodeFrameworkNameQ[xcodeFrameworkName, xcodeSdkName] :=
XcodeFrameworkHeaderList[xcodeFrameworkName] =
FileNames["*", FileNameJoin[{XcodeFrameworksPath[xcodeSdkName], xcodeFrameworkName <> ".framework", "Headers"}]];

End[]

EndPackage[]
