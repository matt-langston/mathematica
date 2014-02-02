BeginPackage["Maven`", {"JLink`"}];

PomXmlFilePathQ::usage    = "";
AddMavenClassPath::usage  = "";

MavenSrcDir::usage        = "";
MavenCommand::usage       = "";
MavenClassPath::usage     = "";
MavenPathSeparator::usage = "";

Begin["`Private`"];

PomXmlFilePathQ[___] := False;
PomXmlFilePathQ[filePath_String] := StringMatchQ["pom.xml", FileNameTake[filePath]];

MavenSrcDir[pomXmlFilePath_?PomXmlFilePathQ] := Module[{projectDir},
	projectDir = DirectoryName[pomXmlFilePath];
	(* TODO: Read this from running Maven. *)
	FileNameJoin[{projectDir, "src/main/java"}]
];

MavenCommand[pomXmlFilePath_?PomXmlFilePathQ, mavenCommand_String] :=
Module[{mavenOutput},
	SetDirectory[DirectoryName[pomXmlFilePath]];
	mavenOutput = ReadList["!" <> mavenCommand, Record];
	ResetDirectory[];
	mavenOutput
];

MavenClassPath[pomXmlFilePath_?PomXmlFilePathQ] :=
Module[{mavenOutput, mavenOutputPattern, mavenOutputPositionList, position},
	mavenOutput             = MavenCommand[pomXmlFilePath, "mvn dependency:build-classpath"];
	mavenOutputPattern      = StartOfString ~~ __ ~~ "Dependencies classpath:" ~~ EndOfString;
	mavenOutputPositionList = Position[StringCases[mavenOutput, mavenOutputPattern], {_String}];
	SameQ[Length[mavenOutputPositionList], 1] || Abort[];
	position = mavenOutputPositionList /. {{position_Integer}} :> position;
	Length[mavenOutput] > position || Abort[];
	(*The classpath is the next line immediately after the line matching mavenOutputPattern.*)
	mavenOutput[[position + 1]]
];

MavenPathSeparator[pomXmlFilePath_?PomXmlFilePathQ] :=
Module[{mavenOutput, mavenOutputPattern, pathSeparator, mavenOutputPositionList, position},
	mavenOutput = MavenCommand[pomXmlFilePath, "mvn help:system"];
	mavenOutputPattern = StartOfString ~~ "path.separator=" ~~ pathSeparator : _ ~~ EndOfString;
	mavenOutputPositionList = Position[StringCases[mavenOutput, mavenOutputPattern], {_String}];
	SameQ[Length[mavenOutputPositionList], 1] || Abort[];
	position = mavenOutputPositionList /. {{position_Integer}} :> position;
	Length[mavenOutput] > position || Abort[];
	(*The classpath is the next line immediately after the line matching mavenOutputPattern.*)
	pathSeparator = StringCases[mavenOutput[[position]], mavenOutputPattern :> pathSeparator] /. {pathSeparator_} :> pathSeparator;
	MavenPathSeparator[pomXmlFilePath] = pathSeparator
];

AddMavenClassPath[pomXmlFilePath_?PomXmlFilePathQ] :=
Module[{mavenClassPathString, mavenPathSeparator, targetClassesPath, targetTestClassesPath, mavenClassPath}, mavenClassPathString = MavenClassPath[pomXmlFilePath];
	mavenPathSeparator = MavenPathSeparator[pomXmlFilePath];
	(*TODO:Read this from running Maven.*)
	targetClassesPath = FileNameJoin[{DirectoryName[pomXmlFilePath], "target/classes"}];
	(*TODO:Read this from running Maven.*)
	targetTestClassesPath = FileNameJoin[{DirectoryName[pomXmlFilePath], "target/test-classes"}];
	mavenClassPath = Join[StringSplit[mavenClassPathString, mavenPathSeparator], {targetClassesPath, targetTestClassesPath}];
	AddToClassPath@mavenClassPath
];
   
End[];

EndPackage[];
