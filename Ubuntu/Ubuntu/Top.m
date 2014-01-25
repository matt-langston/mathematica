(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["Ubuntu`Top`",
	{
		"Utilities`TextFile`",
		"Utilities`TimeInterval`"
	}
]

(*
This Mathematica package can parse the output of top when run like this: 

top -b | tee top_$(date +%s).txt
*)

TopDataFilePathPatern::usage              = "";
TopDataFilePathQ::usage                   = "";
TopDataFileTimestamp::usage               = "";

TopSummaryPlot::usage                     = "";
LoadAveragePlot::usage                    = "";
TotalProcessesPlot::usage                 = "";
CPUPercentPlot::usage                     = "";
PhysicalMemoryPlot::usage                 = "";
SwapMemoryPlot::usage                     = "";

FindProcessByCommand::usage               = "";
CommandPlot::usage                        = "";

FindAllPIDForCommand::usage               = "";
FindProcessByPID::usage                   = "";
PIDPlot::usage                            = "";

TopLogFileData::usage                     = "";
TopLogFileDataQ::usage                    = "";

NextTopData::usage                        = "";
NextPotentialTopDataLineNumber::usage     = "";

TopData::usage                            = "";
TopDataQ::usage                           = "";


TopSummary::usage                         = "";
TopSummaryQ::usage                        = "";


TopSummaryLine1::usage                    = "";
TopSummaryLine1Q::usage                   = "";
TopSummaryLine1SourceQ::usage             = "";

Uptime::usage                             = "";
UserSessions::usage                       = "";
LoadAverage1Minute::usage                 = "";
LoadAverage5Minute::usage                 = "";
LoadAverage15Minute::usage                = "";


TopSummaryLine2::usage                    = "";
TopSummaryLine2Q::usage                   = "";
TopSummaryLine2SourceQ::usage             = "";

TotalProcesses::usage                     = "";
TotalRunningProcesses::usage              = "";
TotalSleepingProcesses::usage             = "";
TotalStoppedProcesses::usage              = "";
TotalZombieProcesses::usage               = "";

TopSummaryLine3::usage                    = "";
TopSummaryLine3Q::usage                   = "";
TopSummaryLine3SourceQ::usage             = "";

CPUPercentForUserProcesses::usage         = "";
CPUPercentForSystemProcesses::usage       = "";
CPUPercentWithPriorityUpgrade::usage      = "";
CPUPercentNotUsed::usage                  = "";
CPUPercentForProcessesWaitingForIO::usage = "";
CPUPercentServingHardwareInterupts::usage = "";
CPUPercentServingSoftwareInterupts::usage = "";
CPUPercentStolenByHypervisor::usage       = "";


TopSummaryLine4::usage                    = "";
TopSummaryLine4Q::usage                   = "";
TopSummaryLine4SourceQ::usage             = "";

TotalPhysicalMemory::usage                = "";
UsedPhysicalMemory::usage                 = "";
FreePhysicalMemory::usage                 = "";
BufferPhysicalMemory::usage               = "";


TopSummaryLine5::usage                    = "";
TopSummaryLine5Q::usage                   = "";
TopSummaryLine5SourceQ::usage             = "";

TotalSwapMemory::usage                    = "";
UsedSwapMemory::usage                     = "";
FreeSwapMemory::usage                     = "";
CachedSwapMemory::usage                   = "";


ProcessList::usage                        = "";
ProcessListQ::usage                       = "";
ProcessListItemList::usage                = "";

ProcessListItem::usage                    = "";
ProcessListItemQ::usage                   = "";
ProcessListItemSourceQ::usage             = "";
ProcessListItemVectorQ::usage             = "";

PID::usage                                = "";
User::usage                               = "";
Priority::usage                           = "";
Nice::usage                               = "";
VirtualMemory::usage                      = "";
PhysicalMemory::usage                     = "";
SharedMemory::usage                       = "";
Status::usage                             = "";
PercentCPU::usage                         = "";
PercentMemory::usage                      = "";
TotalCPUTime::usage                       = "";
Command::usage                            = "";

DurationQ::usage                          = "";
MemoryQ::usage                            = "";

Begin["`Private`"]
Needs["Utilities`UnixTime`"];
Needs["Calendar`"];

DurationQ[___]               := False;
DurationQ[quantity_Quantity] := MatchQ[quantity, Quantity[_?NumberQ, ("Days" | "Hours" | "Minutes" | "Seconds")]];

MemoryQ[___]                 := False;
MemoryQ[quantity_Quantity]   := MatchQ[quantity, Quantity[_?NumberQ, ("Bytes" | "Kilobytes" | "Megabytes" | "Gigabytes")]];

(*
top - 19:27:46 up 8 days, 20:22,  6 users,  load average: 1.73, 1.53, 1.26
Tasks: 304 total,   2 running, 288 sleeping,   0 stopped,  14 zombie
Cpu(s): 18.2%us, 12.7%sy,  0.0%ni, 61.9%id,  0.2%wa,  0.0%hi,  6.7%si,  0.3%st
Mem:   7629464k total,  7583024k used,    46440k free,   768092k buffers
Swap:        0k total,        0k used,        0k free,  5395704k cached

top - 22:38:42 up 7 days, 23:33,  5 users,  load average: 1.28, 1.45, 1.34
Tasks: 240 total,   1 running, 225 sleeping,   0 stopped,  14 zombie
Cpu(s):  0.3%us,  0.2%sy,  0.0%ni, 99.4%id,  0.1%wa,  0.0%hi,  0.0%si,  0.0%st
Mem:   7629464k total,  7296912k used,   332552k free,   414148k buffers
Swap:        0k total,        0k used,        0k free,  5562092k cached

  PID USER      PR  NI  VIRT  RES  SHR S %CPU %MEM    TIME+  COMMAND                                                                                                                                            
14728 ubuntu    20   0 17472 1368  928 R    2  0.0   0:00.02 top                                                                                                                                                
    1 root      20   0 24456 2424 1340 S    0  0.0   0:03.16 init                                                                                                                                               
    2 root      20   0     0    0    0 S    0  0.0   0:00.03 kthreadd                                                                                                                                           
    3 root      20   0     0    0    0 S    0  0.0   0:12.41 ksoftirqd/0                                                                                                                                        
    6 root      RT   0     0    0    0 S    0  0.0   0:01.20 migration/0                                                                                                                                        
*)

TopSummaryLine1StringPattern =
StartOfString ~~
"top -" ~~
Whitespace ~~ timestamp:Except[WhitespaceCharacter].. ~~ Whitespace ~~
"up" ~~ Whitespace ~~
days:DigitCharacter.. ~~ Whitespace ~~ "days," ~~ Whitespace ~~
hours:Repeated[DigitCharacter, 2] ~~ ":" ~~ minutes:Repeated[DigitCharacter, 2] ~~ "," ~~ Whitespace ~~ 
users:DigitCharacter.. ~~ Whitespace ~~ "users," ~~ Whitespace ~~
"load average:" ~~ Whitespace ~~ 
loadAverage1:NumberString ~~ "," ~~ Whitespace ~~ 
loadAverage2:NumberString ~~ "," ~~ Whitespace ~~ 
loadAverage3:NumberString ~~
EndOfString;
Protect[TopSummaryLine1StringPattern];

TopSummaryLine1SourceQ[___]                         := False;
TopSummaryLine1SourceQ[textFileLine_?TextFileLineQ] :=
TopSummaryLine1SourceQ[textFileLine               ]  = StringMatchQ[LineText[textFileLine], TopSummaryLine1StringPattern];

TopSummaryLine1Pattern =
PatternSequence[
	Timestamp           -> _?DateQ,
	Uptime              -> _?DurationQ,
	UserSessions        -> _Integer,
	LoadAverage1Minute  -> _Real,
	LoadAverage5Minute  -> _Real,
	LoadAverage15Minute -> _Real,
	TextFileLine        -> _?TextFileLineQ
];
Protect[TopSummaryLine1Pattern];

TopSummaryLine1Q[___]                                                    := False;
TopSummaryLine1Q[object:TopSummaryLine1[pattern:TopSummaryLine1Pattern]] :=
TopSummaryLine1Q[object                                                ]  = True;

TopSummaryLine1[textFileLine_?TextFileLineQ /; TopSummaryLine1SourceQ[textFileLine]] :=
Module[{data, object},
	data = StringCases[LineText[textFileLine], TopSummaryLine1StringPattern :> {
			Timestamp           -> DatePlus[DateList[timestamp], {$TimeZone, "Hour"}],
			Uptime              -> Quantity[ToExpression[days], "Days"] + Quantity[ToExpression[hours], "Hours"] + Quantity[ToExpression[minutes], "Minutes"],
			UserSessions        -> ToExpression[users],
			LoadAverage1Minute  -> ToExpression[loadAverage1],
			LoadAverage5Minute  -> ToExpression[loadAverage2],
			LoadAverage15Minute -> ToExpression[loadAverage3]
		}
	];
		
	object = data /. rest : {{__}} :> TopSummaryLine1[Sequence @@ Flatten[rest, 1], TextFileLine -> textFileLine];
	TopSummaryLine1[textFileLine] = object
];

TopSummaryLine1[
	timestamp_?DateQ,
	uptime_?DurationQ,
	userSessions_Integer,
	loadAverage1Minute_Real,
	loadAverage5Minute_Real,
	loadAverage15Minute_Real,
	textFileLine_?TextFileLineQ
] :=
TopSummaryLine1[
	timestamp,
	uptime,
	userSessions,
	loadAverage1Minute,
	loadAverage5Minute,
	loadAverage15Minute,
	textFileLine
] =
TopSummaryLine1[
	Timestamp           -> timestamp,
	Uptime              -> uptime,
	UserSessions        -> userSessions,
	LoadAverage1Minute  -> loadAverage1Minute,
	LoadAverage5Minute  -> loadAverage5Minute,
	LoadAverage15Minute -> loadAverage15Minute,
	TextFileLine  -> textFileLine
];

TopSummaryLine1 /:          LineNumber[object:TopSummaryLine1[pattern:TopSummaryLine1Pattern]] :=          LineNumber[object] = LineNumber[TextFileLine[object]];
TopSummaryLine1 /:           Timestamp[object:TopSummaryLine1[pattern:TopSummaryLine1Pattern]] :=           Timestamp[object] = Timestamp           /. {pattern};
TopSummaryLine1 /:              Uptime[object:TopSummaryLine1[pattern:TopSummaryLine1Pattern]] :=              Uptime[object] = Uptime              /. {pattern};
TopSummaryLine1 /:        UserSessions[object:TopSummaryLine1[pattern:TopSummaryLine1Pattern]] :=        UserSessions[object] = UserSessions        /. {pattern};
TopSummaryLine1 /:  LoadAverage1Minute[object:TopSummaryLine1[pattern:TopSummaryLine1Pattern]] :=  LoadAverage1Minute[object] = LoadAverage1Minute  /. {pattern};
TopSummaryLine1 /:  LoadAverage5Minute[object:TopSummaryLine1[pattern:TopSummaryLine1Pattern]] :=  LoadAverage5Minute[object] = LoadAverage5Minute  /. {pattern};
TopSummaryLine1 /: LoadAverage15Minute[object:TopSummaryLine1[pattern:TopSummaryLine1Pattern]] := LoadAverage15Minute[object] = LoadAverage15Minute /. {pattern};
TopSummaryLine1 /:        TextFileLine[object:TopSummaryLine1[pattern:TopSummaryLine1Pattern]] :=        TextFileLine[object] = TextFileLine        /. {pattern};

Format[object:TopSummaryLine1[pattern:TopSummaryLine1Pattern]] := "[top data]:" <> LineText[TextFileLine[object]];


TopSummaryLine2StringPattern = 
StartOfString ~~
"Tasks:" ~~ Whitespace ~~ 
tasks:NumberString ~~ Whitespace ~~ "total," ~~ Whitespace ~~
running:NumberString ~~ Whitespace ~~ "running," ~~ Whitespace ~~
sleeping:NumberString ~~ Whitespace ~~ "sleeping," ~~ Whitespace ~~
stopped:NumberString ~~ Whitespace ~~ "stopped," ~~ Whitespace ~~ 
zombie:NumberString ~~ Whitespace ~~ "zombie" ~~ 
EndOfString;
Protect[TopSummaryLine2StringPattern];

TopSummaryLine2SourceQ[___]                         := False;
TopSummaryLine2SourceQ[textFileLine_?TextFileLineQ] :=
TopSummaryLine2SourceQ[textFileLine               ]  = StringMatchQ[LineText[textFileLine], TopSummaryLine2StringPattern];

TopSummaryLine2Pattern =
PatternSequence[
	TotalProcesses         -> _Integer,
	TotalRunningProcesses  -> _Integer,
	TotalSleepingProcesses -> _Integer,
	TotalStoppedProcesses  -> _Integer,
	TotalZombieProcesses   -> _Integer,
	TextFileLine           -> _?TextFileLineQ
];
Protect[TopSummaryLine2Pattern];

TopSummaryLine2Q[___]                                                    := False;
TopSummaryLine2Q[object:TopSummaryLine2[pattern:TopSummaryLine2Pattern]] :=
TopSummaryLine2Q[object                                                ]  = True;

TopSummaryLine2[textFileLine_?TextFileLineQ /; TopSummaryLine2SourceQ[textFileLine]] :=
Module[{data, object},
	data = StringCases[LineText[textFileLine], TopSummaryLine2StringPattern :> {
			TotalProcesses         -> ToExpression[tasks],
			TotalRunningProcesses  -> ToExpression[running],
			TotalSleepingProcesses -> ToExpression[sleeping],
			TotalStoppedProcesses  -> ToExpression[stopped],
			TotalZombieProcesses   -> ToExpression[zombie]
		}
	];
		
	object = data /. rest : {{__}} :> TopSummaryLine2[Sequence @@ Flatten[rest, 1], TextFileLine -> textFileLine];
	TopSummaryLine2[textFileLine] = object
];

TopSummaryLine2[
	tasks_Integer,
	running_Integer,
	sleeping_Integer,
	stopped_Integer,
	zombie_Integer,
	textFileLine_?TextFileLineQ
] :=
TopSummaryLine2[
	tasks,
	running,
	sleeping,
	stopped,
	zombie,
	textFileLine
] =
TopSummaryLine2[
	TotalProcesses         -> tasks,
	TotalRunningProcesses  -> running,
	TotalSleepingProcesses -> sleeping,
	TotalStoppedProcesses  -> stopped,
	TotalZombieProcesses   -> zombie,
	TextFileLine           -> textFileLine
];

TopSummaryLine2 /:             LineNumber[object:TopSummaryLine2[pattern:TopSummaryLine2Pattern]] :=             LineNumber[object] = LineNumber[TextFileLine[object]];
TopSummaryLine2 /:         TotalProcesses[object:TopSummaryLine2[pattern:TopSummaryLine2Pattern]] :=         TotalProcesses[object] = TotalProcesses         /. {pattern};
TopSummaryLine2 /:  TotalRunningProcesses[object:TopSummaryLine2[pattern:TopSummaryLine2Pattern]] :=  TotalRunningProcesses[object] = TotalRunningProcesses  /. {pattern};
TopSummaryLine2 /: TotalSleepingProcesses[object:TopSummaryLine2[pattern:TopSummaryLine2Pattern]] := TotalSleepingProcesses[object] = TotalSleepingProcesses /. {pattern};
TopSummaryLine2 /:  TotalStoppedProcesses[object:TopSummaryLine2[pattern:TopSummaryLine2Pattern]] :=  TotalStoppedProcesses[object] = TotalStoppedProcesses  /. {pattern};
TopSummaryLine2 /:   TotalZombieProcesses[object:TopSummaryLine2[pattern:TopSummaryLine2Pattern]] :=   TotalZombieProcesses[object] = TotalZombieProcesses   /. {pattern};
TopSummaryLine2 /:           TextFileLine[object:TopSummaryLine2[pattern:TopSummaryLine2Pattern]] :=           TextFileLine[object] = TextFileLine           /. {pattern};

Format[object:TopSummaryLine2[pattern:TopSummaryLine2Pattern]] := "[top data]:" <> LineText[TextFileLine[object]];


TopSummaryLine3StringPattern = 
StartOfString ~~
"Cpu(s):" ~~ Whitespace ~~
cpus:NumberString ~~ "%us," ~~ Whitespace ~~
sy:NumberString ~~ "%sy," ~~ Whitespace ~~
ni:NumberString ~~ "%ni," ~~ Whitespace ~~
id:NumberString ~~ "%id," ~~ Whitespace ~~
wa:NumberString ~~ "%wa," ~~ Whitespace ~~
hi:NumberString ~~ "%hi," ~~ Whitespace ~~
si:NumberString ~~ "%si," ~~ Whitespace ~~
st:NumberString ~~ "%st" ~~
EndOfString;
Protect[TopSummaryLine3StringPattern];

TopSummaryLine3SourceQ[___]                         := False;
TopSummaryLine3SourceQ[textFileLine_?TextFileLineQ] :=
TopSummaryLine3SourceQ[textFileLine               ]  = StringMatchQ[LineText[textFileLine], TopSummaryLine3StringPattern];

TopSummaryLine3Pattern =
PatternSequence[
	CPUPercentForUserProcesses         -> _Real,
	CPUPercentForSystemProcesses       -> _Real,
	CPUPercentWithPriorityUpgrade      -> _Real,
	CPUPercentNotUsed                  -> _Real,
	CPUPercentForProcessesWaitingForIO -> _Real,
	CPUPercentServingHardwareInterupts -> _Real,
	CPUPercentServingSoftwareInterupts -> _Real,
	CPUPercentStolenByHypervisor       -> _Real,
	TextFileLine                       -> _?TextFileLineQ
];
Protect[TopSummaryLine3Pattern];

TopSummaryLine3Q[___]                                                    := False;
TopSummaryLine3Q[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] :=
TopSummaryLine3Q[object                                                ]  = True;

TopSummaryLine3[textFileLine_?TextFileLineQ /; TopSummaryLine3SourceQ[textFileLine]] :=
Module[{data, object},
	data = StringCases[LineText[textFileLine], TopSummaryLine3StringPattern :> {
			CPUPercentForUserProcesses         -> ToExpression[cpus],
			CPUPercentForSystemProcesses       -> ToExpression[sy],
			CPUPercentWithPriorityUpgrade      -> ToExpression[ni],
			CPUPercentNotUsed                  -> ToExpression[id],
			CPUPercentForProcessesWaitingForIO -> ToExpression[wa],
			CPUPercentServingHardwareInterupts -> ToExpression[hi],
			CPUPercentServingSoftwareInterupts -> ToExpression[si],
			CPUPercentStolenByHypervisor       -> ToExpression[st]
		}
	];
		
	object = data /. rest : {{__}} :> TopSummaryLine3[Sequence @@ Flatten[rest, 1], TextFileLine -> textFileLine];
	TopSummaryLine3[textFileLine] = object
];

TopSummaryLine3[
	cpus_Real,
	sy_Real,
	ni_Real,
	id_Real,
	wa_Real,
	hi_Real,
	si_Real,
	st_Real,
	textFileLine_?TextFileLineQ
] :=
TopSummaryLine3[
	cpus,
	sy,
	ni,
	id,
	wa,
	hi,
	si,
	st,
	textFileLine
] =
TopSummaryLine3[
	CPUPercentForUserProcesses         -> cpus,
	CPUPercentForSystemProcesses       -> sy,
	CPUPercentWithPriorityUpgrade      -> ni,
	CPUPercentNotUsed                  -> id,
	CPUPercentForProcessesWaitingForIO -> wa,
	CPUPercentServingHardwareInterupts -> hi,
	CPUPercentServingSoftwareInterupts -> si,
	CPUPercentStolenByHypervisor       -> st,
	TextFileLine                       -> textFileLine
];

TopSummaryLine3 /:                          LineNumber[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] :=                        LineNumber[object] = LineNumber[TextFileLine[object]];
TopSummaryLine3 /:         CPUPercentForUserProcesses[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] :=         CPUPercentForUserProcesses[object] = CPUPercentForUserProcesses         /. {pattern};
TopSummaryLine3 /:       CPUPercentForSystemProcesses[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] :=       CPUPercentForSystemProcesses[object] = CPUPercentForSystemProcesses       /. {pattern};
TopSummaryLine3 /:      CPUPercentWithPriorityUpgrade[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] :=      CPUPercentWithPriorityUpgrade[object] = CPUPercentWithPriorityUpgrade      /. {pattern};
TopSummaryLine3 /:                  CPUPercentNotUsed[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] :=                  CPUPercentNotUsed[object] = CPUPercentNotUsed                  /. {pattern};
TopSummaryLine3 /: CPUPercentForProcessesWaitingForIO[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] := CPUPercentForProcessesWaitingForIO[object] = CPUPercentForProcessesWaitingForIO /. {pattern};
TopSummaryLine3 /: CPUPercentServingHardwareInterupts[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] := CPUPercentServingHardwareInterupts[object] = CPUPercentServingHardwareInterupts /. {pattern};
TopSummaryLine3 /: CPUPercentServingSoftwareInterupts[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] := CPUPercentServingSoftwareInterupts[object] = CPUPercentServingSoftwareInterupts /. {pattern};
TopSummaryLine3 /:       CPUPercentStolenByHypervisor[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] :=       CPUPercentStolenByHypervisor[object] = CPUPercentStolenByHypervisor /. {pattern};
TopSummaryLine3 /:                       TextFileLine[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] :=                       TextFileLine[object] = TextFileLine                       /. {pattern};

Format[object:TopSummaryLine3[pattern:TopSummaryLine3Pattern]] := "[top data]:" <> LineText[TextFileLine[object]];


TopSummaryLine4StringPattern = 
StartOfString ~~
"Mem:" ~~ Whitespace ~~
totalMemory:NumberString ~~ "k" ~~ Whitespace ~~ "total," ~~ Whitespace ~~
usedMemory:NumberString ~~ "k" ~~ Whitespace ~~ "used," ~~ Whitespace ~~ 
freeMemory:NumberString ~~ "k" ~~ Whitespace ~~ "free," ~~ Whitespace ~~
bufferMemory:NumberString ~~ "k" ~~ Whitespace ~~ "buffers" ~~
EndOfString;
Protect[TopSummaryLine4StringPattern];

TopSummaryLine4SourceQ[___]                         := False;
TopSummaryLine4SourceQ[textFileLine_?TextFileLineQ] :=
TopSummaryLine4SourceQ[textFileLine               ]  = StringMatchQ[LineText[textFileLine], TopSummaryLine4StringPattern];

TopSummaryLine4Pattern =
PatternSequence[
	TotalPhysicalMemory  -> _?MemoryQ,
	UsedPhysicalMemory   -> _?MemoryQ,
	FreePhysicalMemory   -> _?MemoryQ,
	BufferPhysicalMemory -> _?MemoryQ,
	TextFileLine         -> _?TextFileLineQ
];
Protect[TopSummaryLine4Pattern];

TopSummaryLine4Q[___]                                                    := False;
TopSummaryLine4Q[object:TopSummaryLine4[pattern:TopSummaryLine4Pattern]] :=
TopSummaryLine4Q[object                                                ]  = True;

TopSummaryLine4[textFileLine_?TextFileLineQ /; TopSummaryLine4SourceQ[textFileLine]] :=
Module[{data, object},
	data = StringCases[LineText[textFileLine], TopSummaryLine4StringPattern :> {
			TotalPhysicalMemory  -> Quantity[ToExpression[totalMemory], "Kilobytes"],
			UsedPhysicalMemory   -> Quantity[ToExpression[usedMemory], "Kilobytes"],
			FreePhysicalMemory   -> Quantity[ToExpression[freeMemory], "Kilobytes"],
			BufferPhysicalMemory -> Quantity[ToExpression[bufferMemory], "Kilobytes"]
		}
	];
		
	object = data /. rest : {{__}} :> TopSummaryLine4[Sequence @@ Flatten[rest, 1], TextFileLine -> textFileLine];
	TopSummaryLine4[textFileLine] = object
];

TopSummaryLine4[
	totalMemory_?MemoryQ,
	usedMemory_?MemoryQ,
	freeMemory_?MemoryQ,
	bufferMemory_?MemoryQ,
	textFileLine_?TextFileLineQ
] :=
TopSummaryLine4[
	totalMemory,
	usedMemory,
	freeMemory,
	bufferMemory,
	textFileLine
] =
TopSummaryLine4[
	TotalPhysicalMemory  -> totalMemory,
	UsedPhysicalMemory   -> usedMemory,
	FreePhysicalMemory   -> freeMemory,
	BufferPhysicalMemory -> bufferMemory,
	TextFileLine         -> textFileLine
];

TopSummaryLine4 /:           LineNumber[object:TopSummaryLine4[pattern:TopSummaryLine4Pattern]] :=           LineNumber[object] = LineNumber[TextFileLine[object]];
TopSummaryLine4 /:  TotalPhysicalMemory[object:TopSummaryLine4[pattern:TopSummaryLine4Pattern]] :=  TotalPhysicalMemory[object] = TotalPhysicalMemory  /. {pattern};
TopSummaryLine4 /:   UsedPhysicalMemory[object:TopSummaryLine4[pattern:TopSummaryLine4Pattern]] :=   UsedPhysicalMemory[object] = UsedPhysicalMemory   /. {pattern};
TopSummaryLine4 /:   FreePhysicalMemory[object:TopSummaryLine4[pattern:TopSummaryLine4Pattern]] :=   FreePhysicalMemory[object] = FreePhysicalMemory   /. {pattern};
TopSummaryLine4 /: BufferPhysicalMemory[object:TopSummaryLine4[pattern:TopSummaryLine4Pattern]] := BufferPhysicalMemory[object] = BufferPhysicalMemory /. {pattern};
TopSummaryLine4 /:         TextFileLine[object:TopSummaryLine4[pattern:TopSummaryLine4Pattern]] :=         TextFileLine[object] = TextFileLine         /. {pattern};

Format[object:TopSummaryLine4[pattern:TopSummaryLine4Pattern]] := "[top data]:" <> LineText[TextFileLine[object]];


TopSummaryLine5StringPattern = 
StartOfString ~~ "Swap:" ~~ Whitespace ~~ 
totalMemory:NumberString ~~ "k" ~~ Whitespace ~~ "total," ~~ Whitespace ~~
usedMemory:NumberString ~~ "k" ~~ Whitespace ~~ "used," ~~ Whitespace ~~ 
freeMemory:NumberString ~~ "k" ~~ Whitespace ~~ "free," ~~ Whitespace ~~
cachedMemory:NumberString ~~ "k" ~~ Whitespace ~~ "cached" ~~
EndOfString;
Protect[TopSummaryLine5StringPattern];

TopSummaryLine5SourceQ[___]                         := False;
TopSummaryLine5SourceQ[textFileLine_?TextFileLineQ] :=
TopSummaryLine5SourceQ[textFileLine               ]  = StringMatchQ[LineText[textFileLine], TopSummaryLine5StringPattern];

TopSummaryLine5Pattern =
PatternSequence[
	TotalSwapMemory  -> _?MemoryQ,
	UsedSwapMemory   -> _?MemoryQ,
	FreeSwapMemory   -> _?MemoryQ,
	CachedSwapMemory -> _?MemoryQ,
	TextFileLine     -> _?TextFileLineQ
];
Protect[TopSummaryLine5Pattern];

TopSummaryLine5Q[___]                                                    := False;
TopSummaryLine5Q[object:TopSummaryLine5[pattern:TopSummaryLine5Pattern]] :=
TopSummaryLine5Q[object                                                ]  = True;

TopSummaryLine5[textFileLine_?TextFileLineQ /; TopSummaryLine5SourceQ[textFileLine]] :=
Module[{data, object},
	data = StringCases[LineText[textFileLine], TopSummaryLine5StringPattern :> {
			TotalSwapMemory  -> Quantity[ToExpression[totalMemory], "Kilobytes"],
			UsedSwapMemory   -> Quantity[ToExpression[usedMemory], "Kilobytes"],
			FreeSwapMemory   -> Quantity[ToExpression[freeMemory], "Kilobytes"],
			CachedSwapMemory -> Quantity[ToExpression[cachedMemory], "Kilobytes"]
		}
	];
		
	object = data /. rest : {{__}} :> TopSummaryLine5[Sequence @@ Flatten[rest, 1], TextFileLine -> textFileLine];
	TopSummaryLine5[textFileLine] = object
];

TopSummaryLine5[
	totalMemory_?MemoryQ,
	usedMemory_?MemoryQ,
	freeMemory_?MemoryQ,
	cachedMemory_?MemoryQ,
	textFileLine_?TextFileLineQ
] :=
TopSummaryLine5[
	totalMemory,
	usedMemory,
	freeMemory,
	cachedMemory,
	textFileLine
] =
TopSummaryLine5[
	TotalSwapMemory  -> totalMemory,
	UsedSwapMemory   -> usedMemory,
	FreeSwapMemory   -> freeMemory,
	CachedSwapMemory -> cachedMemory,
	TextFileLine     -> textFileLine
];

TopSummaryLine5 /:       LineNumber[object:TopSummaryLine5[pattern:TopSummaryLine5Pattern]] :=       LineNumber[object] = LineNumber[TextFileLine[object]];
TopSummaryLine5 /:  TotalSwapMemory[object:TopSummaryLine5[pattern:TopSummaryLine5Pattern]] :=  TotalSwapMemory[object] = TotalSwapMemory  /. {pattern};
TopSummaryLine5 /:   UsedSwapMemory[object:TopSummaryLine5[pattern:TopSummaryLine5Pattern]] :=   UsedSwapMemory[object] = UsedSwapMemory   /. {pattern};
TopSummaryLine5 /:   FreeSwapMemory[object:TopSummaryLine5[pattern:TopSummaryLine5Pattern]] :=   FreeSwapMemory[object] = FreeSwapMemory   /. {pattern};
TopSummaryLine5 /: CachedSwapMemory[object:TopSummaryLine5[pattern:TopSummaryLine5Pattern]] := CachedSwapMemory[object] = CachedSwapMemory /. {pattern};
TopSummaryLine5 /:     TextFileLine[object:TopSummaryLine5[pattern:TopSummaryLine5Pattern]] :=     TextFileLine[object] = TextFileLine     /. {pattern};

Format[object:TopSummaryLine5[pattern:TopSummaryLine5Pattern]] := "[top data]:" <> LineText[TextFileLine[object]];


TopSummaryPattern =
PatternSequence[
	TopSummaryLine1 -> _TopSummaryLine1,
	TopSummaryLine2 -> _TopSummaryLine2,
	TopSummaryLine3 -> _TopSummaryLine3,
	TopSummaryLine4 -> _TopSummaryLine4,
	TopSummaryLine5 -> _TopSummaryLine5
];
Protect[TopSummaryPattern];

TopSummaryQ[___]                                          := False;
TopSummaryQ[object:TopSummary[pattern:TopSummaryPattern]] :=
TopSummaryQ[object                                      ]  = True;

TopSummary[
	topSummaryLine1_TopSummaryLine1,
	topSummaryLine2_TopSummaryLine2,
	topSummaryLine3_TopSummaryLine3,
	topSummaryLine4_TopSummaryLine4,
	topSummaryLine5_TopSummaryLine5
] :=
TopSummary[
	topSummaryLine1,
	topSummaryLine2,
	topSummaryLine3,
	topSummaryLine4,
	topSummaryLine5
] =
TopSummary[
	TopSummaryLine1 -> topSummaryLine1,
	TopSummaryLine2 -> topSummaryLine2,
	TopSummaryLine3 -> topSummaryLine3,
	TopSummaryLine4 -> topSummaryLine4,
	TopSummaryLine5 -> topSummaryLine5
];

TopSummary /:                    TopSummaryLine1[object:TopSummary[pattern:TopSummaryPattern]] :=                    TopSummaryLine1[object] = TopSummaryLine1 /. {pattern};
TopSummary /:                    TopSummaryLine2[object:TopSummary[pattern:TopSummaryPattern]] :=                    TopSummaryLine2[object] = TopSummaryLine2 /. {pattern};
TopSummary /:                    TopSummaryLine3[object:TopSummary[pattern:TopSummaryPattern]] :=                    TopSummaryLine3[object] = TopSummaryLine3 /. {pattern};
TopSummary /:                    TopSummaryLine4[object:TopSummary[pattern:TopSummaryPattern]] :=                    TopSummaryLine4[object] = TopSummaryLine4 /. {pattern};
TopSummary /:                    TopSummaryLine5[object:TopSummary[pattern:TopSummaryPattern]] :=                    TopSummaryLine5[object] = TopSummaryLine5 /. {pattern};

TopSummary /:                          Timestamp[object:TopSummary[pattern:TopSummaryPattern]] :=                          Timestamp[object] =                          Timestamp[TopSummaryLine1[object]];
TopSummary /:                             Uptime[object:TopSummary[pattern:TopSummaryPattern]] :=                             Uptime[object] =                             Uptime[TopSummaryLine1[object]];
TopSummary /:                       UserSessions[object:TopSummary[pattern:TopSummaryPattern]] :=                       UserSessions[object] =                       UserSessions[TopSummaryLine1[object]];
TopSummary /:                 LoadAverage1Minute[object:TopSummary[pattern:TopSummaryPattern]] :=                 LoadAverage1Minute[object] =                 LoadAverage1Minute[TopSummaryLine1[object]];
TopSummary /:                 LoadAverage5Minute[object:TopSummary[pattern:TopSummaryPattern]] :=                 LoadAverage5Minute[object] =                 LoadAverage5Minute[TopSummaryLine1[object]];
TopSummary /:                LoadAverage15Minute[object:TopSummary[pattern:TopSummaryPattern]] :=                LoadAverage15Minute[object] =                LoadAverage15Minute[TopSummaryLine1[object]];

TopSummary /:                     TotalProcesses[object:TopSummary[pattern:TopSummaryPattern]] :=                     TotalProcesses[object] =                     TotalProcesses[TopSummaryLine2[object]];
TopSummary /:              TotalRunningProcesses[object:TopSummary[pattern:TopSummaryPattern]] :=              TotalRunningProcesses[object] =              TotalRunningProcesses[TopSummaryLine2[object]];
TopSummary /:             TotalSleepingProcesses[object:TopSummary[pattern:TopSummaryPattern]] :=             TotalSleepingProcesses[object] =             TotalSleepingProcesses[TopSummaryLine2[object]];
TopSummary /:              TotalStoppedProcesses[object:TopSummary[pattern:TopSummaryPattern]] :=              TotalStoppedProcesses[object] =              TotalStoppedProcesses[TopSummaryLine2[object]];
TopSummary /:               TotalZombieProcesses[object:TopSummary[pattern:TopSummaryPattern]] :=               TotalZombieProcesses[object] =               TotalZombieProcesses[TopSummaryLine2[object]];

TopSummary /:         CPUPercentForUserProcesses[object:TopSummary[pattern:TopSummaryPattern]] :=         CPUPercentForUserProcesses[object] =         CPUPercentForUserProcesses[TopSummaryLine3[object]];
TopSummary /:       CPUPercentForSystemProcesses[object:TopSummary[pattern:TopSummaryPattern]] :=       CPUPercentForSystemProcesses[object] =       CPUPercentForSystemProcesses[TopSummaryLine3[object]];
TopSummary /:      CPUPercentWithPriorityUpgrade[object:TopSummary[pattern:TopSummaryPattern]] :=      CPUPercentWithPriorityUpgrade[object] =      CPUPercentWithPriorityUpgrade[TopSummaryLine3[object]];
TopSummary /:                  CPUPercentNotUsed[object:TopSummary[pattern:TopSummaryPattern]] :=                  CPUPercentNotUsed[object] =                  CPUPercentNotUsed[TopSummaryLine3[object]];
TopSummary /: CPUPercentForProcessesWaitingForIO[object:TopSummary[pattern:TopSummaryPattern]] := CPUPercentForProcessesWaitingForIO[object] = CPUPercentForProcessesWaitingForIO[TopSummaryLine3[object]];
TopSummary /: CPUPercentServingHardwareInterupts[object:TopSummary[pattern:TopSummaryPattern]] := CPUPercentServingHardwareInterupts[object] = CPUPercentServingHardwareInterupts[TopSummaryLine3[object]];
TopSummary /: CPUPercentServingSoftwareInterupts[object:TopSummary[pattern:TopSummaryPattern]] := CPUPercentServingSoftwareInterupts[object] = CPUPercentServingSoftwareInterupts[TopSummaryLine3[object]];
TopSummary /:       CPUPercentStolenByHypervisor[object:TopSummary[pattern:TopSummaryPattern]] :=       CPUPercentStolenByHypervisor[object] =       CPUPercentStolenByHypervisor[TopSummaryLine3[object]];

TopSummary /:                TotalPhysicalMemory[object:TopSummary[pattern:TopSummaryPattern]] :=                TotalPhysicalMemory[object] =                TotalPhysicalMemory[TopSummaryLine4[object]];
TopSummary /:                 UsedPhysicalMemory[object:TopSummary[pattern:TopSummaryPattern]] :=                 UsedPhysicalMemory[object] =                 UsedPhysicalMemory[TopSummaryLine4[object]];
TopSummary /:                 FreePhysicalMemory[object:TopSummary[pattern:TopSummaryPattern]] :=                 FreePhysicalMemory[object] =                 FreePhysicalMemory[TopSummaryLine4[object]];
TopSummary /:               BufferPhysicalMemory[object:TopSummary[pattern:TopSummaryPattern]] :=               BufferPhysicalMemory[object] =               BufferPhysicalMemory[TopSummaryLine4[object]];

TopSummary /:                    TotalSwapMemory[object:TopSummary[pattern:TopSummaryPattern]] :=                    TotalSwapMemory[object] =                    TotalSwapMemory[TopSummaryLine5[object]];
TopSummary /:                     UsedSwapMemory[object:TopSummary[pattern:TopSummaryPattern]] :=                     UsedSwapMemory[object] =                     UsedSwapMemory[TopSummaryLine5[object]];
TopSummary /:                     FreeSwapMemory[object:TopSummary[pattern:TopSummaryPattern]] :=                     FreeSwapMemory[object] =                     FreeSwapMemory[TopSummaryLine5[object]];
TopSummary /:                   CachedSwapMemory[object:TopSummary[pattern:TopSummaryPattern]] :=                   CachedSwapMemory[object] =                   CachedSwapMemory[TopSummaryLine5[object]];

Format[object:TopSummary[pattern:TopSummaryPattern]] := "[top data]:" <> DateString[Timestamp[object]];


ProcessListItemStringPattern = 
StartOfString ~~ WhitespaceCharacter... ~~
pid:NumberString ~~ Whitespace ~~
user:Except[WhitespaceCharacter].. ~~ Whitespace ~~
priority : Except[WhitespaceCharacter] .. ~~ Whitespace ~~
nice:NumberString ~~ Whitespace ~~
virtualMemory:(virtualMemoryMagnitude:NumberString ~~ virtualMemoryUnit:Repeated[("k" | "m"), {0, 1}]) ~~ Whitespace ~~
residentMemory:(residentMemoryMagnitude:NumberString ~~ residentMemoryUnit:Repeated[("k" | "m"), {0, 1}]) ~~ Whitespace ~~
sharedMemory:(sharedMemoryMagnitude:NumberString ~~ sharedMemoryUnit:Repeated[("k" | "m"), {0, 1}]) ~~ Whitespace ~~
status : ("D" | "R" | "S" | "T" | "Z") ~~ Whitespace ~~
cpu:NumberString ~~ Whitespace ~~
memory:NumberString ~~ Whitespace ~~
hours:NumberString ~~ ":" ~~ minutes:Repeated[DigitCharacter, 2] ~~ "." ~~ seconds:Repeated[DigitCharacter, 2] ~~ Whitespace ~~
command:__ ~~ Whitespace ~~
EndOfString;

ProcessListItemSourceQ[___]                         := False;
ProcessListItemSourceQ[textFileLine_?TextFileLineQ] :=
ProcessListItemSourceQ[textFileLine               ]  = StringMatchQ[LineText[textFileLine], ProcessListItemStringPattern];

ProcessListItemPattern =
PatternSequence[
	PID            -> _Integer,
	User           -> _String,
	Priority       -> _String,
	Nice           -> _Integer,
	VirtualMemory  -> _?MemoryQ,
	PhysicalMemory -> _?MemoryQ,
	SharedMemory   -> _?MemoryQ,
	Status         -> _String,
	PercentCPU     -> _?NumberQ,
	PercentMemory  -> _?NumberQ,
	TotalCPUTime   -> _?DurationQ,
	Command        -> _String,
	TextFileLine   -> _?TextFileLineQ,
	TopSummary     -> _?TopSummaryQ
];
Protect[ProcessListItemPattern];

ProcessListItemQ[___]                                                    := False;
ProcessListItemQ[object:ProcessListItem[pattern:ProcessListItemPattern]] :=
ProcessListItemQ[object                                                ]  = True;

(*
ProcessListItem[textFileLine_?TextFileLineQ /; ProcessListItemSourceQ[textFileLine]] :=
Module[{data, object},
	data = StringCases[LineText[textFileLine], ProcessListItemStringPattern :> {
			PID            -> ToExpression[pid],
			User           -> user,
			Priority       -> priority,
			Nice           -> ToExpression[nice],
			VirtualMemory  -> Quantity[ToExpression[virtualMemoryMagnitude], virtualMemoryUnit <> "B"],
			PhysicalMemory -> Quantity[ToExpression[residentMemoryMagnitude], residentMemoryUnit <> "B"],
			SharedMemory   -> Quantity[ToExpression[sharedMemoryMagnitude], sharedMemoryUnit <> "B"],
			Status         -> status,
			PercentCPU     -> ToExpression[cpu],
			PercentMemory  -> ToExpression[memory],
			TotalCPUTime   -> Quantity[ToExpression[hours], "Hours"] + Quantity[ToExpression[minutes], "Minutes"] + Quantity[ToExpression[seconds], "Seconds"],
			Command        -> command
		}
	];
		
	object = data /. rest : {{__}} :> ProcessListItem[Sequence @@ Flatten[rest, 1], TextFileLine -> textFileLine];
	ProcessListItem[textFileLine] = object
];
*)

(*
TODO: For some reason adding topSummary to ProcessListItem (necessary to support Timestamp) jumped the
memoization from from about 30 seconds to 50 seconds for 30,512 TextFile lines (122 top entries). 
*)
ProcessListItem[textFileLine_?TextFileLineQ, topSummary_?TopSummaryQ] :=
Module[{lineText, inputStream, pid, user, priority, nice, virtualMemory, physicalMemory, sharedMemory, status, percentCPU, percentMemory, totalCPUTime, command},
	lineText    = LineText[textFileLine];
	inputStream = StringToStream[lineText];
	
	pid      = Read[inputStream, Number];
	user     = Read[inputStream, Word];
	priority = Read[inputStream, Word];
	nice     = Read[inputStream, Number];
	
	Off[Read::readn];
	virtualMemory  = Read[inputStream, Number];
	physicalMemory = Read[inputStream, Number];
	If[SameQ[physicalMemory, $Failed],
		If[!SameQ["m", Read[inputStream, Character]], Abort[]];
		virtualMemory *= 10^6;
		physicalMemory = Read[inputStream, Number];
	];
	
	sharedMemory = Read[inputStream, Number];
	If[SameQ[sharedMemory, $Failed], If[!SameQ["m", Read[inputStream, Character]], Abort[]];
		physicalMemory *= 10^6;
		sharedMemory = Read[inputStream, Number];
	];
	
	status = Read[inputStream, Character];
	If[SameQ["m", status],
		sharedMemory *= 10^6;
		status = Read[inputStream, Character];
	];
	On[Read::readn];
	
	percentCPU    = Read[inputStream, Number];
	percentMemory = Read[inputStream, Number];
	totalCPUTime  = Read[inputStream, {Number, Character, Number}];
	command       = StringTrim[Read[inputStream, String]];
	Close[inputStream];
	
	If[Length[totalCPUTime] =!= 3, Abort[]];
	
	ProcessListItem[textFileLine] =
	ProcessListItem[
		pid,
		user,
		priority,
		nice,
		Quantity[virtualMemory, "Bytes"],
		Quantity[physicalMemory, "Bytes"],
		Quantity[sharedMemory, "Bytes"],
		status,
		percentCPU,
		percentMemory,
		Quantity[totalCPUTime[[1]] * 60 + totalCPUTime[[3]], "Seconds"],
		command,
		textFileLine,
		topSummary
	]
];

ProcessListItem[
	pid_Integer,
	user_String,
	priority_String,
	nice_Integer,
	virtualMemory_?MemoryQ,
	physicalMemory_?MemoryQ,
	sharedMemory_?MemoryQ,
	status_String,
	percentCPU_Integer,
	percentMemory_Real,
	totalCPUTime_?DurationQ,
	command_String,
	textFileLine_?TextFileLineQ,
	topSummary_?TopSummaryQ
] :=
ProcessListItem[
	pid,
	user,
	priority,
	nice,
	virtualMemory,
	physicalMemory,
	sharedMemory,
	status,
	percentCPU,
	percentMemory,
	totalCPUTime,
	command,
	textFileLine,
	topSummary
] =
ProcessListItem[
	PID            -> pid,
	User           -> user,
	Priority       -> priority,
	Nice           -> nice,
	VirtualMemory  -> virtualMemory,
	PhysicalMemory -> physicalMemory,
	SharedMemory   -> sharedMemory,
	Status         -> status,
	PercentCPU     -> percentCPU,
	PercentMemory  -> percentMemory,
	TotalCPUTime   -> totalCPUTime,
	Command        -> command,
	TextFileLine   -> textFileLine,
	TopSummary     -> topSummary
];

ProcessListItem /:     LineNumber[object:ProcessListItem[pattern:ProcessListItemPattern]] :=     LineNumber[object] = LineNumber[TextFileLine[object]];
ProcessListItem /:      Timestamp[object:ProcessListItem[pattern:ProcessListItemPattern]] :=      Timestamp[object] = Timestamp[TopSummary[object]];
ProcessListItem /:            PID[object:ProcessListItem[pattern:ProcessListItemPattern]] :=            PID[object] = PID            /. {pattern};
ProcessListItem /:           User[object:ProcessListItem[pattern:ProcessListItemPattern]] :=           User[object] = User           /. {pattern};
ProcessListItem /:       Priority[object:ProcessListItem[pattern:ProcessListItemPattern]] :=       Priority[object] = Priority       /. {pattern};
ProcessListItem /:           Nice[object:ProcessListItem[pattern:ProcessListItemPattern]] :=           Nice[object] = Nice           /. {pattern};
ProcessListItem /:  VirtualMemory[object:ProcessListItem[pattern:ProcessListItemPattern]] :=  VirtualMemory[object] = VirtualMemory  /. {pattern};
ProcessListItem /: PhysicalMemory[object:ProcessListItem[pattern:ProcessListItemPattern]] := PhysicalMemory[object] = PhysicalMemory /. {pattern};
ProcessListItem /:   SharedMemory[object:ProcessListItem[pattern:ProcessListItemPattern]] :=   SharedMemory[object] = SharedMemory   /. {pattern};
ProcessListItem /:         Status[object:ProcessListItem[pattern:ProcessListItemPattern]] :=         Status[object] = Status         /. {pattern};
ProcessListItem /:     PercentCPU[object:ProcessListItem[pattern:ProcessListItemPattern]] :=     PercentCPU[object] = PercentCPU     /. {pattern};
ProcessListItem /:  PercentMemory[object:ProcessListItem[pattern:ProcessListItemPattern]] :=  PercentMemory[object] = PercentMemory  /. {pattern};
ProcessListItem /:   TotalCPUTime[object:ProcessListItem[pattern:ProcessListItemPattern]] :=   TotalCPUTime[object] = TotalCPUTime   /. {pattern};
ProcessListItem /:        Command[object:ProcessListItem[pattern:ProcessListItemPattern]] :=        Command[object] = Command        /. {pattern};
ProcessListItem /:   TextFileLine[object:ProcessListItem[pattern:ProcessListItemPattern]] :=   TextFileLine[object] = TextFileLine   /. {pattern};
ProcessListItem /:     TopSummary[object:ProcessListItem[pattern:ProcessListItemPattern]] :=     TopSummary[object] = TopSummary     /. {pattern};

Format[object:ProcessListItem[pattern:ProcessListItemPattern]] := "[top data]:" <> LineText[TextFileLine[object]];


ProcessListItemVectorQ[___]       := False;
ProcessListItemVectorQ[list_List] := VectorQ[list, ProcessListItemQ];

ProcessListPattern =
PatternSequence[
	ProcessListItemList -> _?ProcessListItemVectorQ
];
Protect[ProcessListPattern];

ProcessListQ[___]                                            := False;
ProcessListQ[object:ProcessList[pattern:ProcessListPattern]] :=
ProcessListQ[object                                        ]  = True;

ProcessList[processListItemList_?ProcessListItemVectorQ] :=
ProcessList[processListItemList                        ]  =
ProcessList[ProcessListItemList -> processListItemList];

ProcessList /: ProcessListItemList[object:ProcessList[pattern:ProcessListPattern]] := ProcessListItemList[object] = ProcessListItemList /. {pattern};
ProcessList /:              Length[object:ProcessList[pattern:ProcessListPattern]] := Length[ProcessListItemList[object]];

Format[object:ProcessList[pattern:ProcessListPattern]] := "[top data]: " <> ToString[Length[object]] <> " processes";


TopDataPattern =
PatternSequence[
	TopSummary  -> _?TopSummaryQ,
	ProcessList -> _?ProcessListQ
];
Protect[TopDataPattern];

TopDataQ[___]                                    := False;
TopDataQ[object:TopData[pattern:TopDataPattern]] :=
TopDataQ[object                                ]  = True;

TopData[
	topSummary_?TopSummaryQ,
	processList_?ProcessListQ
] :=
TopData[
	topSummary,
	processList
] =
TopData[
	TopSummary  -> topSummary,
	ProcessList -> processList
];

TopData /:                         TopSummary[object:TopData[pattern:TopDataPattern]] :=                         TopSummary[object] = TopSummary  /. {pattern};
TopData /:                        ProcessList[object:TopData[pattern:TopDataPattern]] :=                        ProcessList[object] = ProcessList /. {pattern};

TopData /:                    TopSummaryLine1[object:TopData[pattern:TopDataPattern]] :=                    TopSummaryLine1[object] =                    TopSummaryLine1[TopSummary[object]];
TopData /:                    TopSummaryLine2[object:TopData[pattern:TopDataPattern]] :=                    TopSummaryLine2[object] =                    TopSummaryLine2[TopSummary[object]];
TopData /:                    TopSummaryLine3[object:TopData[pattern:TopDataPattern]] :=                    TopSummaryLine3[object] =                    TopSummaryLine3[TopSummary[object]];
TopData /:                    TopSummaryLine4[object:TopData[pattern:TopDataPattern]] :=                    TopSummaryLine4[object] =                    TopSummaryLine4[TopSummary[object]];
TopData /:                    TopSummaryLine5[object:TopData[pattern:TopDataPattern]] :=                    TopSummaryLine5[object] =                    TopSummaryLine5[TopSummary[object]];

TopData /:                          Timestamp[object:TopData[pattern:TopDataPattern]] :=                          Timestamp[object] =                          Timestamp[TopSummaryLine1[object]];
TopData /:                             Uptime[object:TopData[pattern:TopDataPattern]] :=                             Uptime[object] =                             Uptime[TopSummaryLine1[object]];
TopData /:                       UserSessions[object:TopData[pattern:TopDataPattern]] :=                       UserSessions[object] =                       UserSessions[TopSummaryLine1[object]];
TopData /:                 LoadAverage1Minute[object:TopData[pattern:TopDataPattern]] :=                 LoadAverage1Minute[object] =                 LoadAverage1Minute[TopSummaryLine1[object]];
TopData /:                 LoadAverage5Minute[object:TopData[pattern:TopDataPattern]] :=                 LoadAverage5Minute[object] =                 LoadAverage5Minute[TopSummaryLine1[object]];
TopData /:                LoadAverage15Minute[object:TopData[pattern:TopDataPattern]] :=                LoadAverage15Minute[object] =                LoadAverage15Minute[TopSummaryLine1[object]];

TopData /:                     TotalProcesses[object:TopData[pattern:TopDataPattern]] :=                     TotalProcesses[object] =                     TotalProcesses[TopSummaryLine2[object]];
TopData /:              TotalRunningProcesses[object:TopData[pattern:TopDataPattern]] :=              TotalRunningProcesses[object] =              TotalRunningProcesses[TopSummaryLine2[object]];
TopData /:             TotalSleepingProcesses[object:TopData[pattern:TopDataPattern]] :=             TotalSleepingProcesses[object] =             TotalSleepingProcesses[TopSummaryLine2[object]];
TopData /:              TotalStoppedProcesses[object:TopData[pattern:TopDataPattern]] :=              TotalStoppedProcesses[object] =              TotalStoppedProcesses[TopSummaryLine2[object]];
TopData /:               TotalZombieProcesses[object:TopData[pattern:TopDataPattern]] :=               TotalZombieProcesses[object] =               TotalZombieProcesses[TopSummaryLine2[object]];

TopData /:         CPUPercentForUserProcesses[object:TopData[pattern:TopDataPattern]] :=         CPUPercentForUserProcesses[object] =         CPUPercentForUserProcesses[TopSummaryLine3[object]];
TopData /:       CPUPercentForSystemProcesses[object:TopData[pattern:TopDataPattern]] :=       CPUPercentForSystemProcesses[object] =       CPUPercentForSystemProcesses[TopSummaryLine3[object]];
TopData /:      CPUPercentWithPriorityUpgrade[object:TopData[pattern:TopDataPattern]] :=      CPUPercentWithPriorityUpgrade[object] =      CPUPercentWithPriorityUpgrade[TopSummaryLine3[object]];
TopData /:                  CPUPercentNotUsed[object:TopData[pattern:TopDataPattern]] :=                  CPUPercentNotUsed[object] =                  CPUPercentNotUsed[TopSummaryLine3[object]];
TopData /: CPUPercentForProcessesWaitingForIO[object:TopData[pattern:TopDataPattern]] := CPUPercentForProcessesWaitingForIO[object] = CPUPercentForProcessesWaitingForIO[TopSummaryLine3[object]];
TopData /: CPUPercentServingHardwareInterupts[object:TopData[pattern:TopDataPattern]] := CPUPercentServingHardwareInterupts[object] = CPUPercentServingHardwareInterupts[TopSummaryLine3[object]];
TopData /: CPUPercentServingSoftwareInterupts[object:TopData[pattern:TopDataPattern]] := CPUPercentServingSoftwareInterupts[object] = CPUPercentServingSoftwareInterupts[TopSummaryLine3[object]];
TopData /:       CPUPercentStolenByHypervisor[object:TopData[pattern:TopDataPattern]] :=       CPUPercentStolenByHypervisor[object] =       CPUPercentStolenByHypervisor[TopSummaryLine3[object]];

TopData /:                TotalPhysicalMemory[object:TopData[pattern:TopDataPattern]] :=                TotalPhysicalMemory[object] =                TotalPhysicalMemory[TopSummaryLine4[object]];
TopData /:                 UsedPhysicalMemory[object:TopData[pattern:TopDataPattern]] :=                 UsedPhysicalMemory[object] =                 UsedPhysicalMemory[TopSummaryLine4[object]];
TopData /:                 FreePhysicalMemory[object:TopData[pattern:TopDataPattern]] :=                 FreePhysicalMemory[object] =                 FreePhysicalMemory[TopSummaryLine4[object]];
TopData /:               BufferPhysicalMemory[object:TopData[pattern:TopDataPattern]] :=               BufferPhysicalMemory[object] =               BufferPhysicalMemory[TopSummaryLine4[object]];

TopData /:                    TotalSwapMemory[object:TopData[pattern:TopDataPattern]] :=                    TotalSwapMemory[object] =                    TotalSwapMemory[TopSummaryLine5[object]];
TopData /:                     UsedSwapMemory[object:TopData[pattern:TopDataPattern]] :=                     UsedSwapMemory[object] =                     UsedSwapMemory[TopSummaryLine5[object]];
TopData /:                     FreeSwapMemory[object:TopData[pattern:TopDataPattern]] :=                     FreeSwapMemory[object] =                     FreeSwapMemory[TopSummaryLine5[object]];
TopData /:                   CachedSwapMemory[object:TopData[pattern:TopDataPattern]] :=                   CachedSwapMemory[object] =                   CachedSwapMemory[TopSummaryLine5[object]];

TopData /:                ProcessListItemList[object:TopData[pattern:TopDataPattern]] :=                ProcessListItemList[object] =                ProcessListItemList[ProcessList[object]];

TopData /:                           TextFile[object:TopData[pattern:TopDataPattern]] :=                           TextFile[object] = TextFile[TextFileLine[TopSummaryLine1[object]]];

Format[object:TopData[pattern:TopDataPattern]] := "[top data]:" <> DateString[Timestamp[object]] <> ", " <> ToString[Length[ProcessList[object]]] <> " processes";


TextFileLineNumberInterval[textFile_?TextFileQ] :=
TextFileLineNumberInterval[textFile           ]  = Interval[LineNumber /@ Lines[textFile][[{1, -1}]]];

(* The next TopSummaryLine1 occurs 3 lines after the end of the ProcessList. *)
NextPotentialTopDataLineNumber[topData_?TopDataQ] := LineNumber[ProcessListItemList[topData][[-1]]] + 3;

NextTopData[topData_?TopDataQ]   := NextTopData[TextFile[topData], NextPotentialTopDataLineNumber[topData]];
NextTopData[textFile_?TextFileQ] := NextTopData[textFile, 1];
NextTopData[textFile_?TextFileQ, lineNumber_Integer] :=
Module[{textFileLineList, topSummary, processListLineNumber, processList, topData},
	(* We need at least 8 lines to create a TopData. *)
	If[!IntervalMemberQ[TextFileLineNumberInterval[textFile],  lineNumber + 8], Return[]];
	textFileLineList = Lines[textFile][[lineNumber ;; lineNumber + 4]];
	topSummary = TopSummary @@ Map[#[[1]]@#[[2]] &, Transpose[{{TopSummaryLine1, TopSummaryLine2, TopSummaryLine3, TopSummaryLine4, TopSummaryLine5}, textFileLineList}]];
	processListLineNumber = LineNumber[TopSummaryLine5[topSummary]] + 3;
	processList = ProcessList@ Map[ProcessListItem[#, topSummary]&, TakeWhile[Lines[textFile][[processListLineNumber ;;]], ProcessListItemSourceQ]];
	topData = TopData[topSummary, processList];
	topData
];

TopDataFilePathPatern = StartOfString ~~ __ ~~ "top_" ~~ timestamp:DigitCharacter.. ~~ ".txt" ~~ EndOfString;
TopDataFilePathQ[filePath_String] := False;
TopDataFilePathQ[filePath_String] := StringMatchQ[filePath, TopDataFilePathPatern]
TopDataFileTimestamp[filePath_?TopDataFilePathQ] :=
Flatten[StringCases[filePath, TopDataFilePathPatern :> DateList[UnixTime[ToExpression[timestamp] 10^3]]], 1];

TopLogFileDataPattern =
PatternSequence[
	Lines    -> Automatic | _List,
	TextFile -> _?TextFileQ
];
Protect[TopLogFileDataPattern];

TopLogFileDataParsedPattern =
PatternSequence[
	Lines    -> _List      ,
	TextFile -> _?TextFileQ
];
Protect[TopLogFileDataParsedPattern];

TopLogFileDataQ[___]                                                  := False;
TopLogFileDataQ[object:TopLogFileData[pattern:TopLogFileDataPattern]] :=
TopLogFileDataQ[object                                              ]  = True;

TopLogFileDataParsedQ[___]                                                        := False;
TopLogFileDataParsedQ[object:TopLogFileData[pattern:TopLogFileDataParsedPattern]] :=
TopLogFileDataParsedQ[object                                                    ]  = True;

TopLogFileData[         filePath_String /; FileQ[filePath] ] :=
TopLogFileData[         filePath                           ]  =
TopLogFileData[TextFile[filePath                          ]];

TopLogFileData[textFile_?TextFileQ] :=
TopLogFileData[textFile           ]  =
TopLogFileData[
	Lines    -> Automatic,
	TextFile -> textFile
];


TopLogFileData /:   FilePath[object:TopLogFileData[pattern:TopLogFileDataPattern]] := FilePath[TextFile[object]];
TopLogFileData /:   TextFile[object:TopLogFileData[pattern:TopLogFileDataPattern]] := TextFile /. {pattern};
TopLogFileData /:      Lines[object:TopLogFileData[pattern:TopLogFileDataPattern]] :=
Module[{textFile, lines, topData},
	textFile = TextFile[object];
	
	If[
		TopLogFileDataParsedQ[TopLogFileData[textFile]],
		
		(* True *)
		lines = Lines /. List @@ TopLogFileData[textFile],

		(* False *)
		topData = NextTopData[textFile];
		lines = Most[NestWhileList[NextTopData, topData, TopDataQ]];
	
		TopLogFileData[FilePath[textFile]] =
		TopLogFileData[textFile]           =
		TopLogFileData[
			Lines    -> lines   ,
			TextFile -> textFile
		];
	];
	
	Lines[object] = lines
];

(* TODO: Change propertyList_List to propertyList_List /; VectorQ[propertyList, SymbolQ] *)
TopSummaryPlot[topLogFileData_?TopLogFileDataQ, plotLabel_String, yAxisFrameLabel_String, propertyList_List] := Module[{indices, topSummaryList, plotData},
	topSummaryList = Map[TopSummary, Lines[topLogFileData]];
	indices        = Riffle[ConstantArray[1, Length[propertyList]], Range[2, Length[propertyList] + 1]];
	plotData       =
	Map[
		Transpose,
		Partition[
			Transpose[
				Transpose[
					Map[Thread, Through[{Timestamp, Sequence @@ propertyList}[topSummaryList]]]
				][[All, indices]]
			]
		, 2]
	];
	DateListPlot[
		plotData,
		PlotRange   -> All,
		Joined      -> True,
		Axes        -> False,
		Frame       -> True,
		ImageSize   -> 8 * 72,
		PlotLabel   -> plotLabel,
		PlotLegends -> propertyList,
		FrameLabel  -> {"Time", yAxisFrameLabel}
	]
];

LoadAveragePlot[topLogFileData_?TopLogFileDataQ, plotLabel_String] :=
TopSummaryPlot[topLogFileData, plotLabel, "Load Average", {LoadAverage1Minute, LoadAverage5Minute, LoadAverage15Minute}];

TotalProcessesPlot[topLogFileData_?TopLogFileDataQ, plotLabel_String] :=
TopSummaryPlot[topLogFileData, plotLabel, "Total Processes", {TotalProcesses, TotalRunningProcesses, TotalSleepingProcesses, TotalStoppedProcesses, TotalZombieProcesses}];

CPUPercentPlot[topLogFileData_?TopLogFileDataQ, plotLabel_String] :=
TopSummaryPlot[topLogFileData, plotLabel, "CPU Percent", {CPUPercentForUserProcesses, CPUPercentForSystemProcesses, CPUPercentWithPriorityUpgrade, CPUPercentNotUsed, CPUPercentForProcessesWaitingForIO, CPUPercentServingHardwareInterupts, CPUPercentServingSoftwareInterupts, CPUPercentStolenByHypervisor}];

PhysicalMemoryPlot[topLogFileData_?TopLogFileDataQ, plotLabel_String] :=
TopSummaryPlot[topLogFileData, plotLabel, "Kilobytes", {TotalPhysicalMemory, UsedPhysicalMemory, FreePhysicalMemory, BufferPhysicalMemory}];

SwapMemoryPlot[topLogFileData_?TopLogFileDataQ, plotLabel_String] :=
TopSummaryPlot[topLogFileData, plotLabel, "Kilobytes", {TotalSwapMemory, UsedSwapMemory, FreeSwapMemory, CachedSwapMemory}];


FindProcessByCommand[topLogFileData_?TopLogFileDataQ, command_String] :=
FindProcessByCommand[topLogFileData                 , command       ]  =
Flatten[Map[Select[#, StringMatchQ[Command[#], command] &] &, Map[ProcessListItemList, Lines[topLogFileData]]]];

CommandPlot[topLogFileData_?TopLogFileDataQ, plotLabel_String, commandList_List /; VectorQ[commandList, StringQ], property_Symbol] :=
Module[{processesListList, plotData},
	processesListList = FindProcessByCommand[topLogFileData, #] & /@ commandList;
	plotData = Map[Map[{Timestamp[#], property[#]} &, #] &, processesListList] /. quantity_Quantity :> QuantityMagnitude[quantity];
	DateListPlot[
		plotData,
		PlotRange   -> All,
		Joined      -> True,
		Axes        -> False,
		Frame       -> True,
		ImageSize   -> 8 * 72,
		PlotLabel   -> plotLabel,
		PlotLegends -> commandList,
		FrameLabel  -> {"Time", property}
	]
];

FindAllPIDForCommand[topLogFileData_?TopLogFileDataQ, command_String] :=
FindAllPIDForCommand[topLogFileData                 , command       ]  =
Module[{processesList},
	processesList = FindProcessByCommand[topLogFileData, command];
	Tally[Map[{Command[#], PID[#]} &, processesList]][[All, 1]]
];
    
FindProcessByPID[topLogFileData_?TopLogFileDataQ, pid_Integer] :=
FindProcessByPID[topLogFileData                 , pid        ]  =
Flatten[Map[Select[#, SameQ[PID[#], pid] &] &, Map[ProcessListItemList, Lines[topLogFileData]]]];

PIDPlot[topLogFileData_?TopLogFileDataQ, plotLabel_String, pidList_List /; VectorQ[pidList, IntegerQ], property_Symbol] :=
Module[{processesListList, plotData, plotLegends},
	processesListList = FindProcessByPID[topLogFileData, #] & /@ pidList;
	plotData          = Map[Map[{Timestamp[#], property[#]} &, #] &, processesListList] /. quantity_Quantity :> QuantityMagnitude[quantity];
	plotLegends       = Map[Command[#] <> " (pid " <> ToString[PID[#]] <> ")" &, processesListList[[All, 1]]];
	DateListPlot[
		plotData,
		PlotRange   -> All,
		Joined      -> True,
		Axes        -> False,
		Frame       -> True,
		ImageSize   -> 8 * 72,
		PlotLabel   -> plotLabel,
		PlotLegends -> plotLegends,
		FrameLabel  -> {"Time", property}
	]
];

End[]

EndPackage[]
