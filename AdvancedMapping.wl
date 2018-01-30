(* ::Package:: *)

BeginPackage["AdvancedMapping`"]

ProgressParallelMap::usage =
 "ProgressParallelMap[f,expr] is a ParallelMap implementation with progress bar. levelspec is always {1}. 
\"ShowInfo\"\[Rule]True show the detailed version of the progress bar. Options are the same as ParallelMap.";
 
ProgressMap::usage =
 "ProgressMap[f,expr] is a Map implementation with progress bar. levelspec is always {1}.
\"ShowInfo\"\[Rule]True show the detailed version of the progress bar.";
  
ProgressTable::usage =
 "Table implementation with progress bar. Same usage as Table. 
\"ShowInfo\"\[Rule]True show the detailed version of the progress bar.";

MapIf::usage = "MapIf[f,expr,crit] Maps if condition in crit is true at each element.";

MapIfElse::usage = "MapIfElse[f,g,expr,crit] Maps f if condition in crit is true at each element.
Else it maps g."

MapPattern::usage = "MapPattern[f,expr,patt] Maps when pattern in patt is matched."

Begin["`Private`"]

(* Reloj *)
GetSeconds[time_] := IntegerString[Round[Mod[time,60]], 10, 2];
GetMinutes[time_]:= IntegerString[Mod[Floor[time/60],60], 10, 2];
GetHours[time_] := IntegerString[Floor[time/3600], 10, 2];
ClockFormat[time_] :=StringJoin[GetHours[time], ":", GetMinutes[time], ":", GetSeconds[time]];

(* Mapeos con progreso *)
DefaultIndicator[indexProgress_, totalSize_] := ProgressIndicator[indexProgress, {1, totalSize}];

DetailedIndicator[indexProgress_,totalSize_,startTime_]:=Module[{progressString,remainingTime,remainingTimeString,indicator,ellapsedTimeString},
	progressString = Row[{Style["Progress: ",Bold],ToString[indexProgress],"/",ToString[totalSize]}];
	ellapsedTimeString = Row[{Style["Elapsed time: ",Bold],ClockFormat[AbsoluteTime[]-startTime]}];

	If[indexProgress != 0,
		remainingTime = (AbsoluteTime[]-startTime)/indexProgress (totalSize-indexProgress);
		remainingTimeString = Row[{Style["Remaining: ",Bold],ClockFormat[remainingTime]}];
	,
		remainingTimeString = Row[{Style["Remaining: ", Bold],"Unknown"}];
	];

	indicator = Panel[
		Column[
			{
				Style["Evaluating...",Bold],
				DefaultIndicator[indexProgress,totalSize],
				progressString,
				ellapsedTimeString,
				remainingTimeString
			}
		]
	];

	Return[indicator];
];

ProgressFunction[f_, arg_, index_] := Module[{output},
	output = f[arg];
	AppendTo[indexProgress, index];
	Return[output];
];

ParallelMapIndexed[f_, expr_, opts: OptionsPattern[]] :=
Parallelize[
	MapIndexed[ProgressFunction[f, #1, #2]&, expr],
	FilterRules[{opts}, Options[Parallelize]]
];

SetSharedVariable[indexProgress];
ProgressParallelMap[f_, expr_, opts: OptionsPattern[{"ShowInfo"->False, Parallelize}]] :=
Module[{startTime},
	indexProgress = {0};
	startTime = AbsoluteTime[];

	Monitor[
		ParallelMapIndexed[f, expr, opts]
		,
		If[OptionValue["ShowInfo"],
			DetailedIndicator[Max[indexProgress], Length[expr], startTime]
			,
			DefaultIndicator[Max[indexProgress], Length[expr]]
		]
	]
]; 

ProgressMap[f_, expr_, OptionsPattern[{"ShowInfo"->False}]] :=
Module[{startTime},
	indexProgress = {0};
	startTime = AbsoluteTime[];

	Monitor[
			MapIndexed[ProgressFunction[f, #1, #2]&, expr],

			If[OptionValue["ShowInfo"],
				DetailedIndicator[Max[indexProgress], Length[expr], startTime]
			,
				DefaultIndicator[Max[indexProgress], Length[expr]]
			]
	]
];

(* Tablas con progreso *)
SetAttributes[ProgressTable, HoldFirst]

Options[ProgressTable] = {"ShowInfo"->False};
ProgressTable[expr_, {i_, iterators_}, opts:OptionsPattern[]] := Module[{tableIndex, startTime},
	tableIndex = 0;
	startTime = AbsoluteTime[];

	Monitor[
		Table[tableIndex++;expr, {i, iterators}]
	,
		If[OptionValue[opts, "ShowInfo"],
			DetailedIndicator[tableIndex, Length[iterators], startTime]
		,
			DefaultIndicator[tableIndex, Length[iterators]]
		]
	]
];

ProgressTable[expr_, n_, opts:OptionsPattern[]] := Module[{tableIndex, startTime},
	tableIndex = 0;
	startTime = AbsoluteTime[];

	Monitor[
		Table[tableIndex++;expr, n]
	,

		If[OptionValue[opts, "ShowInfo"],
			DetailedIndicator[tableIndex, n, startTime]
		,
			DefaultIndicator[tableIndex, n]
		]
	]
];

ProgressTable[expr_, {i_, iMin_, iMax_}, opts:OptionsPattern[]] := Module[{iterators},
	iterators = Range[iMin, iMax, 1];
	ProgressTable[expr, {i, iterators}, opts]
];

ProgressTable[expr_, {i_, iMin_, iMax_, di_}, opts:OptionsPattern[]] := Module[{iterators},
	iterators = Range[iMin, iMax, di];
	ProgressTable[expr, {i, iterators}, opts]
];

ProgressTable[expr_, {i_, iMin_, iMax_, di_}, simpleTableIterators__, opts:OptionsPattern[]] := Module[{iterators},
	iterators = Range[iMin, iMax, di];
	ProgressTable[Table[expr, simpleTableIterators], {i, iterators}, opts]
];

ProgressTable[expr_, {i_, iMin_, iMax_}, simpleTableIterators__, opts:OptionsPattern[]] := ProgressTable[expr, {i, iMin, iMax, 1}, simpleTableIterators, opts]

(* Mapeo selectivo *)
MapIf[f_, expr_, crit_]:=MapAt[f,expr,Position[Map[crit,expr],True]];

MapIfElse[f1_, f2_, expr_, crit_]:=Module[{truePos,falsePos},
	truePos = Position[Map[crit, expr], True];
	falsePos = Complement[Transpose[{Range[Length[expr]]}], truePos];
	MapAt[f2, MapAt[f1, expr, truePos], falsePos]
];

MapPattern[f_, expr_, patt_]:=MapAt[f, expr, Position[expr, patt]];

End[ ]

EndPackage[ ]
