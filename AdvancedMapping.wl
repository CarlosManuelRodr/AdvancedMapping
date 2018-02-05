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
Else it maps g.";

MapPattern::usage = "MapPattern[f,expr,patt] Maps when pattern in patt is matched.";

NestApplyList::usage = "NestApplyList[f, g, expr, n] is equivalent to Map[g, NestList[f, expr, n]], 
but doesn't keep the whole NestList[f, expr, n] list, using less memory.";

NestApplyWhileList::usage = "NestApplyWhileList[f, g, expr, test] is equivalent to 
Map[g, NestWhileList[f, expr, test]], but doesn't keep the whole NestList[f, expr, n] list, using less memory.";

NestListIndexed::usage = "NestListIndexed[f, expr, n, startIndex] works as NestList but returns the number of the index
alongside the result of NestList.";

Begin["`Private`"]

(* Reloj *)
GetSeconds[time_] := IntegerString[Round[Mod[time, 60]], 10, 2];
GetMinutes[time_]:= IntegerString[Mod[Floor[time/60], 60], 10, 2];
GetHours[time_] := IntegerString[Floor[time/3600], 10, 2];
ClockFormat[time_] := StringJoin[GetHours[time], ":", GetMinutes[time], ":", GetSeconds[time]];

(* Mapeos con progreso *)
DefaultIndicator[indexProgress_, totalSize_] := ProgressIndicator[indexProgress, {1, totalSize}];

DetailedIndicator[indexProgress_, totalSize_, startTime_, label_:"Evaluating..."] := 
Module[{progressString, remainingTime, remainingTimeString, indicator, ellapsedTimeString},
	progressString = Row[{Style["Progress: ", Bold], ToString[indexProgress], "/", ToString[totalSize]}];
	ellapsedTimeString = Row[{Style["Elapsed time: ", Bold], ClockFormat[AbsoluteTime[] - startTime]}];

	If[indexProgress != 0,
		remainingTime = ((AbsoluteTime[] - startTime) / indexProgress)*(totalSize - indexProgress);
		remainingTimeString = Row[{Style["Remaining: ", Bold], ClockFormat[remainingTime]}];
		,
		remainingTimeString = Row[{Style["Remaining: ", Bold], "Unknown"}];
	];

	indicator = Panel[
		Column[
			{
				Style[label, Bold],
				DefaultIndicator[indexProgress, totalSize],
				progressString,
				ellapsedTimeString,
				remainingTimeString
			}
		]
	];

	Return[indicator];
];

SetAttributes[ProgressParallelMap, HoldFirst];
ProgressParallelMap[f_, expr_, opts: OptionsPattern[{"ShowInfo"->False, "Label"->"Evaluating...", Parallelize}]] :=
Block[{startTime = AbsoluteTime[], indexProgress = 0, output},
	SetSharedVariable[indexProgress];

	Monitor[
		ParallelMap[
			(
				output = f[#];
				indexProgress++;
				output
			)&,
			expr,
			FilterRules[{opts}, Options[Parallelize]]
		]
		,
		If[OptionValue["ShowInfo"],
			DetailedIndicator[indexProgress, Length[expr], startTime, OptionValue["Label"]]
			,
			DefaultIndicator[indexProgress, Length[expr]]
		]
	]
]; 

SetAttributes[ProgressMap, HoldFirst];
ProgressMap[f_, expr_, OptionsPattern[{"ShowInfo"->False, "Label"->"Evaluating..."}]] :=
Block[{startTime = AbsoluteTime[], indexProgress = 0, output},
	Monitor[
			Map[
			(
				output = f[#];
				indexProgress++;
				output
			)&,
			expr
			]
			,
			If[OptionValue["ShowInfo"],
				DetailedIndicator[indexProgress, Length[expr], startTime, OptionValue["Label"]]
				,
				DefaultIndicator[indexProgress, Length[expr]]
			]
	]
];

(* Tablas con progreso *)
SetAttributes[ProgressTable, HoldFirst]

Options[ProgressTable] = {"ShowInfo"->False, "Label"->"Evaluating..."};
ProgressTable[expr_, {i_, iterators_}, opts:OptionsPattern[]] :=
Block[{tableIndex = 0,startTime = AbsoluteTime[]},
	Monitor[
		Table[tableIndex++;expr, {i, iterators}]
		,
		If[OptionValue[ProgressTable, {opts}, "ShowInfo"],
			DetailedIndicator[tableIndex, Length[iterators], startTime, OptionValue[ProgressTable, {opts}, "Label"]]
			,
			DefaultIndicator[tableIndex, Length[iterators]]
		]
	]
];

ProgressTable[expr_, n_, opts: OptionsPattern[]] :=
Block[{tableIndex = 0,startTime = AbsoluteTime[]},
	Monitor[
		Table[tableIndex++;expr, n]
		,
		If[OptionValue[ProgressTable, {opts}, "ShowInfo"],
			DetailedIndicator[tableIndex, n, startTime, OptionValue[ProgressTable, {opts}, "Label"]]
			,
			DefaultIndicator[tableIndex, n]
		]
	]
];

ProgressTable[expr_, {i_, iMin_, iMax_}, opts: OptionsPattern[]] := ProgressTable[expr, {i, Range[iMin, iMax, 1]}, opts];

ProgressTable[expr_, {i_, iMin_, iMax_, di_}, opts: OptionsPattern[]] := ProgressTable[expr, {i, Range[iMin, iMax, di]}, opts];

ProgressTable[expr_, {i_, iMin_, iMax_, di_}, simpleTableIterators__, opts:OptionsPattern[]] := ProgressTable[Table[expr, simpleTableIterators], {i, Range[iMin, iMax, di]}, opts];

ProgressTable[expr_, {i_, iMin_, iMax_}, simpleTableIterators__, opts:OptionsPattern[]] := ProgressTable[expr, {i, iMin, iMax, 1}, simpleTableIterators, opts];

(* Mapeo selectivo *)
SetAttributes[MapIf, HoldAll];
MapIf[f_, expr_, crit_]:=MapAt[f,expr,Position[Map[crit,expr],True]];

SetAttributes[MapIfElse, HoldAll];
MapIfElse[f1_, f2_, expr_, crit_]:=Module[{truePos,falsePos},
	truePos = Position[Map[crit, expr], True];
	falsePos = Complement[Transpose[{Range[Length[expr]]}], truePos];
	MapAt[f2, MapAt[f1, expr, truePos], falsePos]
];

SetAttributes[MapPattern, HoldAll];
MapPattern[f_, expr_, patt_]:=MapAt[f, expr, Position[expr, patt]];

(* Nesting *)
SetAttributes[Reaped, HoldFirst];
Reaped[list_] := First[Last[Reap[list]]];
Push[expr_, elem_] := Rest[Append[expr, elem]];

SetAttributes[NestApplyList, HoldAll];
NestApplyList[f_, g_, expr_, n_?NonNegative] := Reaped[Sow[g[Nest[(Sow[g[##]];f[##])&, expr, n]]]];

SetAttributes[NestApplyWhileList, HoldAll];
NestApplyWhileList[f_, g_, expr_, test_, All] := Map[g, NestWhileList[f, expr, test, All]];
NestApplyWhileList[f_, g_, expr_, test_, m:(_?Positive): 1] := 
Block[{testArg, tmp, iterations}, 
	(* This couldn't be implemented based on NestWhile because NestWhile saves the result at every step. Not very memory efficent. *)
	Reaped[
		testArg = NestList[(Sow[g[#]];f[#])&, expr, m-1];
			Sow[g[Last[testArg]]];

		While[Apply[test, testArg], 
			tmp = f[Last[testArg]];
			testArg = Push[testArg, tmp];
			Sow[g[tmp]];
		]
	]
];

NestApplyWhileList[f_, g_, expr_, test_, m_?Positive, max_?Positive] := 
Block[{testArg, tmp, iterations = 0}, 
	Reaped[
		testArg = NestList[(Sow[g[#]];f[#])&, expr, m-1];
		Sow[g[Last[testArg]]];

		While[Apply[test, testArg] && (iterations<max), 
			tmp = f[Last[testArg]];
			testArg = Push[testArg, tmp];
			Sow[g[tmp]];
			iterations++;
		]
	]
];

SetAttributes[NestListIndexed,HoldFirst];
NestListIndexed[f_, expr_, n_?NonNegative, startIndex_:1] := 
	Transpose[{Range[startIndex, n+startIndex], NestList[f, expr, n]}];

End[ ]

EndPackage[ ]
