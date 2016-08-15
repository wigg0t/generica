(* ::Package:: *)
(* Mathematica Package *)

(* :Title: RPGSimulation` *)
(* :Context: RPGSimulation` *)
(* :Description: Simulation of fights in Generica *)
(* :Author: Frank Buermann, frank.buermann@web.de *)
(* :Date: 2016-07-14 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 10.3 *)
(* :Copyright: (c) 2016 Frank Buermann *)

(***********************************************************************)
(* :ToDo: *)
(***********************************************************************)

(***********************************************************************)
BeginPackage["RPGSimulation`"];

(***********************************************************************)
(* :Documentation: *)
(***********************************************************************)
ClearAll[fight];
fight::usage = "";

ClearAll[combatRound];
combatRound::usage = "";

ClearAll[monteCarloRPG];
monteCarloRPG::usage = "";

weapons =
    <|
      "Dolch" ->
          <|
            "TP" -> "1W6+2",
            "BTP" -> 2,
            "WM" -> -2
          |>,
      "Schwert" ->
          <|
            "TP" -> "1W6+4",
            "BTP" -> 4,
            "WM" -> 1
          |>,
      "Degen" ->
          <|
            "TP" -> "1W6+3",
            "BTP" -> 3
            "WM" -> 2
          |>,
      "Barbarenstreitaxt" ->
          <|
            "TP" -> "2W6+4",
            "BTP" -> 4,
            "WM" -> -3
          |>,
      "Kampfstab" ->
          <|
            "TP" -> "1W6+3",
            "BTP" -> 3,
            "WM" -> 1
          |>,
      "Knüppel" ->
          <|
            "TP" -> "1W6+3",
            "BTP" -> 3,
            "WM" -> -1
          |>
    |>;

armor =
    <|
      "Keine" -> <|"RS" -> 0, "BE" -> 0|>,
      "Leicht" -> <|"RS" -> 1, "BE" -> 1|>,
      "Mittel" -> <|"RS" -> 2, "BE" -> 2|>,
      "Schwer" -> <|"RS" -> 3, "BE" -> 3|>,
      "Überschwer" -> <|"RS" -> 4, "BE" -> 4|>
    |>;

sampleChars =
    <|
      "Alrik" ->
          <|
            "LE" -> 30,
            "LP" -> 30,
            "Kampf" -> 10,
            "INI" -> 37,
            Sequence @@ Normal@armor["Leicht"],
            Sequence @@ Normal@weapons["Schwert"],
            "Stil" -> "Normal",
            "Pause" -> False,
            "Vorbereitet" -> False,
            "Aktion" -> None
          |>,
      "Zoltan" ->
          <|
            "LE" -> 30,
            "LP" -> 30,
            "Kampf" -> 10,
            "INI" -> 37,
            Sequence @@ Normal@armor["Schwer"],
            Sequence @@ Normal@weapons["Barbarenstreitaxt"],
            "Stil" -> "Normal",
            "Pause" -> False,
            "Vorbereitet" -> False,
            "Aktion" -> None
          |>,
      "Xena" ->
          <|
            "LE" -> 30,
            "LP" -> 30,
            "Kampf" -> 15,
            "INI" -> 42,
            Sequence @@ Normal@armor["Mittel"],
            Sequence @@ Normal@weapons["Schwert"],
            "Stil" -> "Aggresiv",
            "Pause" -> False,
            "Vorbereitet" -> False,
            "Aktion" -> None
          |>
    |>;

(***********************************************************************)
Begin["`Private`"];
ClearAll["`*"];

d6 := RandomInteger[{1, 6}];
d20 := RandomInteger[{1, 20}];

pauseFighter[char_Association] :=
    MapAt[True&, char, "Pause"];

unpauseFighter[char_Association] :=
    (
      MapAt[False&, char, "Pause"]
          // action["Pause"]
    );

prepareFighter[char_Association] :=
    (
      MapAt[True&, char, "Vorbereitet"]
          // action["Vorbereitung"]
    );

unprepareFighter[char_Association] :=
    MapAt[False&, char, "Vorbereitet"];

action[act_String] := action[#, act]&;
action[char_Association, act_String] :=
    MapAt[act&, char, "Aktion"];

combatRound[
  chars_Association,
  aggressions_Association
] :=
    (
      chars
          // SortBy[#, {#INI - #BE&, RandomReal[]&}]&
          // Reverse@*Keys
          // Fold[fight[#1, #2, aggressions[#2]]&, chars, #]&
    );

fight[chars_Association, attacker_String, opponents : {}] := chars;

fight[chars_Association, attacker_String, opponents : {__String}] :=
    Catch[

      If[chars[attacker]["LP"] <= 0, Throw[chars, "fight"]];
      If[
        chars[attacker]["Pause"],
        Throw[MapAt[unpauseFighter, chars, attacker], "fight"]
      ];

      pickTarget[chars, attacker, opponents]
          // If[# === $NoTargets, Throw[chars, "fight"], #]&
          // attackFunction[chars, attacker -> #]&
      ,
      "fight"
    ];

pickTarget[chars_Association, attacker_String, opponents : {__String}] :=
    Catch[
      chars[[opponents]]
          // Select[#LP > 0&]
          // If[# === <||>, Throw[$NoTargets, "pickTarget"], #]&
          // SortBy[#Kampf&]
          // Last@*Keys
      ,
      "pickTarget"
    ];

attackFunction[chars_Association, atk_String -> def_String] :=
    Which[
      (
        chars[atk]["Stil"] === "Aggressiv"
            || chars[atk]["Stil"] === "SicherToedlich"
      ) && (
        chars[atk]["Kampf"]
            + chars[atk]["WM"]
            - chars[atk]["BE"]
            + If[chars[atk]["Vorbereitet"], 5, 0]
            - chars[def]["Kampf"]
            - chars[def]["WM"]
            + chars[def]["BE"]
            >= 5
      ) && (
        chars[def]["LP"] + chars[def]["RS"]
            > getBTP[chars[atk]["TP"]]
      ),
      If[
        chars[atk]["Vorbereitet"],
        sureDeathStrike[chars, atk -> def],
        deathStrike[chars, atk -> def]
      ]
      ,
      chars[atk]["Stil"] === "Berserk",
      If[
        chars[atk]["Vorbereitet"],
        sureDeathStrike[chars, atk -> def],
        deathStrike[chars, atk -> def]
      ]
      ,
      (
        chars[atk]["Stil"] === "Sicher"
            || chars[atk]["Stil"] === "SicherToedlich"
      )
          && Not@chars[atk]["Vorbereitet"]
          &&
          (chars[atk]["Kampf"]
              + chars[atk]["WM"]
              - chars[atk]["BE"]
              - chars[def]["Kampf"]
              - chars[def]["WM"]
              + chars[def]["BE"]
              < 5
          ),
      MapAt[prepareFighter, chars, atk]
      ,
      chars[atk]["Stil"] === "GanzSicher"
          && Not@chars[atk]["Vorbereitet"],
      MapAt[prepareFighter, chars, atk]
      ,
      chars[atk]["Stil"] === "SicherTödlich"
          && Not@chars[atk]["Vorbereitet"]
          &&
          (1 <=
              chars[atk]["Kampf"]
                  + chars[atk]["WM"]
                  - chars[atk]["BE"]
                  - chars[def]["Kampf"]
                  - chars[def]["WM"]
                  + chars[def]["BE"]
              < 5
          ) && (
        chars[def]["LP"] + chars[def]["RS"]
            > getBTP[chars[atk]["TP"]]
      ),
      MapAt[prepareFighter, chars, atk]
      ,
      True,
      If[
        chars[atk]["Vorbereitet"],
        sureStrike[chars, atk -> def],
        strike[chars, atk -> def]
      ]
    ];

sureDeathStrike[chars_Association, atk_String -> def_String] :=
    (
      deathStrike[chars, atk -> def, 5]
          // MapAt[action["SichererTodesschlag"], #, atk]&
          // MapAt[unprepareFighter, #, atk]&
    );

deathStrike[
  chars_Association,
  atk_String -> def_String,
  mod_Integer : 0
] :=
    Module[
      {
        attacker = chars[atk], defender = chars[def],
        attacks, defense, damage
      },

      attacks =
          Table[
            d20 + attacker["Kampf"] - attacker["BE"] + attacker["WM"]
                + mod,
            2];

      defense =
          10 + defender["Kampf"] - defender["BE"] + defender["WM"];

      damage =
          If[
            And @@ Thread[attacks >= defense],
            getTP[attacker["TP"]] + getTP[attacker["TP"]],
            attacker["BTP"]
          ];

      MapAt[dealDamage[#, damage]&, chars, def]
          // MapAt[action["Todesschlag"], #, atk]&
          // MapAt[pauseFighter, #, atk]&
    ];

sureStrike[chars_Association, atk_String -> def_String] :=
    (
      strike[chars, atk -> def, 5]
          // MapAt[action["SichererSchlag"], #, atk]&
          // MapAt[unprepareFighter, #, atk]&
    );

strike[chars_Association, atk_String -> def_String, mod_Integer : 0] :=
    Module[
      {
        attacker = chars[atk], defender = chars[def],
        attackRoll, defense, damage
      },

      attackRoll =
          d20 + attacker["Kampf"] - attacker["BE"] + attacker["WM"]
              + mod;

      defense =
          10 + defender["Kampf"] - defender["BE"] + defender["WM"];

      damage =
          If[
            attackRoll >= defense,
            getTP[attacker["TP"]],
            attacker["BTP"]
          ];

      MapAt[dealDamage[#, damage]&, chars, def]
          // MapAt[action["Schlag"], #, atk]&
    ];

dealDamage[char_Association, tp_Integer] :=
    With[
      {sp = Clip[tp - char["RS"], {0, Infinity}]},
      MapAt[# - sp&, char, "LP"]
    ];

getBTP[str_String] :=
    (
      First@StringCases[
        str,
        n : NumberString ~~ "W" ~~ m : NumberString ~~ p___
            :> Total[ToExpression /@ {n, p}]
      ]
    );

getTP[str_String] :=
    (
      StringCases[
        str,
        n : NumberString ~~ "W" ~~ m : NumberString ~~ p___
            :> ToExpression /@ {n, m, p}]
          // First
          // # /. Null -> 0&
          // Total@Table[RandomInteger@{1, #[[2]]}, #[[1]]] + #[[3]]&
    );

(***********************************************************************)
(* :Analysis: *)
combatPlot[traces : {{__Association}..}, chars_Association] :=
    ListPlot[
      traces[[All, #, "LP"]]& /@ Keys@chars
      , PlotRange -> Full
      , PlotTheme -> "Detailed"
      , FrameLabel -> {"KR", "LP"}
      , PlotLegends -> Keys@chars
      , DataRange -> {0, Length@First@traces - 1}
    ];

quantiles = {Quantile[#, 0.05], Quantile[#, 0.25], Quantile[#, 0.5]}&;

makeLines[quantiles_List] :=
    MapIndexed[
      {ColorData[1][First@#2], InfiniteLine[{{#1, 0}, {#1, 1}}]}&,
      quantiles
    ];

quantileLines[lst_List] :=
    Graphics[{Thickness -> 0.01, makeLines@quantiles@lst}];

LPHistogram[traces : {{__Association}..}, hero_String] :=
    With[
      {
        tr = traces[[All, -1, hero, "LP"]],
        bwidths = {Table[i, {i, -19, 33, 4}]}
      },

      Show[
        Histogram[
          tr, bwidths
          , PlotTheme -> "Detailed"
          , PlotLabel -> "Finale LE (" <> hero <> ")"
          , FrameLabel -> {"LP", "n"}
          , PlotRange -> {{-15, 35}, Automatic}
          , Epilog ->
            Inset[Framed["% Sieg: "
                <> ToString@Count[tr, x_ /; x > 0]
              , Background -> White]
              , Scaled[{0.75, 0.75}]
            ]
        ]
        ,
        Graphics[
          {Dashed, Purple, InfiniteLine[{{1, 0}, {1, 1}}]}
        ]
      (*
        ,
        quantileLines@tr
        *)
      ]
    ];

findEndOfCombat[
  trace : {__Association}
] :=
    (
      trace[[All, All, "LP"]]
          // Map[Values]
          // Map[Total]
          // First@FirstPosition[#, Min[#]] - 1&
    );

KRHistogram[traces : {{__Association}..}] :=
    With[
      {tr = findEndOfCombat /@ traces},

      Show[
        Histogram[
          tr, {1}
          , PlotTheme -> "Detailed"
          , PlotLabel -> "Kampfende"
          , FrameLabel -> {"KR", "n"}
          , PlotRange -> {{0, Length@First@traces - 1}, Automatic}
        ]
      (*
        ,
        quantileLines@tr
        *)
      ]
    ];

monteCarloRPG[
  chars_Association,
  hero_String,
  aggressions_Association,
  kr_Integer,
  n_Integer
] :=
    Module[
      {
        traces =
            Table[NestList[combatRound[#, aggressions]&, chars, kr], n],
        opponents = aggressions[hero]
      },
      <|
        "Traces" -> traces,
        "SampleTracePlot" -> combatPlot[First@traces, chars],
        "LPHistogram" -> LPHistogram[traces, hero],
        "KRHistogram" -> KRHistogram[traces]
      |>
    ];

(***********************************************************************)
End[];
EndPackage[];