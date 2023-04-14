(*    
    Copyright (C) 2022-2023 Raven Beutner

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

module HQPTL.ModelChecking

open FsOmegaLib.LTL
open FsOmegaLib.SAT
open FsOmegaLib.GNBA
open FsOmegaLib.NBA
open FsOmegaLib.Conversion

open Util
open RunConfiguration
open HyperQPTL
open AutomataUtil

let private constructAutomatonSystemProduct (aut : GNBA<int, HyperQPTLAtom<'L>>) (ts : GNBA<int, 'L>) (index : TraceVariable) (project : bool) =
    let tsGNBA =
        ts
        |> GNBA.mapAPs (fun x -> TraceAtom(x, index))
        
    let newAPs = 
        aut.APs
        |> List.filter (fun x ->
            match x with
            | TraceAtom (_, i) -> i <> index
            | PropAtom _ -> true
        )

    let product =
        (aut, tsGNBA)
        ||> AutomataUtil.constructConjunctionOfGnbaPair 

    if project then
        GNBA.projectToTargetAPs newAPs product 
    else 
        product
    
    
let private projectAwayAP (aut : GNBA<int, HyperQPTLAtom<'L>>) (index : PropVariable) =
    let newAPs = 
        aut.APs
        |> List.filter (fun x ->
            match x with
            | TraceAtom _ -> true
            | PropAtom p -> p <> index
        )
    
    aut
    |> GNBA.projectToTargetAPs newAPs
    
type PossiblyNegatedAutomaton<'L when 'L: comparison> =
    {
        Aut : GNBA<int, HyperQPTLAtom<'L>>
        IsNegated : bool 
    }

/// The trace variables in nonProjectedTraces will remain within the automataon language, i.e., they will not be projected away
let rec private generateAutomatonRec (config : SolverConfiguration) (tsMap : Map<TraceVariable, GNBA<int, 'L>>) (nonProjectedTraces : Set<TraceVariable>) (quantifierPrefix : list<HyperQPTLQuantifier>) (possiblyNegatedAut : PossiblyNegatedAutomaton<'L>) = 
    if quantifierPrefix.IsEmpty then
        possiblyNegatedAut   
    else
        let lastQuantifier = List.last quantifierPrefix

        let remainingPrefix = quantifierPrefix[..quantifierPrefix.Length - 2]

        match lastQuantifier with
        | ExistsTrace pi  -> 
            let sw = System.Diagnostics.Stopwatch()
            sw.Start()

            let positiveAut = 
                if possiblyNegatedAut.IsNegated then
                    Util.LOGGER $"Start automaton negation for \"exists %s{pi}\" ..."
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.GetMainPath config.GetAutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    Util.LOGGER $"Start automaton simplification for \"exists %s{pi}\" ..."
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.GetMainPath config.GetAutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

            Util.LOGGER $"Start automaton-system-product for \"exists %s{pi}\" ..."
            sw.Restart()
            let nextAut = constructAutomatonSystemProduct positiveAut tsMap.[pi] pi (Set.contains pi nonProjectedTraces |> not)
            Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

            generateAutomatonRec config tsMap nonProjectedTraces remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = false}
            
        | ForallTrace pi -> 
            let sw = System.Diagnostics.Stopwatch()
            sw.Start()

            let negativeAut = 
                if not possiblyNegatedAut.IsNegated then 
                    Util.LOGGER $"Start automaton negation for \"forall %s{pi}\" ..."
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.GetMainPath config.GetAutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    Util.LOGGER $"Start automaton simplification for \"forall %s{pi}\" ..."
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.GetMainPath config.GetAutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

            Util.LOGGER $"Start automaton-system-product for \"forall %s{pi}\" ..."
            sw.Restart()
            let nextAut = constructAutomatonSystemProduct negativeAut tsMap.[pi] pi (Set.contains pi nonProjectedTraces |> not)
            Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

            generateAutomatonRec config tsMap nonProjectedTraces remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = true}
        | ExistsProp p ->
            let sw = System.Diagnostics.Stopwatch()
            sw.Start()

            let positiveAut = 
                if possiblyNegatedAut.IsNegated then
                    Util.LOGGER $"Start automaton negation for \"E %s{p}\" ..."
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.GetMainPath config.GetAutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    Util.LOGGER $"Start automaton simplification for \"E %s{p}\" ..."
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.GetMainPath config.GetAutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"


            Util.LOGGER $"Start automaton-system-product for \"E %s{p}\" ..."
            sw.Restart()
            let nextAut = projectAwayAP positiveAut p
            Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

            generateAutomatonRec config tsMap nonProjectedTraces remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = false}
            
        | ForallProp p ->
            let sw = System.Diagnostics.Stopwatch()
            sw.Start()

            let negativeAut = 
                if not possiblyNegatedAut.IsNegated then 
                    Util.LOGGER $"Start automaton negation for \"A %s{p}\" ..."
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.GetMainPath config.GetAutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    Util.LOGGER $"Start automaton simplification for \"A %s{p}\" ..."
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.GetMainPath config.GetAutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

            Util.LOGGER $"Start automaton-system-product for \"A %s{p}\" ..."
            sw.Restart()
            let nextAut = projectAwayAP negativeAut p
            Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

            generateAutomatonRec config tsMap nonProjectedTraces remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = true}


let generateAutomaton (config : SolverConfiguration) (tsmap : Map<TraceVariable, GNBA<int, 'L>>) (nonProjectedTraces: Set<TraceVariable>) (quantifierPrefix : list<HyperQPTLQuantifier>) (formula : LTL<HyperQPTLAtom<'L>>) (useOwl : bool) (timeout: int option) = 
    let startWithNegated =
        if List.isEmpty quantifierPrefix then 
            false 
        else 
            match List.last quantifierPrefix with
            | ForallTrace _ | ForallProp _ -> true
            | ExistsTrace _ | ExistsProp _ -> false

    let body = 
        if startWithNegated then 
            LTL.Not formula
        else 
            formula

    Util.LOGGER $"Start LTL-to-NBA translation..."
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()

    let aut =
        if useOwl |> not then 
            match FsOmegaLib.Conversion.LTLConversion.convertLTLtoGNBA Util.DEBUG config.GetMainPath config.GetLtl2tgbaPath timeout body with 
            | Success aut -> aut 
            | Fail err -> raise <| AnalysisException err 
            | Timeout -> raise TimeoutException
        else 
            // Use Owl for the Ltl-GNBA translation
            match FsOmegaLib.Conversion.LTLConversion.convertLTLtoGNBAOwl Util.DEBUG config.GetMainPath config.GetOwlPath timeout body with 
            | Success aut -> aut 
            | Fail err -> raise <| AnalysisException err 
            | Timeout -> raise TimeoutException

    Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

    generateAutomatonRec config tsmap nonProjectedTraces quantifierPrefix {PossiblyNegatedAutomaton.Aut = aut; IsNegated = startWithNegated}
    
let modelCheck (config : SolverConfiguration) (tsMap : Map<TraceVariable, GNBA<int, 'L>>) (hyperqptl : HyperQPTL<'L>) (useOwl: bool) (computeWitness : bool) timeout =
    let tsMapSimplified = 
        tsMap
        |> Map.map (fun _ gnba -> 
            if Util.simplifySystem then 
                // We pass the gnba to spot to enable easy preprocessing 
                
                Util.LOGGER $"Start initial system simplification..."
                let sw = System.Diagnostics.Stopwatch()
                sw.Start()

                let gnba' = 
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.GetMainPath config.GetAutfiltPath (Effort.HIGH) None gnba with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

                Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

                gnba'
            else 
                gnba
            )

    // We determine which traces we do not project away to compute witnesses 
    // This is the largest prefix of the same qunatfier-type of the prefix 
    let nonProjectedTraces : Set<TraceVariable> = 
        if computeWitness |> not then 
            Set.empty
        else 
            let l =
                hyperqptl.QuantifierPrefix
                |> List.choose (function 
                    | ForallProp _ | ExistsProp _ -> None 
                    | ForallTrace pi -> Some (true, pi)
                    | ExistsTrace pi -> Some (false, pi)
                    )

            assert (List.isEmpty l |> not)

            // Find the first index that differs from the quantifiertype of the first variable (head of l)
            let a = 
                l 
                |> List.tryFindIndex (fun (y: bool * TraceVariable) -> fst y <> fst (List.head l))
                |> Option.defaultValue (List.length l)
                |> fun x -> x - 1

            l[..a] 
            |> List.map snd 
            |> set


    let possiblyNegatedAut = generateAutomaton config tsMapSimplified nonProjectedTraces hyperqptl.QuantifierPrefix hyperqptl.LTLMatrix useOwl timeout
    let aut = possiblyNegatedAut.Aut
    let isNegated = possiblyNegatedAut.IsNegated
   
    assert (computeWitness || List.isEmpty aut.APs)

    if not computeWitness then 
        // Just check for emptiness, we use spot for this

        Util.LOGGER $"Start Emptiness Check..."
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()

        let res = 
            match FsOmegaLib.Conversion.AutomataChecks.checkEmptiness Util.DEBUG config.GetMainPath config.GetAutfiltPath None aut with
            | Success isEmpty ->
                if isNegated then
                    isEmpty
                else
                    not isEmpty
            | Fail err -> raise <| AnalysisException err
            | Timeout -> raise <| TimeoutException

        Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

        res, None

    else 
        Util.LOGGER $"Start GNBA-to-NBA translation..."
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()

        let nba = 
            match FsOmegaLib.Conversion.AutomatonConversions.convertToNBA Util.DEBUG config.GetMainPath config.GetAutfiltPath Effort.HIGH None aut with
            | Success nba -> nba
            | Fail err -> raise <| AnalysisException err
            | Timeout -> raise <| TimeoutException

        Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

        Util.LOGGER $"Start lasso search in NBA..."
        sw.Restart()
        let res = AutomataUtil.shortestAcceptingPaths nba
        Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

        match res with 
        | None -> 
            // The automataon is empty 
            (if isNegated then true else false), None 
        | Some lasso -> 

            // We can assume that each NF in this lasso is SAT
            let makeDNFExplict (d : DNF<int>) = 
                // We take the smallest literal for printing
                let f = List.minBy List.length d 

                f 
                |> List.map (fun lit -> 
                    let ap = 
                        match nba.APs.[Literal.getValue lit] with 
                        | PropAtom _ -> raise <| AnalysisException $"Encountered a proptial AP in the lasso. "
                        | TraceAtom (a, pi) -> a, pi
                    
                    let isPos = match lit with PL _ -> true | NL _ -> false
                    isPos, ap 
                    )

            let modLasso = 
                {
                    Lasso.Prefix = lasso.Prefix |> List.map makeDNFExplict
                    Loop = lasso.Loop |> List.map makeDNFExplict
                }

            // The automaton is non-empty
            (if isNegated then false else true), (Some modLasso) 