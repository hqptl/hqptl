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
open FsOmegaLib.GNBA
open FsOmegaLib.Conversion

open Util
open RunConfiguration
open HyperQPTL

let private constructAutomatonSystemProduct (aut : GNBA<int, HyperQPTLAtom<'L>>) (ts : GNBA<int, 'L>) (index : TraceVariable) =
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
    
    (aut, tsGNBA)
    ||> AutomataUtil.constructConjunctionOfGnbaPair 
    |> GNBA.projectToTargetAPs newAPs
    
    
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
    
let rec private generateAutomatonRec (config : SolverConfiguration) (tsMap : Map<TraceVariable, GNBA<int, 'L>>) (quantifierPrefix : list<HyperQPTLQuantifier>) (possiblyNegatedAut : PossiblyNegatedAutomaton<'L>) = 
    
    if quantifierPrefix.IsEmpty then
        possiblyNegatedAut   
    else
        let lastQuantifier = List.last quantifierPrefix

        let remainingPrefix = quantifierPrefix[..quantifierPrefix.Length - 2]

        match lastQuantifier with
        | ExistsTrace pi  -> 
            let positiveAut = 
                if possiblyNegatedAut.IsNegated then
                    Util.LOGGER $"Start automaton negation for \"exists %s{pi}\" ..."
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.MainPath config.AutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    Util.LOGGER $"Start automaton simplification for \"exists %s{pi}\" ..."
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.MainPath config.AutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            Util.LOGGERn $"Done"

            Util.LOGGER $"Start automaton-system-product for \"exists %s{pi}\" ..."
            let nextAut = constructAutomatonSystemProduct positiveAut tsMap.[pi] pi
            Util.LOGGERn $"Done"

            generateAutomatonRec config tsMap remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = false}
            
        | ForallTrace pi -> 
            let negativeAut = 
                if not possiblyNegatedAut.IsNegated then 
                    Util.LOGGER $"Start automaton negation for \"forall %s{pi}\" ..."
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.MainPath config.AutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    Util.LOGGER $"Start automaton simplification for \"forall %s{pi}\" ..."
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.MainPath config.AutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            Util.LOGGERn $"Done"

            Util.LOGGER $"Start automaton-system-product for \"forall %s{pi}\" ..."
            let nextAut = constructAutomatonSystemProduct negativeAut tsMap.[pi] pi
            Util.LOGGERn $"Done"

            generateAutomatonRec config tsMap remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = true}
        | ExistsProp p ->
            let positiveAut = 
                if possiblyNegatedAut.IsNegated then
                    Util.LOGGER $"Start automaton negation for \"E %s{p}\" ..."
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.MainPath config.AutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    Util.LOGGER $"Start automaton simplification for \"E %s{p}\" ..."
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.MainPath config.AutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            Util.LOGGERn $"Done"


            Util.LOGGER $"Start automaton-system-product for \"E %s{p}\" ..."
            let nextAut = projectAwayAP positiveAut p
            Util.LOGGERn $"Done"

            generateAutomatonRec config tsMap remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = false}
            
        | ForallProp p ->
            let negativeAut = 
                if not possiblyNegatedAut.IsNegated then 
                    Util.LOGGER $"Start automaton negation for \"A %s{p}\" ..."
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.MainPath config.AutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    Util.LOGGER $"Start automaton simplification for \"A %s{p}\" ..."
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.MainPath config.AutfiltPath (Effort.HIGH) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            Util.LOGGERn $"Done"

            Util.LOGGER $"Start automaton-system-product for \"A %s{p}\" ..."
            let nextAut = projectAwayAP negativeAut p
            Util.LOGGERn $"Done"

            generateAutomatonRec config tsMap remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = true}


let generateAutomaton (config : SolverConfiguration)  (tsmap : Map<TraceVariable, GNBA<int, 'L>>) (quantifierPrefix : list<HyperQPTLQuantifier>) (formula : LTL<HyperQPTLAtom<'L>>) (timeout: int option) = 
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

    let aut =
        match FsOmegaLib.Conversion.LTLConversion.convertLTLtoGNBA Util.DEBUG config.MainPath config.Ltl2tgbaPath timeout body with 
        | Success aut -> aut 
        | Fail err -> raise <| AnalysisException err 
        | Timeout -> raise TimeoutException

    Util.LOGGERn $"Done"

    generateAutomatonRec config tsmap quantifierPrefix {PossiblyNegatedAutomaton.Aut = aut; IsNegated = startWithNegated}
    
let modelCheck (config : SolverConfiguration)  (tsMap : Map<TraceVariable, GNBA<int, 'L>>) (hyperqptl : HyperQPTL<'L>) timeout =

    let tsMapSimplified = 
        tsMap
        |> Map.map (fun _ gnba -> 
            if Util.simplifySystem then 
                // We pass the gnba to spot to enable easy preprocessing 
                Util.LOGGER $"Start initial system simplification..."
                let gnba' = 
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.MainPath config.AutfiltPath (Effort.HIGH) None gnba with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

                Util.LOGGERn $"Done"

                gnba'
            else 
                gnba
            )




    let possiblyNegatedAut = generateAutomaton config tsMapSimplified hyperqptl.QuantifierPrefix hyperqptl.LTLMatrix timeout
    let aut = possiblyNegatedAut.Aut
    let isNegated = possiblyNegatedAut.IsNegated
    
    assert (aut.APs.Length = 0)

    Util.LOGGER $"Start Emptiness Check..."

    let res = 
        match FsOmegaLib.Conversion.AutomataChecks.checkEmptiness Util.DEBUG config.MainPath config.AutfiltPath None aut with
        | Success isEmpty ->
            if isNegated then
                isEmpty
            else
                not isEmpty
        | Fail err -> raise <| AnalysisException err
        | Timeout -> raise <| TimeoutException

    Util.LOGGERn $"Done"

    res 