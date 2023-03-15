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
    
let rec private generateAutomatonRec (config : Configuration) (tsMap : Map<TraceVariable, GNBA<int, 'L>>) (quantifierPrefix : list<HyperQPTLQuantifier>) (possiblyNegatedAut : PossiblyNegatedAutomaton<'L>) = 
    
    if quantifierPrefix.IsEmpty then
        possiblyNegatedAut   
    else
        let lastQuantifier = List.last quantifierPrefix

        let remainingPrefix = quantifierPrefix[..quantifierPrefix.Length - 2]

        match lastQuantifier with
        | ExistsTrace pi  -> 
            let positiveAut = 
                if possiblyNegatedAut.IsNegated then
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath (Effort.LOW) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath (Effort.LOW) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            let nextAut = constructAutomatonSystemProduct positiveAut tsMap.[pi] pi
            generateAutomatonRec config tsMap remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = false}
            
        | ForallTrace pi -> 
            let negativeAut = 
                if not possiblyNegatedAut.IsNegated then 
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath (Effort.LOW) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath (Effort.LOW) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            let nextAut = constructAutomatonSystemProduct negativeAut tsMap.[pi] pi
            generateAutomatonRec config tsMap remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = true}
        | ExistsProp p ->
            let positiveAut = 
                if possiblyNegatedAut.IsNegated then
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath (Effort.LOW) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath (Effort.LOW) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            let nextAut = projectAwayAP positiveAut p
            generateAutomatonRec config tsMap remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = false}
            
        | ForallProp p ->
            let negativeAut = 
                if not possiblyNegatedAut.IsNegated then 
                    // Negate
                    match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath (Effort.LOW) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException
                else 
                    // Pass into spot (without any changes to the language) to enable easy simplication
                    match FsOmegaLib.Conversion.AutomatonConversions.convertToGNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath (Effort.LOW) None possiblyNegatedAut.Aut with
                    | Success x -> x
                    | Fail err -> raise <| AnalysisException err
                    | Timeout -> raise <| TimeoutException

            let nextAut = projectAwayAP negativeAut p
            generateAutomatonRec config tsMap remainingPrefix {PossiblyNegatedAutomaton.Aut = nextAut; IsNegated = true}


let generateAutomaton (config : Configuration)  (tsmap : Map<TraceVariable, GNBA<int, 'L>>) (quantifierPrefix : list<HyperQPTLQuantifier>) (formula : LTL<HyperQPTLAtom<'L>>) (timeout: int option) = 
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

    let aut =
        match FsOmegaLib.Conversion.LTLConversion.convertLTLtoGNBA Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.Ltl2tgbaPath timeout body with 
        | Success aut -> aut 
        | Fail err -> raise <| AnalysisException err 
        | Timeout -> raise TimeoutException
    
    generateAutomatonRec config tsmap quantifierPrefix {PossiblyNegatedAutomaton.Aut = aut; IsNegated = startWithNegated}
    
let modelCheck (config : Configuration)  (tsmap : Map<TraceVariable, GNBA<int, 'L>>) (hyperqptl : HyperQPTL<'L>) timeout =
    let possiblyNegatedAut = generateAutomaton config tsmap hyperqptl.QuantifierPrefix hyperqptl.LTLMatrix timeout
    let aut = possiblyNegatedAut.Aut
    let isNegated = possiblyNegatedAut.IsNegated
    
    assert (aut.APs.Length = 0)
        
    match FsOmegaLib.Conversion.AutomataChecks.checkEmptiness Util.DEBUG config.SolverConfig.MainPath config.SolverConfig.AutfiltPath None aut with
    | Success isEmpty ->
        if isNegated then
            isEmpty
        else
            not isEmpty
    | Fail err -> raise <| AnalysisException err
    | Timeout -> raise <| TimeoutException