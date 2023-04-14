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

module QPTLTrans.QPTLTranslationUtil

open System.IO

open FsOmegaLib.LTL
open FsOmegaLib.GNBA
open FsOmegaLib.Conversion

open HQPTL.Util
open HQPTL.RunConfiguration
open HQPTL.HyperQPTL
open HQPTL.ModelChecking

open QPTL

let readAndParseQPTL propPath = 
    let propcontent =   
        try 
            File.ReadAllText propPath
        with 
        | _ -> raise <| AnalysisException $"Could not open/read file %s{propPath}"

    let qptl =
        match QPTL.Parser.parseQPTL propcontent with 
        | Result.Ok x -> x
        | Result.Error err -> raise <| AnalysisException $"The QPTL formula could not be parsed. %s{err}"

    qptl


let convertQPTLToGNBA (config : SolverConfiguration) (formula: QPTL<PropVariable>) (useOwl: bool) (timeout: int option)  = 
    let prefix =
        formula.QuantifierPrefix
        |> List.map (fun x -> 
            match x with 
            | QPTLForall q -> ForallProp q 
            | QPTLExists q -> ExistsProp q)
        
    let ltlMatrix =
        formula.LTLMatrix
        |> LTL.map (fun x -> PropAtom x)
            
    let possiblyNegatedAut = HQPTL.ModelChecking.generateAutomaton config Map.empty Set.empty prefix ltlMatrix useOwl timeout
    
    let aut = 
        if possiblyNegatedAut.IsNegated then
            // The automaton is negated, so we need to negate once more
            match FsOmegaLib.Conversion.AutomataOperations.complementToGNBA HQPTL.Util.DEBUG config.GetMainPath config.GetAutfiltPath (Effort.LOW) None possiblyNegatedAut.Aut with
            | Success x -> x
            | Fail err -> raise <| AnalysisException err
            | Timeout -> raise <| TimeoutException
        else
            possiblyNegatedAut.Aut
    
    // We can assume that no Trace Atoms are conatined in the APs, as the formula we started from also has not trace atoms
    aut
    |> GNBA.mapAPs (fun x ->
        match x with
        | PropAtom y -> y
        | TraceAtom _ -> raise <| AnalysisException "Should not happen")
    
let convertQPTLToNBA (config : SolverConfiguration) (formula: QPTL<PropVariable>) (useOwl: bool) (timeout: int option) =
    let gnba = convertQPTLToGNBA config formula useOwl timeout
    
    match FsOmegaLib.Conversion.AutomatonConversions.convertToNBA HQPTL.Util.DEBUG config.GetMainPath config.GetAutfiltPath (Effort.LOW) timeout gnba with
    | Success x -> x
    | Fail err -> raise <| AnalysisException err
    | Timeout -> raise <| TimeoutException
  