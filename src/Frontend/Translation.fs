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

module Frontend.Translation

open System 

open FsOmegaLib.LTL
open FsOmegaLib.GNBA

open HQPTL.HyperQPTL

open Util
open ExplictTransitionSystem
open NuSMV
open BooleanPrograms

let convertSymbolicSystemInstanceToGNBA (plist : list<SmvProgram>, formula : SymbolicHyperQPTL) = 
    match SymbolicHyperQPTL.findError formula with 
    | None -> () 
    | Some msg -> 
        raise <| FrontendException $"Error in the specification: %s{msg}"

    plist 
    |> List.iteri (fun i p -> 
        match NuSMV.SmvProgram.findError p with 
        | None -> () 
        | Some msg -> 
            raise <| FrontendException $"Error in the %i{i}th system: %s{msg}"
        )

    if plist.Length <> 1 && plist.Length <> formula.QuantifierPrefix.Length then 
        raise <| FrontendException $"Invalid number of programs"

    let nameMapping = 
        SymbolicHyperQPTL.quantifiedTraceVariables formula
        |> List.mapi (fun i x -> x, i)
        |> Map.ofList

    let unfoldRelationPredicate (atom : SymbolicExpressionAtom)  = 
        match atom with 
            | UnaryPred (e, n) -> 
                LTL.Atom ((e, n))
            | RelationalEq(e1, n1, e2, n2) -> 
                let t1 = e1 |> Expression.inferType (fun x -> (if plist.Length = 1 then plist.[0] else plist.[nameMapping[n1]]).VarTypes.[x])
                let t2 = e2 |> Expression.inferType (fun x -> (if plist.Length = 1 then plist.[0] else plist.[nameMapping[n2]]).VarTypes.[x])

                let t = 
                    match NuSMV.VariableType.intersectTypes t1 t2 with 
                        | Some x -> x 
                        | None -> 
                            raise <| FrontendException $"Error during unfolding: Could not intersect types %A{t1} and %A{t2} of expressions %A{e1} and %A{e2}."

                VariableType.allValues t
                |> List.map (fun v -> 
                    LTL.And(
                        LTL.Atom((Expression.Eq(e1, Const(v)), n1)),
                        LTL.Atom((Expression.Eq(e2, Const(v)), n2))
                    )
                )
                |> fun l -> if List.isEmpty l then LTL.False else l |> List.reduce (fun x y -> LTL.Or(x, y)) 

    let unfoldedHyperQPTL = 
        {
            HyperQPTL.QuantifierPrefix = formula.QuantifierPrefix
            HyperQPTL.LTLMatrix = 
                formula.LTLMatrix 
                |> LTL.bind (fun x -> 
                    match x with 
                    | SymbolicPropAtom q -> PropAtom q |> LTL.Atom
                    | SymbolicTraceAtom x -> 
                        x
                        |> unfoldRelationPredicate
                        |> LTL.map TraceAtom
                )
        }

    let tsList = 
        if plist.Length = 1 then 
            let atomList = 
                unfoldedHyperQPTL.LTLMatrix
                |> LTL.allAtoms
                |> Set.toList
                |> List.choose (fun x -> 
                    match x with 
                    | TraceAtom (x, _) -> Some x 
                    | PropAtom _ -> None
                    )
                |> List.distinct
                
            atomList
            |> List.iter (fun (v : Expression) ->
                Expression.allVars v
                |> Set.iter (fun x ->
                        if (Set.contains x plist.[0].Vars |> not) &&  (plist.[0].Define.Keys.Contains x |> not) then
                            raise <| FrontendException $"Variable %A{x} is used in the formula but not defined in the system"
                    )
            )
              
            convertProgramToTS plist.[0] atomList
            |> ExplictTransitionSystem.toGNBA
            |> List.singleton
        else 
            HyperQPTL.quantifiedTraceVariables unfoldedHyperQPTL
            |> List.map (fun n -> 
                let atomList = 
                    unfoldedHyperQPTL.LTLMatrix
                    |> LTL.allAtoms
                    |> Set.toList
                    |> List.choose (fun x -> 
                        match x with 
                        | TraceAtom (x, m) when n = m -> Some x 
                        | TraceAtom _ -> None
                        | PropAtom _ -> None
                        )
                    |> List.distinct

                atomList
                |> List.iter (fun (v : Expression) ->
                    Expression.allVars v
                    |> Set.iter (fun x ->
                            if Set.contains x plist.[nameMapping[n]].Vars |> not && (plist.[nameMapping[n]].Define.Keys.Contains x |> not) then
                                raise <| FrontendException $"Variable %A{x}, %s{n} is used in the formula but defined in the system for trace variable %s{n}"
                        )
                )
                    
                convertProgramToTS plist.[nameMapping[n]] atomList
                |> ExplictTransitionSystem.toGNBA
                )

    let renamingMap = 
        unfoldedHyperQPTL.LTLMatrix
        |> LTL.allAtoms
        |> Set.toList
        |> List.choose (fun x -> 
            match x with 
            | TraceAtom (x, _) -> Some x 
            | PropAtom _ -> None
            )
        |> List.mapi (fun i x -> x, "a" + string(i))
        |> Map.ofList

    let mappedTs = 
        tsList
        |> List.map (FsOmegaLib.GNBA.GNBA.mapAPs (fun x -> renamingMap[x]))
            
    let mappedFormula = 
        unfoldedHyperQPTL
        |> HyperQPTL.map (fun x -> renamingMap[x])

    mappedTs, mappedFormula


let convertBooleanProgramInstanceToGNBA (progList : list<BooleanProgram>, formula : BooleanProgramHyperQPTL) = 
    match BooleanProgramHyperQPTL.findError formula with 
    | None -> () 
    | Some msg -> 
        raise <| FrontendException $"Error in the specification: %s{msg}"

    progList 
    |> List.iteri (fun i p -> 
        match BooleanProgram.findError p with 
        | None -> () 
        | Some msg -> 
            raise <| FrontendException $"Error in the %i{i}th system: %s{msg}"
        )

    if progList.Length <> 1 && progList.Length <> formula.QuantifierPrefix.Length then 
        raise <| FrontendException $"Invalid number of programs"

    let nameMapping =
        formula
        |> BooleanProgramHyperQPTL.quantifiedTraceVariables 
        |> List.mapi (fun i x -> x, i)
        |> Map.ofList


    let unfoldedHyperQPTL = 
        {
            HyperQPTL.QuantifierPrefix = formula.QuantifierPrefix
            HyperQPTL.LTLMatrix = 
                formula.LTLMatrix 
                |> LTL.map (fun x -> 
                    match x with 
                    | BooleanProgramPropAtom q -> PropAtom q
                    | BooleanProgramTraceAtom (x, i, n) -> TraceAtom ((x, i), n)
                )
        }

    let tsList = 
        if progList.Length = 1 then 
            let prog = progList[0]

            let relevantAps = 
                unfoldedHyperQPTL.LTLMatrix
                |> LTL.allAtoms
                |> Set.toList
                |> List.choose (fun x -> 
                    match x with 
                    | TraceAtom (x, _) -> Some x 
                    | PropAtom _ -> None
                    )
                
            relevantAps
            |> List.iter (fun (v, i) ->
                if prog.DomainMap.ContainsKey v && prog.DomainMap.[v] > i |> not then
                    raise <| FrontendException $"AP (%A{v}, %i{i}) is used in the HyperQPTL property but variable %A{v} does not exists or has not the required bit width. Aborting."
                )

            BooleanPrograms.Compilation.compileProgramToTS prog relevantAps
            |> ExplictTransitionSystem.toGNBA
            |> List.singleton
        else 
            unfoldedHyperQPTL
            |> HyperQPTL.quantifiedTraceVariables
            |> List.map (fun n ->   
                let relevantAps = 
                    unfoldedHyperQPTL.LTLMatrix
                    |> LTL.allAtoms
                    |> Set.toList
                    |> List.choose (fun x -> 
                        match x with 
                        | TraceAtom (x, m) when n = m -> Some x 
                        | TraceAtom _ -> None
                        | PropAtom _ -> None
                        )
                    
                relevantAps
                |> List.iter (fun (v, j) ->
                    if progList.[nameMapping[n]].DomainMap.ContainsKey v && progList.[nameMapping[n]].DomainMap.[v] > j |> not then
                        raise <| FrontendException $"AP (%A{v}, %i{j}) is used in the HyperQPTL property but variable %A{v} does not exists or has not the required bit width. Aborting."
                    )
                
                BooleanPrograms.Compilation.compileProgramToTS progList.[nameMapping[n]] relevantAps
                |> ExplictTransitionSystem.toGNBA
            )
  
    let mappedTs = 
        tsList
        |> List.map (GNBA.mapAPs (fun (x, i) -> x + "@" + string(i)))
            
    let mappedFormula = 
        unfoldedHyperQPTL
        |> HyperQPTL.map (fun (x, i) -> x + "@" + string(i))

    mappedTs, mappedFormula

let convertExplictSystemInstanceToGNBA (systemList : list<ExplictTransitionSystem<String>>, formula : ExplictSystemHyperQPTL) = 
    let unfoldedHyperQPTL = 
        {
            HyperQPTL.QuantifierPrefix = formula.QuantifierPrefix
            HyperQPTL.LTLMatrix = 
                formula.LTLMatrix 
                |> LTL.map (fun x -> 
                    match x with 
                    | ExplictSystemPropAtom q -> PropAtom q
                    | ExplictSystemTraceAtom (x, n) -> TraceAtom (x, n)
                )
        }

    let tsList = 
        systemList |> List.map ExplictTransitionSystem.toGNBA
     
    tsList, unfoldedHyperQPTL