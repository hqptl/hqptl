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

module Frontend.ExplictTransitionSystem

open System

open FsOmegaLib.SAT
open FsOmegaLib.LTL
open FsOmegaLib.AutomatonSkeleton
open FsOmegaLib.GNBA

open HQPTL.HyperQPTL

exception private NotWellFormedException of String

type ExplictTransitionSystem<'L when 'L : comparison> = 
    {
        States : Set<int>
        InitialStates : Set<int>
        APs : list<'L>
        Edges : Map<int, Set<int>>
        ApEval : Map<int, list<bool>>
    }

module ExplictTransitionSystem =
    let mapAPs (f : 'L -> 'U) (ts : ExplictTransitionSystem<'L>) = 
        {
            States = ts.States
            InitialStates = ts.InitialStates
            APs = ts.APs |> List.map f
            Edges = ts.Edges
            ApEval = ts.ApEval
        }
       
    let findError (ts : ExplictTransitionSystem<'L>) =
        try 
            ts.States
            |> Seq.iter (fun x -> 
                if ts.Edges.ContainsKey x |> not then 
                    raise <| NotWellFormedException $"No successor defined for state $i{x}"

                if ts.ApEval.ContainsKey x |> not then 
                    raise <| NotWellFormedException $"No AP-Eval defined for state $i{x}"

                if ts.ApEval.[x].Length <> ts.APs.Length then 
                    raise <| NotWellFormedException $"The AP-Eval for state $i{x} does not match the length of the defined APs"
            )
            
            
            ts.InitialStates
            |> Seq.iter (fun x -> 
                if ts.States.Contains x |> not then 
                    raise <| NotWellFormedException $"State $i{x} is initial but not conatined in the set of states"
            )

            ts.Edges
            |> Map.toSeq
            |> Seq.iter (fun (k, x) ->
                x
                |> Seq.iter (fun z -> 
                    if ts.States.Contains z |> not then 
                        raise <| NotWellFormedException $"State $i{z} is a successor of states %i{k} but not defined as a state."
                )
            )

            None 
        with 
        | NotWellFormedException msg -> Some msg


    let toGNBA (ts : ExplictTransitionSystem<'L>) =
        {
            GNBA.Skeleton =
                {
                    AutomatonSkeleton.States = ts.States
                    APs = ts.APs
                    Edges =
                        ts.Edges
                        |> Map.map (fun _ x ->
                            x
                            |> Set.toList
                            |> List.map (fun t -> DNF.trueDNF, t)
                            )
                }
            InitialStates = ts.InitialStates
            AcceptanceSets =
                ts.States
                |> Set.toSeq
                |> Seq.map (fun x -> x, Set.empty)
                |> Map.ofSeq
            NumberOfAcceptingSets = 0
        }
    


type ExplictSystemHyperQPTLAtom = 
    | ExplictSystemTraceAtom of String * TraceVariable
    | ExplictSystemPropAtom of PropVariable


type ExplictSystemHyperQPTL = 
    {
        QuantifierPrefix : list<HyperQPTLQuantifier>
        LTLMatrix : LTL<ExplictSystemHyperQPTLAtom>
    }

module ExplictSystemHyperQPTL = 
    let quantifiedTraceVariables (formula : ExplictSystemHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallTrace pi | ExistsTrace pi -> Some pi
            | _ -> None)
        
    let quantifiedPropVariables (formula : ExplictSystemHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallProp p | ExistsProp p -> Some p
            | _ -> None)


    let findError (formula : ExplictSystemHyperQPTL) = 
        let propVars = quantifiedPropVariables formula
            
        let traceVars = quantifiedTraceVariables formula

        try 
            if propVars |> set |> Set.count <> List.length propVars then 
                raise <| NotWellFormedException $"Some propositional variable is used more than once."

            if traceVars |> set |> Set.count <> List.length traceVars then 
                raise <| NotWellFormedException $"Some trace variable is used more than once."

            LTL.allAtoms formula.LTLMatrix
            |> Set.iter (fun x -> 
                match x with 
                | ExplictSystemPropAtom q -> 
                    if List.contains q propVars |> not then 
                        raise <| NotWellFormedException $"Propositional Variable %s{q} is used but not defined in the prefix"

                | ExplictSystemTraceAtom (_, n) -> 
                    if List.contains n traceVars |> not then 
                        raise <| NotWellFormedException $"Trace Variable %s{n} is used but not defined in the prefix"
                )
            None 
        with 
            | NotWellFormedException msg -> Some msg

           
module Parser = 
    open FParsec 

    let private apsatParser = 
        let trueParser = 
            charReturn 't' true 
        let falseParser = 
            charReturn 'f' false 
        skipChar '[' >>. spaces >>. many ((trueParser <|> falseParser) .>> spaces)  .>> spaces .>> skipChar ']'

    let private stateParser = 
        pstring "State:" >>.
            pipe3
                (spaces >>. pint32)
                (spaces >>. apsatParser)
                (spaces >>. many (pint32 .>> spaces))
                (fun id ap sucs -> (id, (sucs, ap)))

    let private bodyParser = 
        spaces >>. many (stateParser .>> spaces)

    let private escapedStringParser : Parser<string, unit> = 
        skipChar '\"' >>. manyChars (satisfy (fun c -> c <> '\"')) .>> skipChar '\"' 

    let private tsParser = 
        pipe3
            (spaces >>. skipString "aps" >>. spaces >>. many1 (escapedStringParser .>> spaces))
            (spaces >>. skipString "init" >>. spaces >>. many1 (pint32 .>> spaces))
            (spaces >>. skipString "--BODY--" >>. bodyParser)
            (fun aps init st -> 
                {
                    ExplictTransitionSystem.States = st |> List.map fst |> set
                    InitialStates = set init
                    APs = aps;
                    Edges = 
                        st 
                        |> List.map (fun (k, (a, _)) -> k, set a)
                        |> Map.ofList
                    ApEval = 
                        st 
                        |> List.map (fun (k, (_, b)) -> k, b)
                        |> Map.ofList
                }       
                )
    
    let parseTS (s: string) =
        let full = tsParser .>> spaces .>> eof
        let res = run full s
        match res with
            | Success (res, _, _) -> Result.Ok res
            | Failure (err, _, _) -> 
                Result.Error ("Transition System could not be parsed: " + err)

    // Parsing Of the Formula 
    let private atomicParser =
        let escapedStringParser = 
                    skipChar '\"' >>. many1Chars (satisfy (fun c -> c <> '\"')) .>> skipChar '\"'

        attempt (
            (escapedStringParser .>> pchar '_' .>>. Parser.traceVarParser)
            |>> fun (x, n) -> ExplictSystemTraceAtom(x, n)
        )
        <|>
        (Parser.propVarParser |>> ExplictSystemPropAtom)

    let private hyperQPTLParser = 
        pipe2 
            Parser.hyperQPTLQuantifierPrefixParser 
            (FsOmegaLib.LTL.Parser.ltlParser atomicParser)
            (fun x y -> {ExplictSystemHyperQPTL.QuantifierPrefix = x; LTLMatrix = y})

    let parseExplictSystemHyperQPTL s =    
        let full = hyperQPTLParser .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err