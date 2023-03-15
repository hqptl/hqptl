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

module Frontend.BooleanPrograms 

open System
open System.Collections.Generic

open FsOmegaLib.LTL

open HQPTL.HyperQPTL

open ExplictTransitionSystem

exception private NotWellFormedException of String

type Var = String 

type Value = list<bool>

type VarAssignment = Map<Var, Value>

type ProgramExpression = 
    | True 
    | False
    | Variable of Var
    | And of ProgramExpression * ProgramExpression
    | Or of ProgramExpression * ProgramExpression
    | Not of ProgramExpression 
    | Proj of ProgramExpression * int 
    | Concat of ProgramExpression * ProgramExpression
    | Dup of ProgramExpression * int 

module ProgramExpression =

    let rec usedVars (e : ProgramExpression) = 
        match e with 
            | True | False -> Set.empty
            | Variable x -> Set.singleton x
            | And(e1, e2) | Or(e1, e2) | Concat(e1, e2) -> Set.union (usedVars e1) (usedVars e2)
            | Not e | Proj(e, _) | Dup(e, _) -> usedVars e
            
    let rec eval (A : VarAssignment) (e : ProgramExpression) = 
        match e with 
            | True -> [true]
            | False -> [false]
            | Variable x -> A.[x]
            | And(e1, e2) -> 
                List.zip (eval A e1) (eval A e2)
                |> List.map (fun (x, y) -> x && y)
            | Or(e1, e2) ->
                List.zip (eval A e1) (eval A e2)
                |> List.map (fun (x, y) -> x || y)
            | Not e -> 
                eval A e |> List.map not
            | Proj(e, n) -> (eval A e).[n] |> List.singleton
            | Concat(e1, e2) -> List.append (eval A e1) (eval A e2)
            | Dup(e, n) -> 
                let t = eval A e
                List.init n (fun _ -> t)
                |> List.concat 

    let rec inferBitWidth (env : Map<Var, int>) (e : ProgramExpression) = 
        match e with 
            | True -> Some 1
            | False -> Some 1
            | Variable x -> Some env.[x]
            | And(e1, e2) -> 
                match inferBitWidth env e1, inferBitWidth env e2 with 
                    | Some i1, Some i2 when i1 = i2 -> Some i1 
                    | _ ->  None
            | Or(e1, e2) ->
                match inferBitWidth env e1, inferBitWidth env e2 with 
                    | Some i1, Some i2 when i1 = i2 -> Some i1 
                    | _ -> None
            | Not e -> 
                inferBitWidth env e
            | Proj(e, n) -> 
                match inferBitWidth env e with 
                    | Some b -> 
                        if n >= b then 
                            None 
                        else 
                            Some 1
                    | None -> None 
            | Concat(e1, e2) -> 
                match inferBitWidth env e1, inferBitWidth env e2 with 
                    | Some i1, Some i2-> Some (i1 + i2) 
                    | _ -> 
                        None
            | Dup(e, n) -> 
                match inferBitWidth env e with 
                    | Some b -> 
                        Some (n * b)
                    | None -> None 


type ProgramStatement = 
    | Terminated
    | Skip
    | Assignment of Var * ProgramExpression 
    | If of ProgramExpression * ProgramStatement * ProgramStatement
    | Nondet of ProgramStatement * ProgramStatement
    | Read of  Var 
    | Seq of list<ProgramStatement>
    | While of ProgramExpression * ProgramStatement

module ProgramStatement =

    let rec usedVars (s : ProgramStatement) = 
        match s with 
            | Terminated | Skip -> Set.empty
            | Assignment(v, e) ->
                Set.add v (ProgramExpression.usedVars e)
            | If(e, s1, s2) -> 
                [ProgramExpression.usedVars e; usedVars s1; usedVars s2]
                |> Set.unionMany
            | Nondet(s1, s2) ->
                Set.union (usedVars s1) (usedVars s2)
            | Read x -> 
                Set.singleton x 
            | Seq slist -> 
                slist 
                |> List.map usedVars
                |> Set.unionMany
            | While(e, s) -> 
                Set.union (ProgramExpression.usedVars e) (usedVars s)

    let rec isWellFormed dom (s : ProgramStatement) = 
        match s with 
            | Terminated -> true
            | Skip -> true
            | Assignment(v, e) ->
                match ProgramExpression.inferBitWidth dom e with 
                    | Some i ->  
                        i = dom.[v]
                    | None -> false
            | If(e, s1, s2) -> 
                match ProgramExpression.inferBitWidth dom e with 
                    | Some 1 ->  
                        isWellFormed dom s1 && isWellFormed dom s2
                    | _ -> false
            | Nondet(s1, s2) ->
                isWellFormed dom s1 && isWellFormed dom s2
            | Read _ -> 
                true 
            | Seq slist -> 
                slist 
                |> List.forall (fun x -> isWellFormed dom x)
            | While(e, s) -> 
                match ProgramExpression.inferBitWidth dom e with 
                    | Some 1 ->  
                        isWellFormed dom s
                    | _ -> false

    let rec oneStep(A : VarAssignment) (s : ProgramStatement) = 
        match s with 
            | Terminated -> Seq.singleton (Terminated, A)
            | Skip -> Seq.singleton (Terminated, A)
            | Assignment(v, e) ->
                let newVal = ProgramExpression.eval A e
                let newAssignment = Map.add v newVal A
                Seq.singleton (Terminated, newAssignment)
            | If(e, s1, s2) -> 
                let res = ProgramExpression.eval A e
                if res.[0] then 
                    Seq.singleton (s1, A)
                else
                    Seq.singleton (s2, A)
            | Nondet(s1, s2) ->
                seq {(s1, A); (s2, A)}
            | Read v -> 
                let width = A.[v].Length
                seq {for b in Util.computeBooleanPowerSet width do (Terminated,  Map.add v b A)}
            | Seq slist -> 
                match slist with 
                    | [] -> Seq.singleton (Terminated, A)
                    | [x] -> oneStep A x
                    | x::xs -> 
                        oneStep A x
                        |> Seq.map (fun (s, A') -> 
                            match s with 
                                | Terminated -> (Seq xs, A')
                                | t -> (Seq(t::xs), A')
                            )
            | While(e, s) -> 
                let res = ProgramExpression.eval A e
                if res.[0] then 
                    Seq.singleton (Seq[s; While(e, s)], A)
                else
                    Seq.singleton (Terminated, A)
                    
type BooleanProgram = 
    {
        Name : String
        DomainMap : Map<Var, int>
        Statement : ProgramStatement
    }

module BooleanProgram =
    let findError (p : BooleanProgram) = 
        try
            let usedVars = ProgramStatement.usedVars p.Statement

            usedVars
            |> Set.iter (fun x -> 
                if p.DomainMap.ContainsKey x |> not then 
                    raise <| NotWellFormedException $"Variable %s{x} is used the program but not defined in the domain."
                )

            if ProgramStatement.isWellFormed p.DomainMap p.Statement |> not then 
                raise <| NotWellFormedException "The bitwidths of the varaibles do not match. Type mismatch."

            None
        with 
        | NotWellFormedException msg -> Some msg


type BooleanProgramHyperQPTLAtom = 
    | BooleanProgramTraceAtom of String * int * TraceVariable
    | BooleanProgramPropAtom of PropVariable


type BooleanProgramHyperQPTL = 
    {
        QuantifierPrefix : list<HyperQPTLQuantifier>
        LTLMatrix : LTL<BooleanProgramHyperQPTLAtom>
    }

module BooleanProgramHyperQPTL = 
    let quantifiedTraceVariables (formula : BooleanProgramHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallTrace pi | ExistsTrace pi -> Some pi
            | _ -> None)
        
    let quantifiedPropVariables (formula : BooleanProgramHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallProp p | ExistsProp p -> Some p
            | _ -> None)


    let findError (formula : BooleanProgramHyperQPTL) = 
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
                | BooleanProgramPropAtom q -> 
                    if List.contains q propVars |> not then 
                        raise <| NotWellFormedException $"Propositional Variable %s{q} is used but not defined in the prefix"

                | BooleanProgramTraceAtom (_, _, n) -> 
                    if List.contains n traceVars |> not then 
                        raise <| NotWellFormedException $"Trace Variable %s{n} is used but not defined in the prefix"
                )
            None 
        with 
            | NotWellFormedException msg -> Some msg

module Compilation = 
    type ProgramState = ProgramStatement * VarAssignment

    let compileProgramToTS (P : BooleanProgram) (relevantAps : list<Var * int>) = 
        let initialState : ProgramState = (P.Statement, Map.map (fun _ x -> List.init x (fun _ -> false)) P.DomainMap)

        let allStates = new HashSet<_>()
        let queue = new Queue<_>()
        queue.Enqueue initialState
        allStates.Add initialState |> ignore

        let edgeDict = new Dictionary<_,_>()
        let apEvalDict = new Dictionary<_,_>()

        while queue.Count <> 0 do 
            let s = queue.Dequeue() 
            let p, A = s

            let sucs = 
                ProgramStatement.oneStep A p
                |> set

            let apEval =   
                relevantAps 
                |> List.map (fun (v, i) -> 
                    A.[v].[i]
                    )

            for s' in sucs do 
                if allStates.Contains s' |> not then 
                    queue.Enqueue s' 
                    allStates.Add s' |> ignore

            edgeDict.Add(s, sucs)
            apEvalDict.Add(s, apEval)

        
        let renamingDict = 
            allStates
            |> Seq.mapi (fun i x -> x, i)
            |> Map.ofSeq

            
        {
            States = allStates |> Seq.map (fun x -> renamingDict[x]) |> set
            InitialStates = renamingDict[initialState] |> Set.singleton
            APs = relevantAps;
            Edges = 
                edgeDict 
                |> Seq.map (fun x -> x.Key, x.Value)
                |> Seq.map (fun (k, v) -> renamingDict[k], Set.map (fun x -> renamingDict[x]) v)
                |> Map.ofSeq
            ApEval = 
                apEvalDict 
                |> Seq.map (fun x -> x.Key, x.Value)
                |> Seq.map (fun (k, v) -> renamingDict[k], v)
                |> Map.ofSeq
        }






module Parser =
    open FParsec

    let private ws = spaces
    
    let private varParser = 
        many1Chars letter 

    let private expParser, private expParserRef = createParserForwardedToRef()

    let private trueParser = 
        stringReturn "t" True

    let private falseParser = 
        stringReturn "f" False 

    let private variableParser = 
        attempt
            (
                varParser 
                >>= (fun x -> if x <> "t" && x <> "f" then Variable x |> preturn else fail "")
            )
    
    let private andParser = 
        pstring "(&" >>. spaces >>.
            pipe2 
                expParser
                expParser
                (fun x y -> And(x, y))
        .>> spaces .>> pstring ")"

    let private orParser = 
        pstring "(|" >>. spaces >>.
            pipe2 
                expParser
                expParser
                (fun x y -> Or(x, y))
        .>> spaces .>> pstring ")"

    let private concatParser = 
        pstring "(@" >>. spaces >>.
            pipe2 
                expParser
                expParser
                (fun x y -> Concat(x, y))
        .>> spaces .>> pstring ")"

    let private notParser = 
        pstring "(!" >>. expParser .>> spaces .>> pstring ")"
        |>> Not

    let private parParser = 
        pstring "(" >>. expParser .>> pstring ")"

    let private projParser = 
        pstring "(#" >>.
            pipe2 
                expParser 
                (spaces >>. pint32)
                (fun x y -> Proj(x, y))
        .>> spaces .>> pstring ")" 

    let private dupParser = 
        pstring "(x" >>.
            pipe2 
                expParser 
                (spaces >>. pint32)
                (fun x y -> Dup(x, y))
        .>> spaces .>> pstring ")" 

    do expParserRef := 
        spaces >>. choice [
                andParser
                orParser
                concatParser
                projParser
                dupParser
                notParser
                parParser
                variableParser
                trueParser
                falseParser
        ] .>> spaces

    let private statementParser, private statementParserRef = createParserForwardedToRef() 

    let private assignmentParser = 
        pipe3 
            varParser
            (spaces .>> pstring ":=" .>> spaces)
            expParser
            (fun x _ y -> Assignment(x, y))

    let private ifParser = 
        pstring "IF" >>. spaces >>. pstring "(" >>.
            pipe3 
                (expParser .>> spaces .>> pstring ")" .>> spaces .>> pstring "THEN" .>> spaces .>> pstring "{")
                (statementParser .>> spaces .>> pstring "}" .>> spaces .>> pstring "ELSE" .>> spaces .>> pstring "{")
                (statementParser .>> spaces .>> pstring "}")
                (fun x y z -> If(x, y, z))
    
    let private ndetParser = 
        pstring "NONDET" >>. spaces >>. pstring "THEN" >>. spaces >>. pstring "{" >>.
            pipe2
                (statementParser .>> spaces .>> pstring "}" .>> spaces .>> pstring "ELSE" .>> spaces .>> pstring "{")
                (statementParser .>> spaces .>> pstring "}")
                (fun x y -> Nondet(x, y))

    let private inParser = 
        pstring "READ" >>. spaces >>. pstring "(" >>. varParser .>> spaces .>> pstring ")"
        |>> Read
    
    let private skipParser = 
        stringReturn "SKIP" Skip

    let private whileParser = 
        pstring "WHILE" >>. spaces >>. pstring "(" >>.
            pipe2
                (expParser .>> spaces .>> pstring ")" .>> spaces .>> pstring "{")
                (statementParser .>> spaces .>> pstring "}")
                (fun x y -> While(x, y))

    let private seqParser = 
        between (pchar '[') (pchar ']') (sepBy (statementParser .>> spaces) (pchar ';'))
        |>> Seq

    do statementParserRef := 
        spaces >>. choice [
                ifParser
                ndetParser
                inParser
                skipParser
                whileParser
                seqParser
                assignmentParser
        ] .>> spaces

    let private parseHeader = 
        spaces >>. pstring "dom:" >>. spaces >>. pchar '[' >>. spaces >>.
        sepBy (between (pchar '(') (pchar ')')  (varParser .>>. (spaces >>. pint32) .>> spaces)) (pchar ',' .>> spaces)
        .>> spaces .>> pchar ']'
        |>> Map.ofList

    let private programParser = 
        pipe2
            parseHeader
            (spaces >>. statementParser)
            (fun x y -> {BooleanProgram.DomainMap = x; Name = ""; Statement = y})

    let parseBooleanProgram (s : String) = 
        let full =
            programParser .>> spaces .>> eof

        let res = run full s

        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err

    // Parsing Of the Formula 

    let private relVarParserBit : Parser<(String * int), unit>= 
        pstring "{" >>. 
            pipe3
                (spaces >>. many1Chars letter)
                (pchar '_')
                (pint32 .>> pstring "}")
                (fun x _ y  -> (x, y))

    let private atomicParser =
            attempt (
                (relVarParserBit .>> pchar '_' .>>. Parser.traceVarParser)
                |>> fun ((x, i), n) -> BooleanProgramTraceAtom(x, i, n)
            )
            <|>
            (Parser.propVarParser |>> BooleanProgramPropAtom)

    let private hyperQPTLParser = 
        pipe2 
            Parser.hyperQPTLQuantifierPrefixParser 
            (FsOmegaLib.LTL.Parser.ltlParser atomicParser)
            (fun x y -> {BooleanProgramHyperQPTL.QuantifierPrefix = x; LTLMatrix = y})

    let parseBooleanProgramHyperQPTL s =    
        let full = hyperQPTLParser .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err