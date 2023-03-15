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

module Frontend.NuSMV 

open System
open System.Collections.Generic

open FsOmegaLib.LTL

open HQPTL.HyperQPTL

open Util
open ExplictTransitionSystem

exception private NotWellFormedException of String

type VariableValue = 
    | BoolValue of bool 
    | IntValue of int 
    | SetValue of list<VariableValue>

type VariableType = 
    | BoolType 
    | IntType of int * int
    | SetType of VariableType

module VariableType =
    let rec allValues (t : VariableType) = 
        match t with 
            | BoolType -> 
                [BoolValue true; BoolValue false]
            | IntType(l, h) -> 
                [l..h]
                |> List.map IntValue
            | SetType t -> 
                allValues t
                |> set 
                |> Util.powerset
                |> Seq.map (fun x -> SetValue (Set.toList x))
                |> Seq.toList 

    let rec joinTypes t1 t2 = 
        match t1, t2 with 
            | BoolType, BoolType -> 
                Some BoolType
            | IntType(l1, h1), IntType(l2, h2) -> 
                Some (IntType(min l1 l2, max h1 h2))
            | SetType t1, SetType t2 ->
                joinTypes t1 t2 
                |> Option.map SetType
            | _ -> 
                None

    let rec intersectTypes t1 t2 = 
        match t1, t2 with 
            | BoolType, BoolType -> 
                Some BoolType
            | IntType(l1, h1), IntType(l2, h2) -> 
                Some (IntType(max l1 l2, min h1 h2))
            | SetType t1, SetType t2 ->
                intersectTypes t1 t2 
                |> Option.map SetType
            | _ -> 
                None

    let rec joinTypesAndExtend t1 t2 = 
        match t1, t2 with 
            | BoolType, BoolType -> 
                Some BoolType
            | IntType(l1, h1), IntType(l2, h2) -> 
                IntType(min l1 l2, max h1 h2)
                |> Some
            | SetType t1, SetType t2 ->
                joinTypes t1 t2 
                |> Option.map SetType
            | BoolType, SetType (BoolType) | SetType (BoolType), BoolType ->
                BoolType
                |> SetType
                |> Some
            | SetType (IntType(l1, h1)), IntType(l2, h2) | IntType(l1, h1), SetType (IntType(l2, h2)) -> 
                IntType(min l1 l2, max h1 h2)
                |> SetType
                |> Some
            | _ ->  
                None

    let rec haveSameBaseType t1 t2 =
        match t1, t2 with 
            | BoolType, BoolType -> 
                true
            | IntType _, IntType _ -> 
                true
            | SetType t1, SetType t2 ->
                haveSameBaseType t1 t2
            | _ -> 
                false

    let rec isValueOfType v t = 
        match v, t with 
            | BoolValue _ , BoolType -> 
                true 
            | IntValue x, IntType(a, b) -> 
                a <= x && x <= b
            | SetValue l, SetType t -> 
                l |> List.forall (fun x -> isValueOfType x t)
            | _, _ -> false

type Expression = 
    | Var of String 
    | Const of VariableValue 
    | Eq of Expression * Expression
    | Neq of Expression * Expression
    | Leq of Expression * Expression
    | Geq of Expression * Expression
    | Lt of Expression * Expression
    | Gt of Expression * Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Implies of Expression * Expression
    | Neg of Expression
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | SetExp of list<Expression>
    | Case of list<Expression * Expression>

module Expression =
    
    let rec allVars (e : Expression) =
        match e with 
            | Var s -> Set.singleton s
            | Const _ -> Set.empty
            | Eq(e1, e2) | Neq(e1, e2) | Leq(e1, e2) | Geq(e1, e2) | Lt(e1, e2) | Gt(e1, e2) | And(e1, e2) | Or(e1, e2) | Implies(e1, e2) | Add(e1, e2) | Sub(e1, e2) -> 
                Set.union (allVars e1) (allVars e2)
            | Neg e -> allVars e
            | SetExp l -> 
                l
                |> List.map allVars
                |> Set.unionMany
            | Case l ->
                l
                |> List.map (fun (x, y) -> Set.union (allVars x) (allVars y))
                |> Set.unionMany

    let rec print (e : Expression) = 
        match e with 
            | Var s -> s 
            | Const a -> 
                match a with 
                    | BoolValue b -> "C(" + string(b) + ")" 
                    | IntValue i -> "C(" + string(i) + ")" 
                    | SetValue _ -> 
                        failwith "Not supported yet."
            | Eq(e1, e2) -> 
                "(=" + print e1 + "," + print e2 + ")"
            | Neq(e1, e2) -> 
                "(!=" + print e1 + "," + print e2 + ")"
            | Leq(e1, e2) -> 
                "(<=" + print e1 + "," + print e2 + ")"
            | Geq(e1, e2) -> 
                "(>=" + print e1 + "," + print e2 + ")"
            | Lt(e1, e2) -> 
                "(<" + print e1 + "," + print e2 + ")"
            | Gt(e1, e2) -> 
                "(>" + print e1 + "," + print e2 + ")"
            | And(e1, e2) -> 
                "(&" + print e1 + "," + print e2 + ")"
            | Or(e1, e2) -> 
                "(|" + print e1 + "," + print e2 + ")"
            | Implies(e1, e2) -> 
                "(->" + print e1 + "," + print e2 + ")"
            | Add(e1, e2) -> 
                "(+" + print e1 + "," + print e2 + ")"
            | Sub(e1, e2) -> 
                "(-" + print e1 + "," + print e2 + ")"
            | Neg e -> 
                "(!" + print e + ")"
            | SetExp l -> 
                "({" + (l |> List.fold (fun s a -> s + print a) "")  + "})"
            | Case l -> 
                "(case { \n" + (l |> List.fold (fun s (g, e) -> s + print g + " : " + print e + "\n") "") + "}esac)"


    let rec inferType (env : String -> VariableType) (e : Expression) = 
        match e with 
            | Var s -> env s
            | Const x -> 
                match x with 
                    | BoolValue _ -> BoolType
                    | IntValue i -> IntType (i, i)
                    | SetValue x -> 
                        x 
                        |> List.map (fun x -> inferType env (Const x))
                        |> List.reduce (fun x y -> 
                            match VariableType.joinTypes x y with 
                                | Some x -> x 
                                | None -> 
                                    raise <| FrontendException "Error during NuSMV-to-Explicit compilation. Failed to join types."
                            )
                        |> SetType
            | Eq _ -> 
                BoolType
            | Neq _ -> 
                BoolType
            | Leq _ -> 
                BoolType
            | Geq _ -> 
                BoolType
            | Lt _ -> 
                BoolType
            | Gt _ -> 
                BoolType
            | And(e1, e2) -> 
                match inferType env e1, inferType env e2  with 
                    | BoolType, BoolType -> BoolType
                    | _ ->
                        raise <| FrontendException "Error during NuSMV-to-Explicit compilation."
            | Or(e1, e2) -> 
                match inferType env e1, inferType env e2  with 
                    | BoolType, BoolType -> BoolType
                    | _ ->
                        raise <| FrontendException "Error during NuSMV-to-Explicit compilation."
            | Implies(e1, e2) -> 
                match inferType env e1, inferType env e2  with 
                    | BoolType, BoolType -> BoolType
                    | _ ->
                        raise <| FrontendException "Error during NuSMV-to-Explicit compilation."
            | Neg e ->
                match inferType env e with 
                    | BoolType -> BoolType
                    | _ -> failwith ""
            | Add(e1, e2) -> 
                match inferType env e1, inferType env e2  with 
                    | IntType(l1, h1), IntType(l2, h2: int) -> IntType(l1 + l2, h1 + h2)
                    | _ ->
                        raise <| FrontendException "Error during NuSMV-to-Explicit compilation."
            | Sub(e1, e2) -> 
                match inferType env e1, inferType env e2  with 
                    | IntType(l1, h1), IntType(l2, h2: int) -> IntType(l1 - h2, h1 - l2)
                    | _ ->
                        raise <| FrontendException "Error during NuSMV-to-Explicit compilation."
            | SetExp e -> 
                e 
                |> List.map (fun x -> inferType env x)
                |> List.reduce (fun x y -> 
                        match VariableType.joinTypesAndExtend x y with 
                            | Some x -> x 
                            | None -> 
                                raise <| FrontendException $"Error during NuSMV-to-Explicit compilation. Failed to join types %A{x} and %A{y}."
                        )
                |> SetType 
            | Case (cases) -> 
                if cases |> List.exists (fun (g, _) -> inferType env g <> BoolType) then 
                    failwith ""
                else 
                    cases 
                    |> List.map (fun (_, e) -> inferType env e)
                    |> List.reduce (fun x y -> 
                        match VariableType.joinTypesAndExtend x y with 
                            | Some x -> x 
                            | None -> 
                                raise <| FrontendException $"Error during NuSMV-to-Explicit compilation. Failed to join types %A{x} and %A{y}."
                        )

    let rec eval (env : String -> VariableValue) (e : Expression) = 
        match e with 
            | Var s -> env s
            | Const x -> x
            | Eq(e1, e2) -> 
                let v1 = eval env e1 
                let v2 = eval env e2 
                BoolValue (v1 = v2)
            | Neq(e1, e2) -> 
                let v1 = eval env e1 
                let v2 = eval env e2 
                BoolValue (v1 <> v2)
            | Leq(e1, e2) -> 
                let v1 = eval env e1 
                let v2 = eval env e2 
                match v1, v2 with 
                    | IntValue i1, IntValue i2 -> 
                        BoolValue (i1 <= i2)
                    | _ -> 
                        raise <| FrontendException "Error during NuSMV-to-Explicit compilation. Invalid Comparison."
            | Geq(e1, e2) -> 
                let v1 = eval env e1 
                let v2 = eval env e2 
                match v1, v2 with 
                    | IntValue i1, IntValue i2 -> 
                        BoolValue (i1 >= i2)
                    | _ -> 
                        raise <| FrontendException "Error during NuSMV-to-Explicit compilation. Invalid Comparison."
            | Lt(e1, e2) -> 
                let v1 = eval env e1 
                let v2 = eval env e2 
                match v1, v2 with 
                    | IntValue i1, IntValue i2 -> 
                        BoolValue (i1 < i2)
                    | _ -> 
                        raise <| FrontendException "Error during NuSMV-to-Explicit compilation. Invalid Comparison."
            | Gt(e1, e2) -> 
                let v1 = eval env e1 
                let v2 = eval env e2 
                match v1, v2 with 
                    | IntValue i1, IntValue i2 -> 
                        BoolValue (i1 > i2)
                    | _ -> 
                        raise <| FrontendException "Error during NuSMV-to-Explicit compilation. Invalid Comparison."
                
            | And(e1, e2) -> 
                let v1 = eval env e1 
                let v2 = eval env e2 
                match v1, v2 with 
                    | BoolValue b1, BoolValue b2 -> 
                        BoolValue (b1 && b2)
                    | _ -> 
                        raise <| FrontendException $"Error during NuSMV-to-Explicit compilation. & is not supported for values {{%A{v1}}} and {{%A{v2}}}"
            | Or(e1, e2) -> 
                let v1 = eval env e1 
                let v2 = eval env e2 
                match v1, v2 with 
                    | BoolValue b1, BoolValue b2 ->     
                        BoolValue (b1 || b2)
                    | _ -> 
                        raise <| FrontendException $"Error during NuSMV-to-Explicit compilation. | is not supported for values {{%A{v1}}} and {{%A{v2}}}"
            | Implies(e1, e2) -> 
                let v1 = eval env e1 
                let v2 = eval env e2 
                match v1, v2 with 
                    | BoolValue b1, BoolValue b2 -> 
                        BoolValue ((not b1) || b2)
                    | _ -> 
                        raise <| FrontendException $"Error during NuSMV-to-Explicit compilation. -> is not supported for values {{%A{v1}}} and {{%A{v2}}}"
            | Neg e ->
                let v = eval env e 
                match v with 
                    | BoolValue b -> 
                        BoolValue (not b)
                    | _ -> 
                        raise <| FrontendException $"Error during NuSMV-to-Explicit compilation. ! is not supported for values {{%A{v}}}"
            | Add(e1, e2) -> 
                let v1 = eval env e1 
                let v2 = eval env e2 
                match v1, v2 with 
                    | IntValue i1, IntValue i2 -> 
                        IntValue (i1 + i2)
                    | _ -> 
                        raise <| FrontendException $"Error during NuSMV-to-Explicit compilation. + is not supported for values {{%A{v1}}} and {{%A{v2}}}"
            | Sub(e1, e2) -> 
                let v1 = eval env e1 
                let v2 = eval env e2 
                match v1, v2 with 
                    | IntValue i1, IntValue i2 -> 
                        IntValue (i1 - i2)
                    | _ -> 
                        raise <| FrontendException $"Error during NuSMV-to-Explicit compilation. - is not supported for values {{%A{v1}}} and {{%A{v2}}}"
            | SetExp e -> 
                e 
                |> List.map (fun x -> eval env x)
                |> SetValue 
            | Case cases -> 
                let firstHit = 
                    cases
                    |> List.tryFind (fun (g, _) -> eval env g = BoolValue true)

                match firstHit with 
                    | Option.None ->
                        raise <| FrontendException "Error during NuSMV-to-Explicit compilation. No case was matched."
                    | Some (_, e) -> 
                        eval env e
    
type SmvProgram = 
    {
        Vars : Set<String>
        VarTypes : Map<String, VariableType>;
        Init : Map<String, VariableValue>
        Next : Map<String, Expression>
        Define : Map<String, Expression>
    }

module SmvProgram =

    let rec findError (smv : SmvProgram) = 
        try 
            smv.Vars 
            |> Set.iter (fun x -> 
                if smv.VarTypes.ContainsKey x |> not then 
                    raise <| NotWellFormedException $"NuSMV is inconsistent: Variable Type for %s{x} is not defined."

                if smv.Init.ContainsKey x |> not then 
                    raise <| NotWellFormedException $"NuSMV is inconsistent: Init Condition for %s{x} is not defined."

                if smv.Next.ContainsKey x |> not then 
                    raise <| NotWellFormedException $"NuSMV is inconsistent: Next Condition for %s{x} is not defined."
                )

            smv.Vars 
            |> Set.iter (fun x -> 
                let v = smv.Init.[x]
                let t = smv.VarTypes.[x]

                if VariableType.isValueOfType v t || VariableType.isValueOfType v (SetType t) then 
                    ()
                else 
                    raise <| NotWellFormedException $"NuSMV is inconsistent: Initial Value for %s{x} is %A{v}, but the type is %A{t}. Type mismatch."
            )

            smv.Vars 
            |> Set.iter (fun x -> 
                let t = smv.VarTypes.[x]
                let e = smv.Next.[x]

                let rec typeLookup y = 
                    if smv.VarTypes.ContainsKey y then 
                        smv.VarTypes.[y]
                    elif smv.Define.ContainsKey y then 
                        let d = smv.Define.[y]
                        Expression.inferType typeLookup d
                    else 
                        raise <| NotWellFormedException $"NuSMV is inconsistent: Variable %s{y} is used in the next condition for variable %s{x}, but not defined."

                let infert = Expression.inferType typeLookup e

                if VariableType.haveSameBaseType infert t || VariableType.haveSameBaseType infert (SetType t) then 
                    () 
                else 
                    raise <| NotWellFormedException $"NuSMV is inconsistent: The next expression for variable %s{x} has type %A{infert} but should have the same basetype as %A{t} or %A{SetType t}. Type mismatch."
            )

            None
        with 
            | NotWellFormedException msg -> 
                Some msg


let convertProgramToTS (p : SmvProgram) (expressionList : list<Expression>) = 
    let rec evalWithDefinitions state (e : Expression) = 
        e 
        |> Expression.eval (fun x -> 
            if Set.contains x p.Vars then 
                state x 
            elif Map.containsKey x p.Define then 
                evalWithDefinitions state p.Define.[x]
            else 
                raise <| FrontendException $"Did not find a definition for |%s{x}|"
            )

    let allStates = new HashSet<_>()
    let queue = new Queue<_>()
    let edgeDict = new Dictionary<_,_>()
    let apEvalDict = new Dictionary<_,_>()
    
    assert(p.Vars = Set(p.VarTypes.Keys))

    let initStates = 
        p.Vars 
        |> Set.toList
        |> List.map (fun name -> 
            let v = p.Init.[name]

            let possibleValues = 
                if VariableType.isValueOfType v p.VarTypes.[name] then 
                    [v]
                else 
                    match v with 
                        | SetValue v -> 
                            if List.forall (fun x -> VariableType.isValueOfType x p.VarTypes.[name]) v then 
                                v
                            else 
                                raise <| FrontendException $"The value of the next expression {{%A{v}}} does not match its intended type {{%A{p.VarTypes.[name]}}}"
                        | _ -> 
                            raise <| FrontendException $"The value of the next expression {{%A{v}}} does not match its intended type {{%A{p.VarTypes.[name]}}}"

            name, set(possibleValues)
            )
        |> Map
        |> Util.cartesianProductMap
        |> HashSet

    for s in initStates do
        queue.Enqueue s
        allStates.Add s |> ignore

    while queue.Count <> 0 do 
        let state = queue.Dequeue()

        let allSucs = 
            p.Vars 
            |> Set.toList
            |> List.map (fun name -> 
                let v = evalWithDefinitions (fun x -> state.[x]) p.Next.[name]
                
                let nextValues = 
                    if VariableType.isValueOfType v p.VarTypes.[name] then 
                        [v]
                    else 
                        match v with 
                            | SetValue v -> 
                                if List.forall (fun x -> VariableType.isValueOfType x p.VarTypes.[name]) v then 
                                    v
                                else 
                                    raise <| FrontendException $"The value of the next expression {{%A{v}}} does not match its intended type {{%A{p.VarTypes.[name]}}}"
                            | _ -> 
                                raise <| FrontendException $"The value of the next expression {{%A{v}}} does not match its intended type {{%A{p.VarTypes.[name]}}}"

                name, set(nextValues)
                )
            |> Map
            |> Util.cartesianProductMap
            |> set

        let apEval = 
            expressionList
            |> List.map (fun x -> evalWithDefinitions (fun y -> state.[y]) x)
            |> List.map (fun x -> match x with BoolValue b -> b | _ -> failwith "")

        edgeDict.Add(state, allSucs)
        apEvalDict.Add(state, apEval)

        for s in allSucs do 
            if allStates.Contains s |> not then 
                queue.Enqueue s
                allStates.Add s |> ignore


    let renamingDict = 
        allStates
        |> Seq.mapi (fun i x -> x, i)
        |> Map.ofSeq
 
    {
        States = allStates |> Seq.map (fun x -> renamingDict[x]) |> set
        InitialStates = initStates |> Seq.map (fun x -> renamingDict[x]) |> set
        APs = expressionList;
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


type SymbolicExpressionAtom = 
    | UnaryPred of Expression * TraceVariable
    | RelationalEq of Expression * TraceVariable * Expression * TraceVariable

type SymbolicHyperQPTLAtom = 
    | SymbolicTraceAtom of SymbolicExpressionAtom
    | SymbolicPropAtom of PropVariable


type SymbolicHyperQPTL = 
    {
        QuantifierPrefix : list<HyperQPTLQuantifier>
        LTLMatrix : LTL<SymbolicHyperQPTLAtom>
    }

module SymbolicHyperQPTL = 
    let quantifiedTraceVariables (formula : SymbolicHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallTrace pi | ExistsTrace pi -> Some pi
            | _ -> None)
        
    let quantifiedPropVariables (formula : SymbolicHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallProp p | ExistsProp p -> Some p
            | _ -> None)


    let findError (formula : SymbolicHyperQPTL) = 
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
                | SymbolicPropAtom q -> 
                    if List.contains q propVars |> not then 
                        raise <| NotWellFormedException $"Propositional Variable %s{q} is used but not defined in the prefix"

                | SymbolicTraceAtom y -> 
                    match y with 
                    | UnaryPred (_, n) -> 
                        if List.contains n traceVars |> not then 
                            raise <| NotWellFormedException $"Trace Variable %s{n} is used but not defined in the prefix"

                    | RelationalEq (_, n1, _, n2) -> 
                        if List.contains n1 traceVars |> not then 
                            raise <| NotWellFormedException $"Trace Variable %s{n1} is used but not defined in the prefix"

                        if List.contains n2 traceVars |> not then 
                            raise <| NotWellFormedException $"Trace Variable %s{n2} is used but not defined in the prefix"  
                )
            None 
        with 
            | NotWellFormedException msg -> Some msg


module Parser = 
    open FParsec
    
    let private commentParser =
        (skipString "--" .>> restOfLine false)

    let private ws = spaces .>> sepEndBy commentParser spaces

    let private keywords = 
        [
            "MODULE"
            "ASSIGN"
            "VAR"
            "DEFINE"
            "init"
            "next"
            "TRUE"
            "FALSE"
            "case"
            "esac"
        ]

    let private varParser = 
        attempt(
            (letter <|> pchar '_') .>>. many1Chars (letter <|> digit <|> pchar '_' <|> pchar '$' <|> pchar '#' <|> pchar '-' <|> pchar '[' <|> pchar ']' <|> pchar '.') 
            >>= fun (x, y) -> (
                let s = string(x) + y
                if List.contains s keywords then 
                    fail ""
                else 
                    preturn s
                )
        )
        
    let private expParser = 
        let expParser, expParserRef = createParserForwardedToRef()

        let trueParser = 
            stringReturn "TRUE" (Const (BoolValue true))

        let falseParser = 
            stringReturn "FALSE" (Const (BoolValue false))

        let intParser = 
            pint32 
            |>> fun x -> (Const (IntValue x))

        let variableParser = 
            attempt
                (
                    varParser 
                    >>= (fun x -> if x <> "TRUE" && x <> "FALSE" then Var x |> preturn else fail "")
                )

        let setParser = 
            pchar '{' >>. sepBy (expParser .>> ws) (pchar ',' .>> ws) .>> pchar '}'
            |>> SetExp

        let caseParser = 
            pstring "case" >>. ws >>.
            sepEndBy (expParser .>> ws .>> pchar ':' .>> ws .>>. expParser) (pchar ';' .>> ws)
            .>> ws .>> pstring "esac"
            |>> Case

        let parParser = 
                skipChar '(' >>. ws >>. expParser .>> ws .>> skipChar ')'

        let basicParser = 
            ws >>. choice [ 
                caseParser
                trueParser
                falseParser
                intParser
                setParser
                parParser
                variableParser
            ] .>> ws

        let oppParser = new OperatorPrecedenceParser<Expression, unit, unit>()

        let addInfixOperator string precedence associativity f =
            oppParser.AddOperator(
                InfixOperator(string, ws, precedence, associativity, f)
            )

        let addPrefixOperator string precedence associativity f =
            oppParser.AddOperator(
                PrefixOperator(string, ws, precedence, associativity, f)
            )        

        do
            oppParser.TermParser <- basicParser

            addInfixOperator "&" 30 Associativity.Left (fun x y -> And(x, y))
            addInfixOperator "|" 20 Associativity.Left (fun x y -> Or(x, y))
            addInfixOperator "->" 10 Associativity.Right (fun x y -> Implies(x, y))
            addInfixOperator "=" 40 Associativity.Left (fun x y -> Eq(x, y))
            addInfixOperator "!=" 40 Associativity.Left (fun x y -> Neq(x, y))
            addInfixOperator "<=" 40 Associativity.Left (fun x y -> Leq(x, y))
            addInfixOperator ">=" 40 Associativity.Left (fun x y -> Geq(x, y))
            addInfixOperator "<" 40 Associativity.Left (fun x y -> Lt(x, y))
            addInfixOperator ">" 40 Associativity.Left (fun x y -> Gt(x, y))
            addInfixOperator "+" 50 Associativity.None (fun x y -> Add(x, y))
            addInfixOperator "-" 50 Associativity.None (fun x y -> Sub(x, y))

            addPrefixOperator "!" 60 true (fun x -> Neg x)
        do 
            expParserRef.Value <- oppParser.ExpressionParser

        expParser

    let private varTypeParser = 
        let varTypeParser, varTypeParserRef = createParserForwardedToRef()

        let rangeParser = 
            pint32 .>> pstring ".." .>>. pint32
            |>> (fun (x, y) -> IntType(x, y))

        let boolParser = 
            stringReturn "boolean" BoolType
            
        do 
            varTypeParserRef.Value <-
                ws >>. choice [ 
                    rangeParser
                    boolParser
                ] .>> ws

        varTypeParser

    let private initParser = 
        pstring "init(" >>. ws >>. varParser .>> ws .>> pchar ')' .>> ws .>> pstring ":=" .>> ws .>>. expParser
        |>> fun (x, y) -> (x, y)

    let private nextParser = 
        pstring "next(" >>. ws >>. varParser .>> ws .>> pchar ')' .>> ws .>> pstring ":=" .>> ws .>>. expParser
        |>> fun (x, y) -> (x, y)

    let private defineParser = 
        ws >>. varParser .>> ws .>> pstring ":=" .>> ws .>>. expParser
        |>> fun (x, y) -> (x, y)

    let private varWithTypeParser = 
        varParser 
        .>>. 
        (ws >>. pchar ':' >>. ws >>. varTypeParser)
        
    let private varDecParser = 
        ws >>. many (varWithTypeParser .>> ws .>> pchar ';' .>> ws)

    let private assignSectionParser = 
        many (
            (initParser .>> ws .>> pchar ';' .>> ws
            |>> fun x -> x, true)
            <|>
            (nextParser .>> ws .>> pchar ';' .>> ws
            |>> fun x -> x, false)
        )
        |>> fun x -> 
            let inits = 
                x 
                |> List.filter snd 
                |> List.map fst
            
            let nexts = 
                x 
                |> List.filter (snd >> not) 
                |> List.map fst

            inits, nexts

    let private defineDecParser = 
        many (defineParser .>> ws .>> pchar ';' .>> ws)

    let private programParser = 
        pipe4
            (ws >>. skipString "MODULE" >>. ws >>. (many1 letter))
            (ws >>. skipString "VAR" >>. varDecParser)
            (ws >>. skipString "ASSIGN" >>. ws >>. assignSectionParser)
            (ws >>. skipString "DEFINE" >>. ws >>. defineDecParser)
            (fun _ var (init, next) define ->   
                init 
                |> List.map fst
                |> List.countBy id 
                |> List.iter (fun (n, c) -> 
                    if c > 1 then 
                        raise <| FrontendException $"Error while parsing NuSMV Model: The initial condition for variable %s{n} is defined multiple times."
                    )

                next 
                |> List.map fst
                |> List.countBy id 
                |> List.iter (fun (n, c) -> 
                    if c > 1 then 
                        raise <| FrontendException $"Error while parsing NuSMV Model: The next condition for variable %s{n} is defined multiple times."
                    )

                
                {
                    SmvProgram.Vars = var |> List.map fst |> Set
                    SmvProgram.VarTypes = var |> Map.ofList
                    SmvProgram.Init = 
                        init 
                        |> List.map 
                            (fun (n, e) -> 
                                n, e |> Expression.eval (fun x -> raise <| FrontendException $"Error while parsing NuSMV Model: Only closed expression in init conditions. %s{x} is unbound in %s{n}, %A{e}")
                            )
                        |> Map.ofList
                    SmvProgram.Next = next |> Map.ofList
                    SmvProgram.Define = define |> Map.ofList
                }
                )
        
    let parseProgram (s : String) = 
        let full =
            programParser .>> spaces .>> eof

        let res = run full s

        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err

    let private symbolicHyperQPTLParser = 
        let unaryAtomParser = 
            pipe2
                (skipString "*" >>. expParser .>> spaces .>> skipChar '*')
                (spaces >>. skipChar '_' >>. (many1Chars letter))
                (fun e n -> UnaryPred(e, n))

        let relationalAtomParser = 
            pipe4
                (skipString "*" >>. expParser .>> spaces .>> skipChar '*')
                (spaces >>. skipChar '_' >>. (many1Chars letter) .>> spaces .>> skipString "=")
                (spaces >>. skipString "*" >>. expParser .>> spaces .>> skipChar '*')
                (spaces >>. skipChar '_' >>. (many1Chars letter))
                (fun e1 n1 e2 n2 -> RelationalEq(e1, n1, e2, n2))

        let symbolicAtomParser = 
            attempt(relationalAtomParser) <|> unaryAtomParser

        let symbplicHyperQPTLAtomParser =
            attempt (
                symbolicAtomParser
                |>> SymbolicTraceAtom
            )
            <|>
            (Parser.propVarParser |>> SymbolicPropAtom)

        pipe2 
            Parser.hyperQPTLQuantifierPrefixParser
            (FsOmegaLib.LTL.Parser.ltlParser symbplicHyperQPTLAtomParser)
            (fun x y -> {SymbolicHyperQPTL.QuantifierPrefix = x; LTLMatrix = y})

    let parseSymbolicHyperQPTL s =    
        let full = symbolicHyperQPTLParser .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err
