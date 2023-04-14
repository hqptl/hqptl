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

module internal QPTLTrans.CommandLineParser

open System

type ExecutionMode = 
    | ConvertQPTLToNBA of String
    | ConvertQPTLToGNBA of String

type CommandLineArguments = 
    {
        ExecMode : option<ExecutionMode>
        Output : option<String>
        
        Timeout : Option<int>
        UseOwl : bool
        InitialSystemSimplification : bool
        IntermediateSimplification : bool
    }

    static member Default = 
        {
            ExecMode = None
            Output = None
            
            Timeout = None
            UseOwl = false
            InitialSystemSimplification = true
            IntermediateSimplification = true
        }

let rec private splitByPredicate (f : 'T -> bool) (xs : list<'T>) = 
    match xs with 
        | [] -> [], []
        | x::xs -> 
            if f x then 
                [], x::xs 
            else 
                let r1, r2 = splitByPredicate f xs 
                x::r1, r2

let parseCommandLineArguments (args : list<String>) =
    let rec parseArgumentsRec (args : list<String>) (opt : CommandLineArguments) = 

        match args with 
            | [] -> Result.Ok opt
            | x::xs -> 
                match x with 
                    | "--qptl-to-nba" -> 
                        if List.length xs < 1 then 
                            Result.Error "Option --qptl-to-nba must be followed by an argument"
                        else 
                            parseArgumentsRec (List.tail xs) {opt with ExecMode = ConvertQPTLToNBA(xs.[0])  |> Some}
                            
                    | "--qptl-to-gnba" -> 
                        if List.length xs < 1 then 
                            Result.Error "Option --qptl-to-gnba must be followed by an argument"
                        else 
                            parseArgumentsRec (List.tail xs) {opt with ExecMode = ConvertQPTLToGNBA(xs.[0])  |> Some}
                    | "-o" -> 
                        if List.length xs < 1 then 
                            Result.Error "Option -o must be followed by an argument"
                        else 
                            parseArgumentsRec (List.tail xs) {opt with Output = xs.[0] |> Some}
                    | "--owl" -> 
                        parseArgumentsRec xs {opt with UseOwl = true}
                    | "--no-simplification" -> 
                        parseArgumentsRec xs {opt with IntermediateSimplification = false}
                    | "--no-initial-simplification" -> 
                        parseArgumentsRec xs {opt with InitialSystemSimplification = false}
                    | "-t" -> 
                        match xs with 
                            | [] -> 
                                Result.Error "Option -t must be followed by an argument" 
                            | y::ys -> 
                                try     
                                    let vl = System.Int32.Parse y
                                    parseArgumentsRec ys { opt with Timeout = Some vl }
                                with _ -> Result.Error ("Unsupported timeout value: " + y)           
                    | _ -> Result.Error ("Option " + x + " is not supported" )
        
    parseArgumentsRec args CommandLineArguments.Default
                                