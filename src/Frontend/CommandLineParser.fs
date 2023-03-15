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

module Frontend.CommandLineParser

open System

type ExecutionMode = 
    | HanoiSystem // The native input format
    | SymbolicSystem
    | BooleanProgramSystem
    | ExplictSystem
    

type CommandLineArguments = 
    {
        InputType : option<ExecutionMode>
        InputFiles : option<list<String> * String>
        Verify : bool
        WriteExplicitSystems : bool
        ExplicitInstanceOutputFiles : option<list<String> * String>
        Timeout : Option<int>
    }

    static member Default = 
        {
            InputType = None 
            InputFiles = None
            Verify = false
            WriteExplicitSystems = false
            ExplicitInstanceOutputFiles = None
            Timeout = None
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
                    | "--hanoi" -> 
                        let args, ys = splitByPredicate (fun (x : String) -> x.[0] = '-') xs
            
                        if List.length args < 2 then 
                            Result.Error "Option --hanoi must be followed by at least two arguments"
                        else 
                            let propertyFile = args[args.Length - 1]
                            let systemFiles = args[0..args.Length - 2]
                            parseArgumentsRec ys {opt with InputType = Some HanoiSystem; InputFiles = (systemFiles, propertyFile)  |> Some}

                    | "--symbolic" -> 
                        let args, ys = splitByPredicate (fun (x : String) -> x.[0] = '-') xs
            
                        if List.length args < 2 then 
                            Result.Error "Option --symbolic must be followed by at least two arguments"
                        else 
                            let propertyFile = args[args.Length - 1]
                            let systemFiles = args[0..args.Length - 2]
                            parseArgumentsRec ys {opt with InputType = Some SymbolicSystem; InputFiles = (systemFiles, propertyFile)  |> Some}

                    | "--explicit" -> 
                        let args, ys = splitByPredicate (fun (x : String) -> x.[0] = '-') xs
            
                        if List.length args < 2 then 
                            Result.Error "Option --explicit must be followed by at least two arguments"
                        else 
                            let propertyFile = args[args.Length - 1]
                            let systemFiles = args[0..args.Length - 2]
                            parseArgumentsRec ys {opt with InputType = Some ExplictSystem; InputFiles = (systemFiles, propertyFile)  |> Some}

                    | "--bp" -> 
                        let args, ys = splitByPredicate (fun (x : String) -> x.[0] = '-') xs
            
                        if List.length args < 2 then 
                            Result.Error "Option --bp must be followed by at least two arguments"
                        else 
                            let propertyFile = args[args.Length - 1]
                            let systemFiles = args[0..args.Length - 2]
                            parseArgumentsRec ys {opt with InputType = Some BooleanProgramSystem; InputFiles = (systemFiles, propertyFile)  |> Some}
                    | "-o" -> 
                        let args, ys = splitByPredicate (fun (x : String) -> x.[0] = '-') xs

                        if List.isEmpty args then 
                            parseArgumentsRec ys {opt with ExplicitInstanceOutputFiles = None; WriteExplicitSystems = true}
                        elif List.length args < 2 then 
                            Result.Error "Option -o must be followed by either no argument or at least two arguments"
                        else 
                            let propertyFile = args[args.Length - 1]
                            let systemFiles = args[0..args.Length - 2]
                            parseArgumentsRec ys {opt with ExplicitInstanceOutputFiles = Some (systemFiles, propertyFile); WriteExplicitSystems = true}
                    | "-t" -> 
                        match xs with 
                            | [] -> 
                                Result.Error "Option -t must be followed by an argument" 
                            | y::ys -> 
                                try     
                                    let vl = System.Int32.Parse y
                                    parseArgumentsRec ys { opt with Timeout = Some vl }
                                with _ -> Result.Error ("Unsupported timeout option: " + y)         

                    | "--verify" -> 
                        parseArgumentsRec xs {opt with Verify = true}
                    
                    | _ -> Result.Error ("Option " + x + " is not supported" )
        
    parseArgumentsRec args CommandLineArguments.Default
                                