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
        InputType : ExecutionMode
        InputFiles : option<list<String> * String>
        Verify : bool
        WriteExplicitSystems : bool
        ExplicitInstanceOutputFiles : option<list<String> * String>
        Timeout : Option<int>
        DebugPrintouts : bool
        ComputeWitnessForOuterTraces : bool
    }

    static member Default = 
        {
            InputType = HanoiSystem 
            InputFiles = None
            Verify = true
            WriteExplicitSystems = false
            ExplicitInstanceOutputFiles = None
            Timeout = None
            DebugPrintouts = false
            ComputeWitnessForOuterTraces = false
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
            | x :: xs -> 
                match x with 
                    | "--hanoi" -> 
                        parseArgumentsRec xs {opt with InputType = HanoiSystem}
                    | "--symbolic" -> 
                        parseArgumentsRec xs {opt with InputType = SymbolicSystem}
                    | "--explicit" -> 
                        parseArgumentsRec xs {opt with InputType = ExplictSystem}
                    | "--bp" -> 
                        parseArgumentsRec xs {opt with InputType = BooleanProgramSystem}
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

                    | "--debug" -> 
                        parseArgumentsRec xs { opt with DebugPrintouts = true }
                    | "--no-verification" -> 
                        parseArgumentsRec xs {opt with Verify = false}
                    | "--witness" -> 
                        parseArgumentsRec xs {opt with ComputeWitnessForOuterTraces = true}
                    | s when s <> "" && s.Trim().StartsWith "-" -> 
                        Result.Error ("Option " + s + " is not supported" )
                    | x -> 
                        // When no option is given, we assume that this is the input 
                        if opt.InputFiles.IsSome then 
                            Result.Error "Input files cannot be given more than once"
                        else 
                            let args, ys = splitByPredicate (fun (y : String) -> y.[0] = '-') (x :: xs)
                
                            if List.length args < 2 then 
                                Result.Error "The input must consist of at least two arguments"
                            else 
                                let propertyFile = args[args.Length - 1]
                                let systemFiles = args[0..args.Length - 2]
                                parseArgumentsRec ys {opt with InputFiles = (systemFiles, propertyFile) |> Some}
        
    parseArgumentsRec args CommandLineArguments.Default
                                