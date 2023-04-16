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

module Frontend.InstanceParsing 

open System.IO

open Util

let readAndParseHanoiInstance systemInputPaths formulaInputPath  = 
    let propcontent =   
        try 
            File.ReadAllText formulaInputPath
        with 
            | _ -> 
                raise <| FrontendException $"Could not open/read file %s{formulaInputPath}"
                
    let tsStringList = 
        systemInputPaths
        |> List.map (fun x -> 
                try 
                    File.ReadAllText  x
                with 
                    | _ -> 
                        raise <| FrontendException $"Could not open/read file %s{x}"
            )

    let formula =
        match HQPTL.HyperQPTL.Parser.parseHyperQPTL HQPTL.Util.ParserUtil.escapedStringParser propcontent with 
            | Result.Ok x -> x
            | Result.Error err -> 
                raise <| FrontendException $"The HyperQPTL formula could not be parsed. %s{err}"
        
    tsStringList, formula

let readAndParseSymbolicInstance systemInputPaths formulaInputPath =
    let systemList = 
        systemInputPaths 
        |> List.map (fun x -> 
                try 
                    File.ReadAllText  x
                with 
                    | _ -> 
                        raise <| FrontendException $"Could not open/read file %s{x}"
            )
        |> List.map (fun s -> 
            match NuSMV.Parser.parseProgram s with 
                | Result.Ok x -> x 
                | Result.Error msg -> 
                    raise <| FrontendException $"The symbolic system could not be parsed. %s{msg}"
            )

    let propContent = 
        try 
            File.ReadAllText formulaInputPath
        with 
        | _ -> raise <| FrontendException $"Could not open/read file %s{formulaInputPath}"

    let formula = 
        match NuSMV.Parser.parseSymbolicHyperQPTL propContent with
        | Result.Ok x -> x
        | Result.Error err -> 
            raise <| FrontendException $"The HyperQPTL formula could not be parsed: %s{err}"

    systemList, formula


let readAndParseBooleanProgramInstance systemInputPaths formulaInputPath =
    let programList = 
        systemInputPaths 
        |> List.map (fun x -> 
                try 
                    File.ReadAllText  x
                with 
                    | _ -> 
                        raise <| FrontendException $"Could not open/read file %s{x}"
            )
        |> List.map (fun s -> 
            match BooleanPrograms.Parser.parseBooleanProgram s with 
                | Result.Ok x -> x 
                | Result.Error msg -> 
                    raise <| FrontendException $"The boolean program could not be parsed. %s{msg}"
            )

    let propContent = 
        try 
            File.ReadAllText formulaInputPath
        with 
        | _ -> raise <| FrontendException $"Could not open/read file %s{formulaInputPath}"


    let formula = 
        match BooleanPrograms.Parser.parseBooleanProgramHyperQPTL propContent with
        | Result.Ok x -> x
        | Result.Error err -> 
            raise <| FrontendException $"The HyperQPTL formula could not be parsed: %s{err}"

    programList, formula

let readAndParseExplicitInstance systemInputPaths formulaInputPath =
    let explicitTsList = 
        systemInputPaths 
        |> List.map (fun x -> 
                try 
                    File.ReadAllText  x
                with 
                    | _ -> 
                        raise <| FrontendException $"Could not open/read file %s{x}"
            )
        |> List.map (fun s -> 
            match ExplictTransitionSystem.Parser.parseTS s with 
                | Result.Ok x -> x 
                | Result.Error msg -> 
                    raise <| FrontendException $"The NuSMV system could not be parsed. %s{msg}"
            )

    let propContent = 
        try 
            File.ReadAllText formulaInputPath
        with 
        | _ -> raise <| FrontendException $"Could not open/read file %s{formulaInputPath}"


    let formula = 
        match ExplictTransitionSystem.Parser.parseExplictSystemHyperQPTL propContent with
        | Result.Ok x -> x
        | Result.Error err -> 
            raise <| FrontendException $"The HyperQPTL formula could not be parsed: %s{err}"

    explicitTsList, formula