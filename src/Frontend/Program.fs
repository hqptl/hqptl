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

module Frontend.Program

open System
open System.IO

open FsOmegaLib.GNBA
open FsOmegaLib.Conversion

open HQPTL.RunConfiguration
open HQPTL.HyperQPTL
open HQPTL.ModelChecking

open Util
open ExplictTransitionSystem
open CommandLineParser

let readAndParseHanoiInstance (config : SolverConfiguration) systemInputPaths formulaInputPath  = 
    let propcontent =   
        try 
            File.ReadAllText formulaInputPath
        with 
            | _ -> 
                raise <| FrontendException $"Could not open/read file %s{formulaInputPath}"
                
    let tscontent = 
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
                
    let tsList = 
        tscontent
        |> List.map (fun x -> 
            match FsOmegaLib.Conversion.AutomatonFromString.convertHoaStringToGNBA HQPTL.Util.DEBUG config.GetMainPath config.GetAutfiltPath Effort.LOW None x with 
            | Success x -> x 
            | Fail msg -> raise <| FrontendException $"Failure when obtaining GNBA for system: %s{msg}"
            | Timeout -> raise <| FrontendException $"Timeout"
        )

    tsList, formula

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


let private writeFormulaAndSystemString (systemOutputPaths: list<String>) formulaOutputPath (tsStringList : list<String>) (formulaString : String) =
    (systemOutputPaths, tsStringList)
    ||> List.zip
    |> List.iter (fun (file, tsString) -> 
        try 
            File.WriteAllText(file, tsString)
        with 
        | _ -> raise <| FrontendException $"Could not write to file %s{file}"
    )

    try 
        File.WriteAllText(formulaOutputPath, formulaString)
    with 
    | _ -> raise <| FrontendException $"Could not write to file %s{formulaOutputPath}"


[<EntryPoint>]
let main args =
    try 
        let swTotal = System.Diagnostics.Stopwatch()
        swTotal.Start()


        let cmdArgs =
            match CommandLineParser.parseCommandLineArguments (Array.toList args) with
                | Result.Ok x -> x
                | Result.Error e ->
                    raise <| FrontendException $"%s{e}"

        let config = HQPTL.RunConfiguration.getConfig()

        HQPTL.Util.DEBUGPrintouts <- cmdArgs.DebugPrintouts

        let systemInputPaths, formulaInputPath = 
            match cmdArgs.InputFiles with 
            | Some x -> x 
            | None -> 
                raise <| FrontendException "Must specify input files"

        let tsList, formula = 
            match cmdArgs.InputType with 
            | HanoiSystem -> 
                readAndParseHanoiInstance config systemInputPaths formulaInputPath 
            | SymbolicSystem -> 
                readAndParseSymbolicInstance systemInputPaths formulaInputPath 
                |> Translation.convertSymbolicSystemInstanceToGNBA
            | BooleanProgramSystem -> 
                readAndParseBooleanProgramInstance systemInputPaths formulaInputPath
                |> Translation.convertBooleanProgramInstanceToGNBA
            | ExplictSystem -> 
                readAndParseExplicitInstance systemInputPaths formulaInputPath
                |> Translation.convertExplictSystemInstanceToGNBA

        HQPTL.Util.LOGGERn "Read, parsed, and converted input"

        if cmdArgs.WriteExplicitSystems then 
            HQPTL.Util.LOGGER "Start writing explict-state system to disc..."
            let sw = System.Diagnostics.Stopwatch()
            sw.Start()

            // Write the converted explict state system to a file
            let systemOutputPaths, formulaOutputPath = 
                match cmdArgs.ExplicitInstanceOutputFiles with 
                | Some (a, b) -> 
                    if a.Length <> systemInputPaths.Length then 
                        raise <| FrontendException "The number of output files must match the input"
                    (a, b)
                | None -> 
                    systemInputPaths
                    |> List.map (fun x -> x + ".exp"), formulaInputPath + ".exp"

            let tsStringList = 
                tsList 
                |> List.map (GNBA.toHoaString string id)

            let formulaString = HyperQPTL.print id formula

            writeFormulaAndSystemString systemOutputPaths formulaOutputPath tsStringList formulaString

            HQPTL.Util.LOGGERn $"Done: %i{sw.ElapsedMilliseconds} ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0} s)"

        if cmdArgs.Verify then 
            try
                match HQPTL.ModelCheckingUtil.findErrorOnMcInstance tsList formula with 
                | None -> () 
                | Some msg -> 
                    raise <| FrontendException $"Error in model and/or formula: %s{msg}"

                let traceVarList =
                    HyperQPTL.quantifiedTraceVariables formula

                let tsMap =
                    if tsList.Length > 1 then 
                        (traceVarList, tsList)
                        ||> List.zip
                        |> Map.ofList
                    else
                        traceVarList
                        |> List.map (fun x -> x, tsList.[0])
                        |> Map.ofList

                let mcOptions = 
                    {
                        ModelCheckingOptions.Timeout = cmdArgs.Timeout
                        UseOwl = cmdArgs.UseOwl
                        ComputeWitnesses = cmdArgs.ComputeWitnesses
                        InitialSystemSimplification = cmdArgs.InitialSystemSimplification
                        IntermediateSimplification = cmdArgs.IntermediateSimplification
                    }
                
                let res, lasso = HQPTL.ModelChecking.modelCheck config mcOptions tsMap formula

                if res then 
                    printfn "SAT"
                else
                    printfn "UNSAT"


                if cmdArgs.ComputeWitnesses then 
                    match lasso with 
                    | None -> 
                        //printfn "Could not compute a Lasso"
                        ()
                    | Some lasso -> 
                        // We can assume that each DNF in this lasso is SAT

                        let printList (l : list<list<bool * (String * TraceVariable)>>) = 
                            l 
                            |> List.map (fun a -> 
                                a 
                                |> List.map (fun (b, (a, pi)) -> 
                                    let s = "\"" + a + "\"_" + pi
                                    if b then s else "!" + s
                                    )
                                |> Util.combineStringsWithSeperator " & "
                                |> fun s -> "(" + s + ")"
                                )
                            |> Util.combineStringsWithSeperator " "
                            |> fun x -> "[" + x + "]"

                        let prefixString = printList lasso.Prefix
                        let loopString = printList lasso.Loop

                        printfn $"%s{prefixString}"
                        printfn $"%s{loopString}"
            with 
            | HQPTL.Util.TimeoutException -> 
                printfn "TIMEOUT"

        HQPTL.Util.LOGGERn $"Total Time: %i{swTotal.ElapsedMilliseconds} ms (%.4f{double(swTotal.ElapsedMilliseconds) / 1000.0} s)"
     
        0
    with 
    | HQPTL.Util.AnalysisException err
    | FrontendException err when HQPTL.Util.DEBUG -> 
        printfn $"Error: %s{err}"
        reraise()
    | _  when HQPTL.Util.DEBUG -> reraise()
    | HQPTL.Util.AnalysisException err | FrontendException err -> 
        printfn $"Error: %s{err}"
        exit -1
    | e -> 
        printfn $"General Error: %s{e.Message}"
        exit -1
