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

module QPTLTrans.Program

open FsOmegaLib.GNBA
open FsOmegaLib.NBA

open HQPTL.Util
open HQPTL.RunConfiguration

open CommandLineParser

let private run (args: array<string>) =
    // Parse the command line args
    let cmdArgs =
        match CommandLineParser.parseCommandLineArguments (Array.toList args) with
            | Result.Ok x -> x
            | Result.Error e ->
                   raise <| AnalysisException $"%s{e}"
                   
    let config = HQPTL.RunConfiguration.getConfig()

    match cmdArgs.ExecMode with 
        | None -> 
            raise <| AnalysisException "Must specify a mode"
        | Some e ->
            match e with
            | ConvertQPTLToGNBA path ->
                try 
                    let formula = QPTLTranslationUtil.readAndParseQPTL path
                    let gnba = QPTLTranslationUtil.convertQPTLToGNBA config formula cmdArgs.UseOwl cmdArgs.Timeout 
                    let s = GNBA.toHoaString string string gnba

                    match cmdArgs.Output with 
                    | None -> 
                        printfn $"%s{s}"
                    | Some file -> 
                        try 
                            System.IO.File.WriteAllText(file, s)
                        with _ -> raise <| AnalysisException "Could not write to output file"                        
                with 
                | TimeoutException -> 
                    printfn "TIMEOUT"
                    
            | ConvertQPTLToNBA path ->
                try 
                    let formula = QPTLTranslationUtil.readAndParseQPTL path
                    let nba = QPTLTranslationUtil.convertQPTLToNBA config formula cmdArgs.UseOwl cmdArgs.Timeout 
                    let s = NBA.toHoaString string string nba

                    match cmdArgs.Output with 
                    | None -> 
                        printfn $"%s{s}"
                    | Some file -> 
                        try 
                            System.IO.File.WriteAllText(file, s)
                        with _ -> raise <| AnalysisException "Could not write to output file"   
                with 
                | TimeoutException -> 
                    printfn "TIMEOUT"
                    
    0

[<EntryPoint>]
let main args =
    try 
        run args
    with 
        | AnalysisException err when HQPTL.Util.DEBUG -> 
            printfn $"Error: %s{err}"
            reraise()
        | _  when HQPTL.Util.DEBUG -> reraise()
        | AnalysisException err -> 
            printfn $"Error: %s{err}"
            exit -1
        | e -> 
            printfn $"General Error: %s{e.Message}"
            exit -1