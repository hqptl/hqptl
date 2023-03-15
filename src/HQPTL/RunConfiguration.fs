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

module HQPTL.RunConfiguration 

open System
open System.IO

open Util
open JSON 

type SolverConfiguration = 
    {
        MainPath : String
        AutfiltPath: String
        Ltl2tgbaPath: String
    }

        
type VerbosityLevel = 
    | ZERO
    | ONE 
    | TWO 
    | THREE
    | FOUR 


type Configuration = 
    {
        SolverConfig : SolverConfiguration
        Logger : list<VerbosityLevel> -> String -> unit
    }

let private parseConfigFile (s : string) =
    match JSON.Parser.parseJsonString s with 
    | Result.Error err -> raise <| AnalysisException $"Could not parse config file: %s{err}"
    | Result.Ok x -> 
        {
            MainPath = "./"
            AutfiltPath =
                (JSON.tryLookup "autfilt" x)
                |> Option.bind (fun x -> JSON.tryGetString x)
                |> Option.defaultWith (fun _ -> raise <| AnalysisException "Must specify path to autfilt")
            Ltl2tgbaPath = 
                (JSON.tryLookup "ltl2tgba" x)
                |> Option.bind (fun x -> JSON.tryGetString x)
                |> Option.defaultWith (fun _ -> raise <| AnalysisException "Must specify path to ltl2tgba")
        }

let getConfig() = 
    // By convention the paths.json file is located in the same directory as the HyPA executable
    let configPath = 
        System.IO.Path.Join [|System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location); "paths.json"|]
                     
    // Check if the path to the config file is valid , i.e., the file exists
    if System.IO.FileInfo(configPath).Exists |> not then 
        raise <| AnalysisException "The paths.json file does not exist in the same directory as the executable"            
    
    // Parse the config File
    let configContent = 
        try
            File.ReadAllText configPath
        with 
            | _ -> 
                raise <| AnalysisException "Could not open paths.json file"

    let solverConfig = parseConfigFile configContent

    if System.IO.FileInfo(solverConfig.AutfiltPath).Exists |> not then 
        raise <| AnalysisException "The path to the spot's autfilt is incorrect"

    if System.IO.FileInfo(solverConfig.Ltl2tgbaPath).Exists |> not then 
        raise <| AnalysisException "The path to the spot's ltl2tgba is incorrect"
    
    solverConfig