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

module HQPTL.Util 
#nowarn "59"

open System
open System.Collections.Generic

exception AnalysisException of String 
exception TimeoutException 


/// If set to true, we raise exceptions which is useful for debugging. If not set, all exceptions will be caught
let mutable DEBUG = false
let mutable DEBUGPrintouts = false
let mutable simplifySystem = true
let mutable simplifyIntermediateGnba = true

let LOGGER (s : String) = 
    if DEBUGPrintouts then 
        printf $"%s{s}"
    else 
        ()

let LOGGERn (s : String) = 
    if DEBUGPrintouts then 
        printfn $"%s{s}"
    else 
        ()

let rec cartesianProduct (LL: list<seq<'a>>) =
    match LL with
    | [] -> Seq.singleton []
    | L :: Ls ->
        seq {
            for x in L do
                for xs in cartesianProduct Ls -> x :: xs
        }

let rec combineStringsWithSeperator (s: String) (l: list<String>) = 
    match l with 
    | [] -> ""
    | [x] -> x
    | x::y::xs -> 
        x + s + combineStringsWithSeperator s (y::xs)

let dictToMap (d : Dictionary<'A, 'B>) = 
    d 
    |> Seq.map (fun x -> x.Key, x.Value)
    |> Map.ofSeq


module ParserUtil = 
    open FParsec

    let escapedStringParser : Parser<string, unit> = 
        skipChar '\"' >>. manyChars (satisfy (fun c -> c <> '\"')) .>> skipChar '\"'    
