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

module QPTLTrans.QPTL

open FsOmegaLib.LTL 

type QPTLQuantifier<'L> = 
    | QPTLForall of 'L
    | QPTLExists of 'L

type QPTL<'L when 'L: comparison> =
    {
        QuantifierPrefix : list<QPTLQuantifier<'L>>
        LTLMatrix : LTL<'L>
    }

module QPTL =
    ()

module Parser = 
    open FParsec
    
    let private qptlPrefixParser = 
        let existsPropParser = 
            skipString "E " >>. spaces >>. HQPTL.HyperQPTL.Parser.propVarParser .>> spaces .>> pchar '.'
            |>> fun p -> QPTLExists p

        let forallPropParser = 
            skipString "A " >>. spaces >>. HQPTL.HyperQPTL.Parser.propVarParser .>> spaces .>> pchar '.'
            |>> fun p -> QPTLForall p

        spaces >>.
        many (choice [existsPropParser; forallPropParser] .>> spaces)
        .>> spaces

    let private qptlParser = 
        pipe2 
            qptlPrefixParser
            (FsOmegaLib.LTL.Parser.ltlParser HQPTL.HyperQPTL.Parser.propVarParser)
            (fun x y -> {QPTL.QuantifierPrefix = x; LTLMatrix = y}) 

    let parseQPTL s =    
        let full = qptlParser .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err