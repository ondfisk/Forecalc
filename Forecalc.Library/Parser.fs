namespace Forecalc.Library

open System
open System.Globalization
open System.Text.RegularExpressions
open System.Threading
open Microsoft.FSharp.Text.Lexing
open Ast
open LexerSpecification
open ParserSpecification

module Parser =

    Thread.CurrentThread.CurrentCulture <- CultureInfo("en-US")
    Thread.CurrentThread.CurrentUICulture <- CultureInfo("en-US")

    let isFloat s =
        let b, _ = Double.TryParse(s)
        b

    let isBoolean s =
        let b, _ = bool.TryParse(s)
        b

    let toString (s : string) =
        match s with
            | _ when s.StartsWith("'") -> s.Substring 1
            | _ -> s
            
    let isFormula (s : string) =
        s.StartsWith("=")

    let parse (expr : string) =
        match expr with
            | _ when isFormula expr ->
                    let e = expr.Substring 1
                    try
                        let lexbuff = LexBuffer<char>.FromString(e)
                        let expression = ParserSpecification.start LexerSpecification.tokenize lexbuff
                        expression
                    with
                        | ex -> Error ex.Message
            | _ when isFloat expr -> Float (float expr)
            | _ when isBoolean expr -> Boolean (bool.Parse expr)
            | _ -> String (toString expr)

    let alphaToNumeric (a : string) =
        let array = a.ToUpper().ToCharArray() |> Array.map (fun c -> int c - 64) 
        for c in [ 0 .. array.Length - 1 ] do
            array.[c] <- array.[c] + c * 25
        Array.sum array

    let resolveA1 (cell : CellRef) (ref : string) =
        let pattern = new Regex(@"^(\$?)([A-Z]+)(\$?)(\d+)$")
        let groups = pattern.Match(ref).Groups
        let colAbs = groups.[1].Value = "$"
        let colValue = alphaToNumeric groups.[2].Value
        let rowAbs = groups.[3].Value = "$"
        let rowValue = int groups.[4].Value
        
        let col =
            match colAbs with
            | true -> colValue
            | false -> colValue - cell.Cell.Col

        let row =
            match rowAbs with
            | true -> rowValue
            | false -> rowValue - cell.Cell.Row

        { cell with Cell = { Col = col ; ColAbs = colAbs ; Row = row ; RowAbs = rowAbs } }

    let resolveRef (cell : CellRef) (ref : UnresolvedRef) =
        match ref with
            | A1Cell(value) -> resolveA1 cell value
            | _ -> failwith "i dunno yet"
            

            