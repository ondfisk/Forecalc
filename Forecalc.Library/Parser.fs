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

        { Col = col ; ColAbs = colAbs ; Row = row ; RowAbs = rowAbs }

    let resolveR1C1 (cell : CellRef) (ref : string) =
        let pattern = new Regex(@"^R(\[?)([\+|\-]?\d+)?(\]?)C(\[?)([\+|\-]?\d+)?(\])?$")
        let groups = pattern.Match(ref).Groups
        let rowAbs = groups.[1].Value = "" && groups.[2].Value <> "" && groups.[3].Value = ""
        let (_, row) = Int32.TryParse(groups.[2].Value)
        let colAbs = groups.[4].Value = "" && groups.[5].Value <> "" && groups.[6].Value = ""
        let (_, col) = Int32.TryParse(groups.[5].Value)
        
        { Col = col ; ColAbs = colAbs ; Row = row ; RowAbs = rowAbs }

    let resolveRef (cell : CellRef) (ref : UnresolvedRef) =
        match ref with
            | A1Cell(value) -> CellRef({ cell with Cell = resolveA1 cell value })
            | A1Range(topLeft, bottomRight) -> RangeRef({ Sheet = cell.Sheet ; TopLeft = resolveA1 cell topLeft ; BottomRight = resolveA1 cell bottomRight })
            | A1SheetRef(sheet, value) -> CellRef({ Sheet = sheet ; Cell = resolveA1 cell value })
            | A1SheetRange(sheet, topLeft, bottomRight) -> RangeRef({ Sheet = sheet ; TopLeft = resolveA1 cell topLeft ; BottomRight = resolveA1 cell bottomRight })
            | R1C1Cell(value) -> CellRef({ cell with Cell = resolveR1C1 cell value })
            | R1C1SheetRef(sheet, value) -> CellRef({ Sheet = sheet ; Cell = resolveR1C1 cell value })
            | _ -> failwith "i dunno yet"
            

            