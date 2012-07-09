module Program 

open Ast
open Forecalc.Library.Parser
open System.Text.RegularExpressions

let cell = { Sheet = "Sheet1" ; Cell = { Row = 3 ; RowAbs = false ; Col = 3 ; ColAbs = false } }

let resolveA1z cell ref =
    let pattern = @"^(\$?)([A-Z]+)(\$?)(\d+)$"
    match (|Groups|_|) "A1" pattern with
        | Some ("" :: col :: "" :: row :: []) -> { Col = columnFromAlpha col - cell.Cell.Col ; ColAbs = false ; Row = int row - cell.Cell.Row ; RowAbs = false }
        | Some ("$" :: col :: "" :: row :: []) -> { Col = columnFromAlpha col ; ColAbs = true ; Row = int row - cell.Cell.Row ; RowAbs = false }
        | Some ("" :: col :: "$" :: row :: []) -> { Col = columnFromAlpha col - cell.Cell.Col ; ColAbs = false ; Row = int row ; RowAbs = true }
        | Some ("$" :: col :: "$" :: row :: []) -> { Col = columnFromAlpha col ; ColAbs = true ; Row = int row ; RowAbs = true }
        | _ -> { Col = System.Int32.MaxValue ; ColAbs = true ; Row = System.Int32.MaxValue ; RowAbs = true }

let res = resolveA1z cell "$A$1"

printfn "%A" res
