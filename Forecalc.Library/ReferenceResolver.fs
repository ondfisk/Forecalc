namespace Forecalc.Library

open System
open System.Text.RegularExpressions
open Ast

module ReferenceResolver =

    let columnFromAlpha (c : string) =
        let rec inner index acc list =
            match list with
                | [] -> acc
                | x::xs -> inner (index + 1) (acc + x + index * 25) xs
        c.ToUpper().ToCharArray() 
            |> Array.toList 
            |> List.map (fun c -> int c - 64)
            |> inner 0 0

    let (|Groups|_|) str regex =
        let m = Regex.Match(str, regex)
        if m.Success then
            Some (List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let resolveA1 cell ref =
        let p = @"^(\$?)([A-Z]+)(\$?)(\d+)$"
        match (|Groups|_|) ref p with
            | Some ("" :: col :: "" :: row :: []) -> { Row = int row - cell.Cell.Row ; RowAbs = false ; Col = columnFromAlpha col - cell.Cell.Col ; ColAbs = false }
            | Some ("$" :: col :: "" :: row :: []) -> { Row = int row - cell.Cell.Row ; RowAbs = false ; Col = columnFromAlpha col ; ColAbs = true }
            | Some ("" :: col :: "$" :: row :: []) -> { Row = int row ; RowAbs = true ; Col = columnFromAlpha col - cell.Cell.Col ; ColAbs = false }
            | Some ("$" :: col :: "$" :: row :: []) -> { Row = int row ; RowAbs = true ; Col = columnFromAlpha col ; ColAbs = true }
            | _ -> failwith "Invalid ref format"

    let resolveR1C1 ref =
        let getInt s = 
            let (_, i) = Int32.TryParse s
            i
        let p = @"^R(\[?)([\+|\-]?\d+)?(\]?)C(\[?)([\+|\-]?\d+)?(\])?$"
        match (|Groups|_|) ref p with
            | Some ("[" :: row :: "]" :: "[" :: col :: "]" :: []) -> { Row = getInt row ; RowAbs = false ; Col = getInt col ; ColAbs = false }
            | Some ("" :: row :: "" :: "[" :: col :: "]" :: []) -> { Row = getInt row ; RowAbs = row <> "" ; Col = getInt col ; ColAbs = false }
            | Some ("[" :: row :: "]" :: "" :: col :: "" :: []) -> { Row = getInt row ; RowAbs = false ; Col = getInt col ; ColAbs = col <> "" }
            | Some ("" :: row :: "" :: "" :: col :: "" :: []) -> { Row = getInt row ; RowAbs = row <> "" ; Col = getInt col ; ColAbs = col <> "" }
            | _ -> failwith "Invalid ref format"
        
    let resolveRef (cell : CellRef) (ref : UnresolvedRef) =
        match ref with
            | A1Cell(value) -> CellRef({ Sheet = cell.Sheet ; Cell = resolveA1 cell value })
            | A1Range(topLeft, bottomRight) -> RangeRef({ Sheet = cell.Sheet ; TopLeft = resolveA1 cell topLeft ; BottomRight = resolveA1 cell bottomRight })
            | A1SheetRef(sheet, value) -> CellRef({ Sheet = sheet ; Cell = resolveA1 cell value })
            | A1SheetRange(sheet, topLeft, bottomRight) -> RangeRef({ Sheet = sheet ; TopLeft = resolveA1 cell topLeft ; BottomRight = resolveA1 cell bottomRight })
            | R1C1Cell(value) -> CellRef({ Sheet = cell.Sheet ; Cell = resolveR1C1 value })
            | R1C1Range(topLeft, bottomRight) -> RangeRef({ Sheet = cell.Sheet ; TopLeft = resolveR1C1 topLeft ; BottomRight = resolveR1C1 bottomRight })
            | R1C1SheetRef(sheet, value) -> CellRef({ Sheet = sheet ; Cell = resolveR1C1 value })
            | R1C1SheetRange(sheet, topLeft, bottomRight) -> RangeRef({ Sheet = sheet ; TopLeft = resolveR1C1 topLeft ; BottomRight = resolveR1C1 bottomRight })
            
