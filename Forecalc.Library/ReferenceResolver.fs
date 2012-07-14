namespace Forecalc.Library

open System
open System.Text.RegularExpressions
open Ast

module ReferenceResolver =

    let columnFromAlpha (c : string) =
        let rec inner acc list =
            match list with
                | [] -> acc + 1
                | x::xs -> inner ((acc + 1) * 26 + int x - 65) xs
        c.ToUpper().ToCharArray() 
            |> Array.toList 
            |> inner -1

    let (|Groups|_|) str regex =
        let m = Regex.Match(str, regex)
        match m.Success with
            | true -> Some (List.tail [ for g in m.Groups -> g.Value ])
            | false -> None

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
            
    let rec resolveRefs (cell : CellRef) (expr : Expr) =
        match expr with
            | Float(_) | Boolean(_) | String(_) | Ref(_) | Error(_) -> expr
            | Negate(e) -> Negate(resolveRefs cell e)
            | Eq(e1, e2) -> Eq(resolveRefs cell e1, resolveRefs cell e2)
            | NotEq(e1, e2) -> NotEq(resolveRefs cell e1, resolveRefs cell e2)
            | Lt(e1, e2) -> Lt(resolveRefs cell e1, resolveRefs cell e2)
            | Lte(e1, e2) -> Lte(resolveRefs cell e1, resolveRefs cell e2)
            | Gt(e1, e2) -> Gt(resolveRefs cell e1, resolveRefs cell e2)
            | Gte(e1, e2) -> Gte(resolveRefs cell e1, resolveRefs cell e2)
            | Concat(e1, e2) -> Concat(resolveRefs cell e1, resolveRefs cell e2)
            | Add(e1, e2) -> Add(resolveRefs cell e1, resolveRefs cell e2)
            | Sub(e1, e2) -> Sub(resolveRefs cell e1, resolveRefs cell e2)
            | Mul(e1, e2) -> Mul(resolveRefs cell e1, resolveRefs cell e2)
            | Div(e1, e2) -> Div(resolveRefs cell e1, resolveRefs cell e2)
            | Pow(e1, e2) -> Pow(resolveRefs cell e1, resolveRefs cell e2)
            | UnresolvedRef(ref) -> Ref(resolveRef cell ref)
            | Fun(name, list) -> Fun(name, List.map (resolveRefs cell) list)
