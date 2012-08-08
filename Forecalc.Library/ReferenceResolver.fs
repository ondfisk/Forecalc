namespace Forecalc.Library

open System
open System.Text.RegularExpressions
open Forecalc.Library.Ast

module ReferenceResolver =

    let regexA1 = Regex(@"^(\$?)([A-Z]+)(\$?)(\d+)$")
    let regexR1C1 = Regex(@"^R(\[?)([\+|\-]?\d+)?(\]?)C(\[?)([\+|\-]?\d+)?(\])?$")

    let columnFromAlpha (c : string) =
        c.ToUpper().ToCharArray()
            |> Array.toList
            |> List.rev
            |> List.mapi (fun i c -> (int c - 64) * int (26.0 ** float i))
            |> List.sum

    let groups (regex : Regex) ref =
        let m = regex.Match(ref)
        match m.Success with
            | true -> List.tail [ for g in m.Groups -> g.Value ]
            | false -> []

    let resolveA1 cell ref =
        match groups regexA1 ref with
            | [ "" ; col ; "" ; row ] -> { Sheet = None ; Row = int row - cell.Row ; RowAbs = false ; Col = columnFromAlpha col - cell.Col ; ColAbs = false }
            | [ "$" ; col ; "" ; row ] -> { Sheet = None ; Row = int row - cell.Row ; RowAbs = false ; Col = columnFromAlpha col ; ColAbs = true }
            | [ "" ; col ; "$" ; row ] -> { Sheet = None ; Row = int row ; RowAbs = true ; Col = columnFromAlpha col - cell.Col ; ColAbs = false }
            | [ "$" ; col ; "$" ; row ] -> { Sheet = None ; Row = int row ; RowAbs = true ; Col = columnFromAlpha col ; ColAbs = true }
            | _ -> failwith "Invalid ref format"

    let resolveR1C1 ref =
        let parseInt = Int32.TryParse >> snd
        match groups regexR1C1 ref with
            | [ "[" ; row ; "]" ; "[" ; col ; "]" ] -> { Sheet = None ; Row = parseInt row ; RowAbs = false ; Col = parseInt col ; ColAbs = false }
            | [ "" ; row ; "" ; "[" ; col ; "]" ] -> { Sheet = None ; Row = parseInt row ; RowAbs = parseInt row <> 0 ; Col = parseInt col ; ColAbs = false }
            | [ "[" ; row ; "]" ; "" ; col ; "" ] -> { Sheet = None ; Row = parseInt row ; RowAbs = false ; Col = parseInt col ; ColAbs = parseInt col <> 0 }
            | [ "" ; row ; "" ; "" ; col ; "" ] -> { Sheet = None ; Row = parseInt row ; RowAbs = parseInt row <> 0 ; Col = parseInt col ; ColAbs = parseInt col <> 0 }
            | _ -> failwith "Invalid ref format"
        
    let flipRangeWhenRequired (cell : AbsCell) ref =
        match ref with
            | Cell(_) -> ref
            | Range(range) -> 
                let (r1, c1) = (
                    match range.TopLeft.RowAbs with
                        | true -> range.TopLeft.Row
                        | false -> cell.Row + range.TopLeft.Row
                    ,
                    match range.TopLeft.ColAbs with
                        | true -> range.TopLeft.Col
                        | false -> cell.Col + range.TopLeft.Col
                    )
                let (r2, c2) = (
                    match range.BottomRight.RowAbs with
                        | true -> range.BottomRight.Row
                        | false -> cell.Row + range.BottomRight.Row             
                    ,
                    match range.BottomRight.ColAbs with
                        | true -> range.BottomRight.Col
                        | false -> cell.Col + range.BottomRight.Col
                    )
                match min r1 r2 = r2, min c1 c2 = c2 with
                    | true, true -> ref
                    | true, false -> ref
                    | false, true -> ref
                    | false, false -> ref
                

    let resolveRef cell ref =
        match ref with
            | A1Cell(value) -> Cell(resolveA1 cell value)
            | A1Range(topLeft, bottomRight) -> Range({ Sheet = None ; TopLeft = resolveA1 cell topLeft ; BottomRight = resolveA1 cell bottomRight }) |> flipRangeWhenRequired cell
            | A1SheetRef(sheet, value) -> Cell({ resolveA1 cell value with Sheet = Some sheet })
            | A1SheetRange(sheet, topLeft, bottomRight) -> Range({ Sheet = Some sheet ; TopLeft = { resolveA1 cell topLeft with Sheet = Some sheet } ; BottomRight = { resolveA1 cell bottomRight with Sheet = Some sheet } } ) |> flipRangeWhenRequired cell
            | R1C1Cell(value) -> Cell(resolveR1C1 value)
            | R1C1Range(topLeft, bottomRight) -> Range({ Sheet = None ; TopLeft = resolveR1C1 topLeft ; BottomRight = resolveR1C1 bottomRight }) |> flipRangeWhenRequired cell
            | R1C1SheetRef(sheet, value) -> Cell({ resolveR1C1 value with Sheet = Some sheet })
            | R1C1SheetRange(sheet, topLeft, bottomRight) -> Range({ Sheet = Some sheet ; TopLeft = { resolveR1C1 topLeft with Sheet = Some sheet } ; BottomRight = { resolveR1C1 bottomRight with Sheet = Some sheet } }) |> flipRangeWhenRequired cell
            
    let rec resolveRefs cell expr =
        match expr with
            | Float(_) 
            | Boolean(_) 
            | String(_) 
            | EscapedString(_) 
            | Ref(_) 
            | Error(_)
            | Null -> expr
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
