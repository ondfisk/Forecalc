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
            | [ "" ; col ; "" ; row ] -> { Sheet = Option.None ; Row = int row - cell.Row ; RowAbs = false ; Col = columnFromAlpha col - cell.Col ; ColAbs = false }
            | [ "$" ; col ; "" ; row ] -> { Sheet = Option.None ; Row = int row - cell.Row ; RowAbs = false ; Col = columnFromAlpha col ; ColAbs = true }
            | [ "" ; col ; "$" ; row ] -> { Sheet = Option.None ; Row = int row ; RowAbs = true ; Col = columnFromAlpha col - cell.Col ; ColAbs = false }
            | [ "$" ; col ; "$" ; row ] -> { Sheet = Option.None ; Row = int row ; RowAbs = true ; Col = columnFromAlpha col ; ColAbs = true }
            | _ -> failwith "Invalid ref format"

    let resolveR1C1 ref =
        let parseInt = Int32.TryParse >> snd
        match groups regexR1C1 ref with
            | [ "[" ; row ; "]" ; "[" ; col ; "]" ] -> { Sheet = Option.None ; Row = parseInt row ; RowAbs = false ; Col = parseInt col ; ColAbs = false }
            | [ "" ; row ; "" ; "[" ; col ; "]" ] -> { Sheet = Option.None ; Row = parseInt row ; RowAbs = parseInt row <> 0 ; Col = parseInt col ; ColAbs = false }
            | [ "[" ; row ; "]" ; "" ; col ; "" ] -> { Sheet = Option.None ; Row = parseInt row ; RowAbs = false ; Col = parseInt col ; ColAbs = parseInt col <> 0 }
            | [ "" ; row ; "" ; "" ; col ; "" ] -> { Sheet = Option.None ; Row = parseInt row ; RowAbs = parseInt row <> 0 ; Col = parseInt col ; ColAbs = parseInt col <> 0 }
            | _ -> failwith "Invalid ref format"
        
    let resolveRef cell ref =
        match ref with
            | A1Cell(value) -> Cell(resolveA1 cell value)
            | A1Range(topLeft, bottomRight) -> Range({ Sheet = Option.None ; TopLeft = resolveA1 cell topLeft ; BottomRight = resolveA1 cell bottomRight })
            | A1SheetRef(sheet, value) -> Cell({ resolveA1 cell value with Sheet = Some sheet })
            | A1SheetRange(sheet, topLeft, bottomRight) -> Range({ Sheet = Some sheet ; TopLeft = { resolveA1 cell topLeft with Sheet = Some sheet } ; BottomRight = { resolveA1 cell bottomRight with Sheet = Some sheet } } )
            | R1C1Cell(value) -> Cell(resolveR1C1 value)
            | R1C1Range(topLeft, bottomRight) -> Range({ Sheet = Option.None ; TopLeft = resolveR1C1 topLeft ; BottomRight = resolveR1C1 bottomRight })
            | R1C1SheetRef(sheet, value) -> Cell({ resolveR1C1 value with Sheet = Some sheet })
            | R1C1SheetRange(sheet, topLeft, bottomRight) -> Range({ Sheet = Some sheet ; TopLeft = { resolveR1C1 topLeft with Sheet = Some sheet } ; BottomRight = { resolveR1C1 bottomRight with Sheet = Some sheet } })
            
    let rec resolveRefs cell expr =
        match expr with
            | Float(_) 
            | Boolean(_) 
            | String(_) 
            | EscapedString(_) 
            | Ref(_) 
            | Error(_) -> expr
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
