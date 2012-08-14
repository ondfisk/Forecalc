module Program 

open System
open System.Collections.Generic
open System.Linq
open Forecalc.Library
open Forecalc.Library.Ast

//let worksheet = QT4.create<CellContent>()
//worksheet.[1, 0] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
//worksheet.[1, 1] <- Some({ Expr = Float 2.0 ; Value = FloatValue 2.0 ; Volatile = false })
//worksheet.[1, 2] <- Some({ Expr = Float 3.0 ; Value = FloatValue 3.0 ; Volatile = false })
//let workbook = Map.ofList [ "Sheet1", QT4.create<CellContent>() ; "Sheet2", worksheet ]
//let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
//let expr = Ref(Range({ Sheet = Some "Sheet2" ; TopLeft = { Sheet = Some "Sheet2" ; Row = 0 ; RowAbs = false ; Col = 1 ; ColAbs = false } ; BottomRight = { Sheet = Some "Sheet2" ; Row = 2 ; RowAbs = false ; Col = 1 ; ColAbs = false }}))
//eval cell expr workbook |> should equal (ValueList([ FloatValue 1.0 ; FloatValue 2.0 ; FloatValue 3.0 ]))

//let res = Eval.eval cell expr workbook

//printfn "%A" res

//let dirty = HashSet<AbsCell>()
//let computing = HashSet<AbsCell>()
//
//workbook |> Map.iter (fun k v -> v |> QT4.iteri (fun col row cell -> if cell.Volatile then dirty.Add { Sheet = k ; Row = row ; Col = col } |> ignore))




//let createCell (cell : AbsCell) (exprString : string) (workbook : Map<string, QT4.qt4<CellContent>>) = 
//    let expr = Parser.parse exprString |> ReferenceResolver.resolveRefs cell
//    let cellContent = 
//        match Eval.isVolatile expr with
//            | false -> { Expr = expr ; Value = Eval.eval cell expr workbook ; Volatile = false }
//            | true -> { Expr = expr ; Value = NullValue ; Volatile = true }
//    workbook.[cell.Sheet].[cell.Col, cell.Row] <- Some cellContent
//
//
//
//let recalculate (workbook : Map<string, QT4.qt4<CellContent>>) =
//    let computing = Set.empty<AbsCell>
//    0



    


(*

let rec eval (cell : AbsCell) (expr : Expr) (workbook : Map<string, QT4.qt4<CellContent>>) dirty computing =
    0  

while dirty.Count <> 0 do
    let cell = dirty.First()
    dirty.Remove(cell) |> ignore
    computing.Add(cell) |> ignore
    let content = workbook.[cell.Sheet].[cell.Col, cell.Row].Value
    let value = Eval.eval cell content.Expr workbook
    workbook.[cell.Sheet].[cell.Col, cell.Row] <- Some { content with Value = value }
    computing.Remove(cell) |> ignore
    
  *)  