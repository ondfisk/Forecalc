module EvalRefTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

//[<Test>]
//let ``Invalid reference -> Error("#REF!")``() =
//    let workbook = QT4.create<CellContent>()
//    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
//    let expr = Ref(Cell({ Sheet = None ; Row = 0 ; RowAbs = true ; Col = 0 ; ColAbs = true }))
//    eval cell expr workbook |> should equal (Error "#REF!")