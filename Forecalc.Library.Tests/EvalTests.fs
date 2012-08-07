module EvalTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``Float(42.0) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Float(42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``Boolean(true) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Boolean(true)
    eval cell expr workbook |> should equal (BooleanValue true)