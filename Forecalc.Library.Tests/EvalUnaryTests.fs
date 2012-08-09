module EvalUnaryTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``Negate(Float -42.0) -> FloatValue 42.0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Float -42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``Negate(Boolean false) -> FloatValue 0.0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Boolean false)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Negate(Boolean true) -> FloatValue -0.0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Boolean true)
    eval cell expr workbook |> should equal (FloatValue -1.0)

[<Test>]
let ``Negate(String "42") -> ErrorValue "#VALUE!"``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(String "42")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Negate(Error "#NAME?") -> ErrorValue "#NAME?"``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Error Name)
    eval cell expr workbook |> should equal (ErrorValue Name)

[<Test>]
let ``Negate(Null) -> FloatValue 0.0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Null)
    eval cell expr workbook |> should equal (FloatValue 0.0)
