module EvalBinaryTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``4.0&2.0 -> "42"``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Float 4.0, Float 2.0)
    eval cell expr workbook |> should equal (StringValue "42")

[<Test>]
let ``Error("#VALUE!") & 42.0 -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Error "#VALUE!", Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

[<Test>]
let ``42.0 & Error("#VALUE!") -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Float 42.0, Error "#VALUE!")
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

[<Test>]
let ``"Life, " & "the " & "Universe " & "and " & "Everything = " & 42.0 -> "LifeT, the Universe and Everything = 42"``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Concat(Concat(Concat(Concat(String "Life, ",String "the "),String "Universe "), String "and "),String "Everything = "),Float 42.0)
    eval cell expr workbook |> should equal (StringValue "Life, the Universe and Everything = 42")

[<Test>]
let ``true&false -> "TRUEFALSE"``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Boolean true, Boolean false)
    eval cell expr workbook |> should equal (StringValue "TRUEFALSE")