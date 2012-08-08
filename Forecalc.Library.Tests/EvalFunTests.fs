module EvalFunTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``Unknown function -> ErrorValue("#PARSE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUPER", [ String "" ; Null ; Null ])
    eval cell expr workbook |> should equal (ErrorValue "#PARSE!")

[<Test>]
let ``IF(non-boolean;_;_) -> ErrorValue("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ String "" ; Null ; Null ])
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

[<Test>]
let ``IF() -> ErrorValue("#PARSE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [])
    eval cell expr workbook |> should equal (ErrorValue "#PARSE!")

[<Test>]
let ``IF(true, 42.0, null) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Boolean true ; Float 42.0 ; Null ])
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``IF(false, null, 42.0) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Boolean false ; Null ; Float 42.0 ])
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``IF(null, null, 42.0) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Null ; Null ; Float 42.0 ])
    eval cell expr workbook |> should equal (FloatValue 42.0)