module EvalConstantsTests

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

[<Test>]
let ``String("42") -> StringValue("42")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = String("42")
    eval cell expr workbook |> should equal (StringValue "42")
   
[<Test>]
let ``EscapedString("42") -> StringValue("42")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = EscapedString("42")
    eval cell expr workbook |> should equal (StringValue "42")

[<Test>]
let ``Error(Reference) -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Error(Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``UnresolvedRef(_) -> Fail``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = UnresolvedRef(A1Cell("A1"))
    (fun () -> eval cell expr workbook |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``Blank -> NullValue``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Blank
    eval cell expr workbook |> should equal NullValue