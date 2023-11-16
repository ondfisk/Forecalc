module EvalUnaryTests

open System.Collections.Generic
open Xunit
open FsUnit.Xunit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Fact>]
let ``Negate(Float -42.0) -> FloatValue 42.0``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Float -42.0)
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 42.0)

[<Fact>]
let ``Negate(Boolean false) -> FloatValue 0.0``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Boolean false)
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

[<Fact>]
let ``Negate(Boolean true) -> FloatValue -0.0``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Boolean true)
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue -1.0)

[<Fact>]
let ``Negate(String "42") -> ErrorValue "#VALUE!"``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(String "42")
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Value)

[<Fact>]
let ``Negate(Error "#NAME?") -> ErrorValue "#NAME?"``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Error Name)
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Name)

[<Fact>]
let ``Negate(Blank) -> FloatValue 0.0``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Blank)
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)
