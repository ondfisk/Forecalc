module EvalConstantsTests

open System.Collections.Generic
open Xunit
open FsUnit.Xunit
open Forecalc.Library
open Forecalc.Library.Eval

[<Fact>]
let ``Float(42.0) -> FloatValue(42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Float(42.0)
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 42.0)

[<Fact>]
let ``Boolean(true) -> BooleanValue(true)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Boolean(true)
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (BooleanValue true)

[<Fact>]
let ``String("42") -> StringValue("42")``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = String("42")
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (StringValue "42")

[<Fact>]
let ``EscapedString("42") -> StringValue("42")``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = EscapedString("42")
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (StringValue "42")

[<Fact>]
let ``Error(Reference) -> ErrorValue(Reference)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Error(Reference)
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Reference)

[<Fact>]
let ``UnresolvedRef(_) -> Fail``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = UnresolvedRef(A1Cell("A1"))
    (fun () -> eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> ignore) |> should throw typeof<System.Exception>

[<Fact>]
let ``Blank -> NullValue``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Blank
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal NullValue