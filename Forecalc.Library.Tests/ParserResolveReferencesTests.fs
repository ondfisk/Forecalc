namespace Forecalc.Library.Tests

open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library

[<TestFixture>] 
type ParserResolveReferencesTests () =
    // Cell = C3 / R3C3
    let cell = { Sheet = "Sheet1" ; Cell = { Row = 3 ; RowAbs = false ; Col = 3 ; ColAbs = false } }

    [<Test>]
    member this.``alphaToNumeric "A" -> 1``() =
        Parser.alphaToNumeric "A" |> should equal 1

    [<Test>]
    member this.``alphaToNumeric "B" -> 2``() =
        Parser.alphaToNumeric "B" |> should equal 2

    [<Test>]
    member this.``alphaToNumeric "Z" -> 26``() =
        Parser.alphaToNumeric "Z" |> should equal 26

    [<Test>]
    member this.``alphaToNumeric "AA" -> 27``() =
        Parser.alphaToNumeric "AA" |> should equal 27

    [<Test>]
    member this.``alphaToNumeric "AP" -> 42``() =
        Parser.alphaToNumeric "AP" |> should equal 42

    [<Test>]
    member this.``UnresolvedRef(A1) -> { Sheet = "Sheet1" ;  Cell = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false }}``() =
        A1Cell("A1") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } }))

    [<Test>]
    member this.``UnresolvedRef(E5) -> { Sheet = "Sheet1" ;  Cell = { Row = 2 ; RowAbs = false ; Col = 2 ; ColAbs = false }}``() =
        A1Cell("E5") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = false ; Col = 2 ; ColAbs = false } }))

    [<Test>]
    member this.``UnresolvedRef($A1) -> { Sheet = "Sheet1" ;  Cell = { Row = -2 ; RowAbs = false ; Col = 1 ; ColAbs = true }}``() =
        A1Cell("$A1") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = -2 ; RowAbs = false ; Col = 1 ; ColAbs = true } }))
        
    [<Test>]
    member this.``UnresolvedRef(A$1) -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = true ; Col = -2 ; ColAbs = false }}``() =
        A1Cell("A$1") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = -2 ; ColAbs = false } }))

    [<Test>]
    member this.``UnresolvedRef($A$1) -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }}``() =
        A1Cell("$A$1") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true } }))

    [<Test>]
    member this.``UnresolvedRef(A1:B2) -> { Sheet = "Sheet1" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }}``() =
        A1Range("A1", "B2") |> Parser.resolveRef cell |> should equal (RangeRef({ Sheet = "Sheet1" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
        
    [<Test>]
    member this.``UnresolvedRef(Sheet2!A1) -> { Sheet = "Sheet2" ;  Cell = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false }}``() =
        A1SheetRef("Sheet2", "A1") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet2" ; Cell = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } }))

    [<Test>]
    member this.``UnresolvedRef(Sheet2!A1:B2) -> { Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }}``() =
        A1SheetRange("Sheet2", "A1", "B2") |> Parser.resolveRef cell |> should equal (RangeRef({ Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))

    [<Test>]
    member this.``UnresolvedRef(R1C1) -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }}``() =
        R1C1Cell("R1C1") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true } }))

    [<Test>]
    member this.``UnresolvedRef(R[1]C1) -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = false ; Col = 1 ; ColAbs = true }}``() =
        R1C1Cell("R[1]C1") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = false ; Col = 1 ; ColAbs = true } }))
 
    [<Test>]
    member this.``UnresolvedRef(R1C[-1]) -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = true ; Col = -1 ; ColAbs = false }}``() =
        R1C1Cell("R1C[-1]") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = -1 ; ColAbs = false } }))
         
    [<Test>]
    member this.``UnresolvedRef(RC) -> { Sheet = "Sheet1" ;  Cell = { Row = 0 ; RowAbs = false ; Col = 0 ; ColAbs = false }}``() =
        R1C1Cell("RC") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 0 ; RowAbs = false ; Col = 0 ; ColAbs = false } }))

    [<Test>]
    member this.``UnresolvedRef(RC[-1]) -> { Sheet = "Sheet1" ;  Cell = { Row = 0 ; RowAbs = false ; Col = -1 ; ColAbs = false }}``() =
        R1C1Cell("RC[-1]") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 0 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
        
    [<Test>]
    member this.``UnresolvedRef(R[+1]C) -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = false ; Col = 0 ; ColAbs = false }}``() =
        R1C1Cell("R[+1]C") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = false ; Col = 0 ; ColAbs = false } }))

    [<Test>]
    member this.``UnresolvedRef(Sheet2!R1C1) -> { Sheet = "Sheet2" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }}``() =
        R1C1SheetRef("Sheet2", "R1C1") |> Parser.resolveRef cell |> should equal (CellRef({ Sheet = "Sheet2" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true } }))

