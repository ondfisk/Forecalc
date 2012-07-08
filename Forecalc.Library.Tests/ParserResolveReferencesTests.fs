namespace Forecalc.Library.Tests

open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library

[<TestFixture>] 
type ParserResolveReferencesTests () =
    let cell = { Sheet = "Sheet1" ; Cell = { Column = 1 ; ColumnAbsolute = false ; Row = 1 ; RowAbsolute = false } }

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


//    [<Test>]
//    member this.``UnresolvedRef(A1) -> CellRef(Sheet = "Sheet1", Cell(Column = 1, ColumnAbsolute = false, Row = 1, RowAbsolute = false))``() =
//        A1Cell("A1") |> Parser.resolveRef cell |> should equal { Sheet = "Sheet1" ; Cell = { Column = 1 ; ColumnAbsolute = false ; Row = 1 ; RowAbsolute = false } }