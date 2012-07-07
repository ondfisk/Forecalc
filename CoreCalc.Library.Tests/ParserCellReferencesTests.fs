namespace CoreCalc.Library.Tests

open NUnit.Framework
open FsUnit
open Ast
open CoreCalc.Library

[<TestFixture>] 
type ParserCellReferencesTests () =
    [<Test>]
    member this.``"=A1" -> Cell(A1)``() =
        "=A1" |> Parser.parse |> should equal (Cell("A1"))

    [<Test>]
    member this.``"=a1" -> Cell(A1)``() =
        "=a1" |> Parser.parse |> should equal (Cell("A1"))

    [<Test>]
    member this.``"=$A1" -> Cell($A1)``() =
        "=$A1" |> Parser.parse |> should equal (Cell("$A1"))

    [<Test>]
    member this.``"=A$1" -> Cell(A$1)``() =
        "=A$1" |> Parser.parse |> should equal (Cell("A$1"))

    [<Test>]
    member this.``"=$A$1" -> Cell($A$1)``() =
        "=$A$1" |> Parser.parse |> should equal (Cell("$A$1"))

    [<Test>]
    member this.``"=A1:B2" -> Range(A1, B2)``() =
        "=A1:B2" |> Parser.parse |> should equal (Range("A1", "B2"))
        
    [<Test>]
    member this.``"=$A$1:$B$2" -> Range($A$1, $B$2)``() =
        "=$A$1:$B$2" |> Parser.parse |> should equal (Range("$A$1", "$B$2"))

    [<Test>]
    member this.``"=LOG10" -> Cell(LOG10)``() =
        "=LOG10" |> Parser.parse |> should equal (Cell("LOG10"))

    [<Test>]
    member this.``"=Sheet1!A1" -> SheetRef(Sheet1, A1)``() =
        "=Sheet1!A1" |> Parser.parse |> should equal (SheetRef("Sheet1", "A1"))
        
    [<Test>]
    member this.``"=Sheet1!A1:B2" -> SheetRef(Sheet1, A1, B2)``() =
        "=Sheet1!A1:B2" |> Parser.parse |> should equal (SheetRange("Sheet1", "A1", "B2"))
        
    [<Test>]
    member this.``"='..42..'!A1" -> SheetRef(..42.., A1)``() =
        "='..42..'!A1" |> Parser.parse |> should equal (SheetRef("..42..", "A1"))
        
    [<Test>]
    member this.``"='..42..'!A1:B2" -> SheetRange(..42.., A1, B2)``() =
        "='..42..'!A1:B2" |> Parser.parse |> should equal (SheetRange("..42..", "A1", "B2"))

    [<Test>]
    member this.``"=Sheet1!$A1" -> SheetRef(Sheet1, $A1)``() =
        "=Sheet1!$A1" |> Parser.parse |> should equal (SheetRef("Sheet1", "$A1"))
        
    [<Test>]
    member this.``"=Sheet1!$A1:B2" -> SheetRef(Sheet1, $A1, B2)``() =
        "=Sheet1!$A1:B2" |> Parser.parse |> should equal (SheetRange("Sheet1", "$A1", "B2"))

        
    [<Test>]
    member this.``"='..42..'!$A1" -> SheetRef(..42.., $A1)``() =
        "='..42..'!$A1" |> Parser.parse |> should equal (SheetRef("..42..", "$A1"))

    [<Test>]
    member this.``"='..42..'!$A1:B2" -> SheetRange(..42.., $A1, B2)``() =
        "='..42..'!$A1:B2" |> Parser.parse |> should equal (SheetRange("..42..", "$A1", "B2"))

    [<Test>]
    member this.``"=Sheet1!$A$1" -> SheetRef(Sheet1, $A$1)``() =
        "=Sheet1!$A$1" |> Parser.parse |> should equal (SheetRef("Sheet1", "$A$1"))
        
    [<Test>]
    member this.``"=Sheet1!$A1:$B2" -> SheetRef(Sheet1, $A1, $B2)``() =
        "=Sheet1!$A1:$B2" |> Parser.parse |> should equal (SheetRange("Sheet1", "$A1", "$B2"))
        
    [<Test>]
    member this.``"='..42..'!$A$1" -> SheetRef(..42.., $A$1)``() =
        "='..42..'!$A$1" |> Parser.parse |> should equal (SheetRef("..42..", "$A$1"))
        
    [<Test>]
    member this.``"='..42..'!$A1:$B$2" -> SheetRange(..42.., $A1, $B$2)``() =
        "='..42..'!$A1:$B$2" |> Parser.parse |> should equal (SheetRange("..42..", "$A1", "$B$2"))

    [<Test>]
    member this.``"=RC" -> Cell(RC)``() =
        "=RC" |> Parser.parse |> should equal (Cell("RC"))

    [<Test>]
    member this.``"=R1C1" -> Cell(R1C1)``() =
        "=R1C1" |> Parser.parse |> should equal (Cell("R1C1"))

    [<Test>]
    member this.``"=RC1" -> Cell(RC1)``() =
        "=RC1" |> Parser.parse |> should equal (Cell("RC1"))

    [<Test>]
    member this.``"=R1C" -> Cell(R1C)``() =
        "=R1C" |> Parser.parse |> should equal (Cell("R1C"))

    [<Test>]
    member this.``"=R[+1]C" -> Cell(R[+1]C)``() =
        "=R[+1]C" |> Parser.parse |> should equal (Cell("R[+1]C"))

    [<Test>]
    member this.``"=R[+1]C1" -> Cell(R[+1]C1)``() =
        "=R[+1]C1" |> Parser.parse |> should equal (Cell("R[+1]C1"))

    [<Test>]
    member this.``"=R[-1]C" -> Cell(R[-1]C)``() =
        "=R[-1]C" |> Parser.parse |> should equal (Cell("R[-1]C"))

    [<Test>]
    member this.``"=RC[+1]" -> Cell(RC[+1])``() =
        "=RC[+1]" |> Parser.parse |> should equal (Cell("RC[+1]"))

    [<Test>]
    member this.``"=R1C[+1]" -> Cell(R1C[+1])``() =
        "=R1C[+1]" |> Parser.parse |> should equal (Cell("R1C[+1]"))
   
    [<Test>]
    member this.``"=RC[-1]" -> Cell(RC[-1])``() =
        "=RC[-1]" |> Parser.parse |> should equal (Cell("RC[-1]"))

    [<Test>]
    member this.``"=R1C[-1]" -> Cell(R1C[-1])``() =
        "=R1C[-1]" |> Parser.parse |> should equal (Cell("R1C[-1]"))

    [<Test>]
    member this.``"=R[+1]C[+1]" -> Cell(R[+1]C[+1])``() =
        "=R[+1]C[+1]" |> Parser.parse |> should equal (Cell("R[+1]C[+1]"))

    [<Test>]
    member this.``"=R[-1]C[-1]" -> Cell(R[-1]C[-1])``() =
        "=R[-1]C[-1]" |> Parser.parse |> should equal (Cell("R[-1]C[-1]"))
