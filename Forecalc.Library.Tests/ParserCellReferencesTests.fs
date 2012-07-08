namespace Forecalc.Library.Tests

open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library

[<TestFixture>] 
type ParserCellReferencesTests () =
    [<Test>]
    member this.``"=A1" -> UnresolvedRef(A1Cell(A1))``() =
        "=A1" |> Parser.parse |> should equal (UnresolvedRef(A1Cell("A1")))

    [<Test>]
    member this.``"=a1" -> UnresolvedRef(A1Cell(A1))``() =
        "=a1" |> Parser.parse |> should equal (UnresolvedRef(A1Cell("A1")))

    [<Test>]
    member this.``"=$A1" -> UnresolvedRef(A1Cell($A1))``() =
        "=$A1" |> Parser.parse |> should equal (UnresolvedRef(A1Cell("$A1")))

    [<Test>]
    member this.``"=A$1" -> UnresolvedRef(A1Cell(A$1))``() =
        "=A$1" |> Parser.parse |> should equal (UnresolvedRef(A1Cell("A$1")))

    [<Test>]
    member this.``"=$A$1" -> UnresolvedRef(A1Cell($A$1))``() =
        "=$A$1" |> Parser.parse |> should equal (UnresolvedRef(A1Cell("$A$1")))

    [<Test>]
    member this.``"=A1:B2" -> UnresolvedRef(A1Range(A1, B2))``() =
        "=A1:B2" |> Parser.parse |> should equal (UnresolvedRef(A1Range("A1", "B2")))
        
    [<Test>]
    member this.``"=$A$1:$B$2" -> UnresolvedRef(A1Range($A$1, $B$2))``() =
        "=$A$1:$B$2" |> Parser.parse |> should equal (UnresolvedRef(A1Range("$A$1", "$B$2")))

    [<Test>]
    member this.``"=LOG10" -> UnresolvedRef(A1Cell(LOG10))``() =
        "=LOG10" |> Parser.parse |> should equal (UnresolvedRef(A1Cell("LOG10")))

    [<Test>]
    member this.``"=Sheet1!A1" -> UnresolvedRef(A1SheetRef(Sheet1, A1))``() =
        "=Sheet1!A1" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRef("Sheet1", "A1")))
        
    [<Test>]
    member this.``"=Sheet1!A1:B2" -> UnresolvedRef(A1SheetRef(Sheet1, A1, B2))``() =
        "=Sheet1!A1:B2" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRange("Sheet1", "A1", "B2")))
        
    [<Test>]
    member this.``"='..42..'!A1" -> UnresolvedRef(A1SheetRef(..42.., A1))``() =
        "='..42..'!A1" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRef("..42..", "A1")))
        
    [<Test>]
    member this.``"='..42..'!A1:B2" -> UnresolvedRef(A1SheetRange(..42.., A1, B2))``() =
        "='..42..'!A1:B2" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRange("..42..", "A1", "B2")))

    [<Test>]
    member this.``"=Sheet1!$A1" -> UnresolvedRef(A1SheetRef(Sheet1, $A1))``() =
        "=Sheet1!$A1" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRef("Sheet1", "$A1")))
        
    [<Test>]
    member this.``"=Sheet1!$A1:B2" -> UnresolvedRef(A1SheetRef(Sheet1, $A1, B2))``() =
        "=Sheet1!$A1:B2" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRange("Sheet1", "$A1", "B2")))

        
    [<Test>]
    member this.``"='..42..'!$A1" -> UnresolvedRef(A1SheetRef(..42.., $A1))``() =
        "='..42..'!$A1" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRef("..42..", "$A1")))

    [<Test>]
    member this.``"='..42..'!$A1:B2" -> UnresolvedRef(A1SheetRange(..42.., $A1, B2)``() =
        "='..42..'!$A1:B2" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRange("..42..", "$A1", "B2")))

    [<Test>]
    member this.``"=Sheet1!$A$1" -> UnresolvedRef(A1SheetRef(Sheet1, $A$1))``() =
        "=Sheet1!$A$1" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRef("Sheet1", "$A$1")))
        
    [<Test>]
    member this.``"=Sheet1!$A1:$B2" -> UnresolvedRef(A1SheetRef(Sheet1, $A1, $B2))``() =
        "=Sheet1!$A1:$B2" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRange("Sheet1", "$A1", "$B2")))
        
    [<Test>]
    member this.``"='..42..'!$A$1" -> UnresolvedRef(A1SheetRef(..42.., $A$1))``() =
        "='..42..'!$A$1" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRef("..42..", "$A$1")))
        
    [<Test>]
    member this.``"='..42..'!$A1:$B$2" -> UnresolvedRef(A1SheetRange(..42.., $A1, $B$2))``() =
        "='..42..'!$A1:$B$2" |> Parser.parse |> should equal (UnresolvedRef(A1SheetRange("..42..", "$A1", "$B$2")))

    [<Test>]
    member this.``"=RC" -> UnresolvedRef(R1C1Cell(RC))``() =
        "=RC" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("RC")))

    [<Test>]
    member this.``"=R1C1" -> UnresolvedRef(R1C1Cell(R1C1))``() =
        "=R1C1" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("R1C1")))

    [<Test>]
    member this.``"=RC1" -> UnresolvedRef(R1C1Cell(RC1))``() =
        "=RC1" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("RC1")))

    [<Test>]
    member this.``"=R1C" -> UnresolvedRef(R1C1Cell(R1C))``() =
        "=R1C" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("R1C")))

    [<Test>]
    member this.``"=R[+1]C" -> UnresolvedRef(R1C1Cell(R[+1]C))``() =
        "=R[+1]C" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("R[+1]C")))

    [<Test>]
    member this.``"=R[+1]C1" -> UnresolvedRef(R1C1Cell(R[+1]C1))``() =
        "=R[+1]C1" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("R[+1]C1")))

    [<Test>]
    member this.``"=R[-1]C" -> UnresolvedRef(R1C1Cell(R[-1]C))``() =
        "=R[-1]C" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("R[-1]C")))

    [<Test>]
    member this.``"=RC[+1]" -> UnresolvedRef(R1C1Cell(RC[+1]))``() =
        "=RC[+1]" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("RC[+1]")))

    [<Test>]
    member this.``"=R1C[+1]" -> UnresolvedRef(R1C1Cell(R1C[+1]))``() =
        "=R1C[+1]" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("R1C[+1]")))
   
    [<Test>]
    member this.``"=RC[-1]" -> UnresolvedRef(R1C1Cell(RC[-1]))``() =
        "=RC[-1]" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("RC[-1]")))

    [<Test>]
    member this.``"=R1C[-1]" -> UnresolvedRef(R1C1Cell(R1C[-1]))``() =
        "=R1C[-1]" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("R1C[-1]")))

    [<Test>]
    member this.``"=R[+1]C[+1]" -> UnresolvedRef(R1C1Cell(R[+1]C[+1]))``() =
        "=R[+1]C[+1]" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("R[+1]C[+1]")))

    [<Test>]
    member this.``"=R[-1]C[-1]" -> UnresolvedRef(R1C1Cell(R[-1]C[-1]))``() =
        "=R[-1]C[-1]" |> Parser.parse |> should equal (UnresolvedRef(R1C1Cell("R[-1]C[-1]")))
