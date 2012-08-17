module ParserCellReferencesTests

open NUnit.Framework
open FsUnit
open Forecalc.Library.Ast
open Forecalc.Library.Parser

[<Test>]
let ``"=A1" -> UnresolvedRef(A1Cell(A1))``() =
    "=A1" |> parse |> should equal (UnresolvedRef(A1Cell("A1")))

[<Test>]
let ``"=a1" -> UnresolvedRef(A1Cell(A1))``() =
    "=a1" |> parse |> should equal (UnresolvedRef(A1Cell("A1")))

[<Test>]
let ``"=$A1" -> UnresolvedRef(A1Cell($A1))``() =
    "=$A1" |> parse |> should equal (UnresolvedRef(A1Cell("$A1")))

[<Test>]
let ``"=A$1" -> UnresolvedRef(A1Cell(A$1))``() =
    "=A$1" |> parse |> should equal (UnresolvedRef(A1Cell("A$1")))

[<Test>]
let ``"=$A$1" -> UnresolvedRef(A1Cell($A$1))``() =
    "=$A$1" |> parse |> should equal (UnresolvedRef(A1Cell("$A$1")))

[<Test>]
let ``"=A1:B2" -> UnresolvedRef(A1Range(A1, B2))``() =
    "=A1:B2" |> parse |> should equal (UnresolvedRef(A1Range("A1", "B2")))
        
[<Test>]
let ``"=$A$1:$B$2" -> UnresolvedRef(A1Range($A$1, $B$2))``() =
    "=$A$1:$B$2" |> parse |> should equal (UnresolvedRef(A1Range("$A$1", "$B$2")))

[<Test>]
let ``"=LOG10" -> UnresolvedRef(A1Cell(LOG10))``() =
    "=LOG10" |> parse |> should equal (UnresolvedRef(A1Cell("LOG10")))

[<Test>]
let ``"=Sheet1!A1" -> UnresolvedRef(A1SheetRef(Sheet1, A1))``() =
    "=Sheet1!A1" |> parse |> should equal (UnresolvedRef(A1SheetRef("Sheet1", "A1")))
        
[<Test>]
let ``"=Sheet1!A1:B2" -> UnresolvedRef(A1SheetRef(Sheet1, A1, B2))``() =
    "=Sheet1!A1:B2" |> parse |> should equal (UnresolvedRef(A1SheetRange("Sheet1", "A1", "B2")))
        
[<Test>]
let ``"='..42..'!A1" -> UnresolvedRef(A1SheetRef(..42.., A1))``() =
    "='..42..'!A1" |> parse |> should equal (UnresolvedRef(A1SheetRef("..42..", "A1")))
        
[<Test>]
let ``"='..42..'!A1:B2" -> UnresolvedRef(A1SheetRange(..42.., A1, B2))``() =
    "='..42..'!A1:B2" |> parse |> should equal (UnresolvedRef(A1SheetRange("..42..", "A1", "B2")))

[<Test>]
let ``"=Sheet1!$A1" -> UnresolvedRef(A1SheetRef(Sheet1, $A1))``() =
    "=Sheet1!$A1" |> parse |> should equal (UnresolvedRef(A1SheetRef("Sheet1", "$A1")))
        
[<Test>]
let ``"=Sheet1!$A1:B2" -> UnresolvedRef(A1SheetRef(Sheet1, $A1, B2))``() =
    "=Sheet1!$A1:B2" |> parse |> should equal (UnresolvedRef(A1SheetRange("Sheet1", "$A1", "B2")))

        
[<Test>]
let ``"='..42..'!$A1" -> UnresolvedRef(A1SheetRef(..42.., $A1))``() =
    "='..42..'!$A1" |> parse |> should equal (UnresolvedRef(A1SheetRef("..42..", "$A1")))

[<Test>]
let ``"='..42..'!$A1:B2" -> UnresolvedRef(A1SheetRange(..42.., $A1, B2)``() =
    "='..42..'!$A1:B2" |> parse |> should equal (UnresolvedRef(A1SheetRange("..42..", "$A1", "B2")))

[<Test>]
let ``"=Sheet1!$A$1" -> UnresolvedRef(A1SheetRef(Sheet1, $A$1))``() =
    "=Sheet1!$A$1" |> parse |> should equal (UnresolvedRef(A1SheetRef("Sheet1", "$A$1")))
        
[<Test>]
let ``"=Sheet1!$A1:$B2" -> UnresolvedRef(A1SheetRef(Sheet1, $A1, $B2))``() =
    "=Sheet1!$A1:$B2" |> parse |> should equal (UnresolvedRef(A1SheetRange("Sheet1", "$A1", "$B2")))
        
[<Test>]
let ``"='..42..'!$A$1" -> UnresolvedRef(A1SheetRef(..42.., $A$1))``() =
    "='..42..'!$A$1" |> parse |> should equal (UnresolvedRef(A1SheetRef("..42..", "$A$1")))
        
[<Test>]
let ``"='..42..'!$A1:$B$2" -> UnresolvedRef(A1SheetRange(..42.., $A1, $B$2))``() =
    "='..42..'!$A1:$B$2" |> parse |> should equal (UnresolvedRef(A1SheetRange("..42..", "$A1", "$B$2")))

[<Test>]
let ``"=RC" -> UnresolvedRef(R1C1Cell(RC))``() =
    "=RC" |> parse |> should equal (UnresolvedRef(R1C1Cell("RC")))

[<Test>]
let ``"=R1C1" -> UnresolvedRef(R1C1Cell(R1C1))``() =
    "=R1C1" |> parse |> should equal (UnresolvedRef(R1C1Cell("R1C1")))

[<Test>]
let ``"=RC1" -> UnresolvedRef(R1C1Cell(RC1))``() =
    "=RC1" |> parse |> should equal (UnresolvedRef(R1C1Cell("RC1")))

[<Test>]
let ``"=R1C" -> UnresolvedRef(R1C1Cell(R1C))``() =
    "=R1C" |> parse |> should equal (UnresolvedRef(R1C1Cell("R1C")))

[<Test>]
let ``"=R[+1]C" -> UnresolvedRef(R1C1Cell(R[+1]C))``() =
    "=R[+1]C" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[+1]C")))

[<Test>]
let ``"=R[+1]C1" -> UnresolvedRef(R1C1Cell(R[+1]C1))``() =
    "=R[+1]C1" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[+1]C1")))

[<Test>]
let ``"=R[-1]C" -> UnresolvedRef(R1C1Cell(R[-1]C))``() =
    "=R[-1]C" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[-1]C")))

[<Test>]
let ``"=RC[+1]" -> UnresolvedRef(R1C1Cell(RC[+1]))``() =
    "=RC[+1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("RC[+1]")))

[<Test>]
let ``"=R1C[+1]" -> UnresolvedRef(R1C1Cell(R1C[+1]))``() =
    "=R1C[+1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("R1C[+1]")))
   
[<Test>]
let ``"=RC[-1]" -> UnresolvedRef(R1C1Cell(RC[-1]))``() =
    "=RC[-1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("RC[-1]")))

[<Test>]
let ``"=R1C[-1]" -> UnresolvedRef(R1C1Cell(R1C[-1]))``() =
    "=R1C[-1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("R1C[-1]")))

[<Test>]
let ``"=R[+1]C[+1]" -> UnresolvedRef(R1C1Cell(R[+1]C[+1]))``() =
    "=R[+1]C[+1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[+1]C[+1]")))

[<Test>]
let ``"=R[-1]C[-1]" -> UnresolvedRef(R1C1Cell(R[-1]C[-1]))``() =
    "=R[-1]C[-1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[-1]C[-1]")))

[<Test>]
let ``"=r1c1" -> UnresolvedRef(R1C1Cell(R1C1))``() =
    "=r1c1" |> parse |> should equal (UnresolvedRef(R1C1Cell("R1C1")))

[<Test>]
let ``"=r[-1]c[-1]" -> UnresolvedRef(R1C1Cell(R[-1]C[-1]))``() =
    "=r[-1]c[-1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[-1]C[-1]")))
