module ParserCellReferencesTests

open Xunit
open FsUnit.Xunit
open Forecalc.Library.Ast
open Forecalc.Library.ExpressionParser

[<Fact>]
let ``"=A1" -> UnresolvedRef(A1Cell(A1))``() =
    "=A1" |> parse |> should equal (UnresolvedRef(A1Cell("A1")))

[<Fact>]
let ``"=a1" -> UnresolvedRef(A1Cell(A1))``() =
    "=a1" |> parse |> should equal (UnresolvedRef(A1Cell("A1")))

[<Fact>]
let ``"=$A1" -> UnresolvedRef(A1Cell($A1))``() =
    "=$A1" |> parse |> should equal (UnresolvedRef(A1Cell("$A1")))

[<Fact>]
let ``"=A$1" -> UnresolvedRef(A1Cell(A$1))``() =
    "=A$1" |> parse |> should equal (UnresolvedRef(A1Cell("A$1")))

[<Fact>]
let ``"=$A$1" -> UnresolvedRef(A1Cell($A$1))``() =
    "=$A$1" |> parse |> should equal (UnresolvedRef(A1Cell("$A$1")))

[<Fact>]
let ``"=A1:B2" -> UnresolvedRef(A1Range(A1, B2))``() =
    "=A1:B2" |> parse |> should equal (UnresolvedRef(A1Range("A1", "B2")))

[<Fact>]
let ``"=$A$1:$B$2" -> UnresolvedRef(A1Range($A$1, $B$2))``() =
    "=$A$1:$B$2" |> parse |> should equal (UnresolvedRef(A1Range("$A$1", "$B$2")))

[<Fact>]
let ``"=LOG10" -> UnresolvedRef(A1Cell(LOG10))``() =
    "=LOG10" |> parse |> should equal (UnresolvedRef(A1Cell("LOG10")))

[<Fact>]
let ``"=Sheet1!A1" -> UnresolvedRef(A1SheetRef(Sheet1, A1))``() =
    "=Sheet1!A1" |> parse |> should equal (UnresolvedRef(A1SheetRef("Sheet1", "A1")))

[<Fact>]
let ``"=Sheet1!A1:B2" -> UnresolvedRef(A1SheetRef(Sheet1, A1, B2))``() =
    "=Sheet1!A1:B2" |> parse |> should equal (UnresolvedRef(A1SheetRange("Sheet1", "A1", "B2")))

[<Fact>]
let ``"='..42..'!A1" -> UnresolvedRef(A1SheetRef(..42.., A1))``() =
    "='..42..'!A1" |> parse |> should equal (UnresolvedRef(A1SheetRef("..42..", "A1")))

[<Fact>]
let ``"='..42..'!A1:B2" -> UnresolvedRef(A1SheetRange(..42.., A1, B2))``() =
    "='..42..'!A1:B2" |> parse |> should equal (UnresolvedRef(A1SheetRange("..42..", "A1", "B2")))

[<Fact>]
let ``"=Sheet1!$A1" -> UnresolvedRef(A1SheetRef(Sheet1, $A1))``() =
    "=Sheet1!$A1" |> parse |> should equal (UnresolvedRef(A1SheetRef("Sheet1", "$A1")))

[<Fact>]
let ``"=Sheet1!$A1:B2" -> UnresolvedRef(A1SheetRef(Sheet1, $A1, B2))``() =
    "=Sheet1!$A1:B2" |> parse |> should equal (UnresolvedRef(A1SheetRange("Sheet1", "$A1", "B2")))


[<Fact>]
let ``"='..42..'!$A1" -> UnresolvedRef(A1SheetRef(..42.., $A1))``() =
    "='..42..'!$A1" |> parse |> should equal (UnresolvedRef(A1SheetRef("..42..", "$A1")))

[<Fact>]
let ``"='..42..'!$A1:B2" -> UnresolvedRef(A1SheetRange(..42.., $A1, B2)``() =
    "='..42..'!$A1:B2" |> parse |> should equal (UnresolvedRef(A1SheetRange("..42..", "$A1", "B2")))

[<Fact>]
let ``"=Sheet1!$A$1" -> UnresolvedRef(A1SheetRef(Sheet1, $A$1))``() =
    "=Sheet1!$A$1" |> parse |> should equal (UnresolvedRef(A1SheetRef("Sheet1", "$A$1")))

[<Fact>]
let ``"=Sheet1!$A1:$B2" -> UnresolvedRef(A1SheetRef(Sheet1, $A1, $B2))``() =
    "=Sheet1!$A1:$B2" |> parse |> should equal (UnresolvedRef(A1SheetRange("Sheet1", "$A1", "$B2")))

[<Fact>]
let ``"='..42..'!$A$1" -> UnresolvedRef(A1SheetRef(..42.., $A$1))``() =
    "='..42..'!$A$1" |> parse |> should equal (UnresolvedRef(A1SheetRef("..42..", "$A$1")))

[<Fact>]
let ``"='..42..'!$A1:$B$2" -> UnresolvedRef(A1SheetRange(..42.., $A1, $B$2))``() =
    "='..42..'!$A1:$B$2" |> parse |> should equal (UnresolvedRef(A1SheetRange("..42..", "$A1", "$B$2")))

[<Fact>]
let ``"=RC" -> UnresolvedRef(R1C1Cell(RC))``() =
    "=RC" |> parse |> should equal (UnresolvedRef(R1C1Cell("RC")))

[<Fact>]
let ``"=R1C1" -> UnresolvedRef(R1C1Cell(R1C1))``() =
    "=R1C1" |> parse |> should equal (UnresolvedRef(R1C1Cell("R1C1")))

[<Fact>]
let ``"=RC1" -> UnresolvedRef(R1C1Cell(RC1))``() =
    "=RC1" |> parse |> should equal (UnresolvedRef(R1C1Cell("RC1")))

[<Fact>]
let ``"=R1C" -> UnresolvedRef(R1C1Cell(R1C))``() =
    "=R1C" |> parse |> should equal (UnresolvedRef(R1C1Cell("R1C")))

[<Fact>]
let ``"=R[+1]C" -> UnresolvedRef(R1C1Cell(R[+1]C))``() =
    "=R[+1]C" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[+1]C")))

[<Fact>]
let ``"=R[+1]C1" -> UnresolvedRef(R1C1Cell(R[+1]C1))``() =
    "=R[+1]C1" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[+1]C1")))

[<Fact>]
let ``"=R[-1]C" -> UnresolvedRef(R1C1Cell(R[-1]C))``() =
    "=R[-1]C" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[-1]C")))

[<Fact>]
let ``"=RC[+1]" -> UnresolvedRef(R1C1Cell(RC[+1]))``() =
    "=RC[+1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("RC[+1]")))

[<Fact>]
let ``"=R1C[+1]" -> UnresolvedRef(R1C1Cell(R1C[+1]))``() =
    "=R1C[+1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("R1C[+1]")))

[<Fact>]
let ``"=RC[-1]" -> UnresolvedRef(R1C1Cell(RC[-1]))``() =
    "=RC[-1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("RC[-1]")))

[<Fact>]
let ``"=R1C[-1]" -> UnresolvedRef(R1C1Cell(R1C[-1]))``() =
    "=R1C[-1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("R1C[-1]")))

[<Fact>]
let ``"=R[+1]C[+1]" -> UnresolvedRef(R1C1Cell(R[+1]C[+1]))``() =
    "=R[+1]C[+1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[+1]C[+1]")))

[<Fact>]
let ``"=R[-1]C[-1]" -> UnresolvedRef(R1C1Cell(R[-1]C[-1]))``() =
    "=R[-1]C[-1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[-1]C[-1]")))

[<Fact>]
let ``"=r1c1" -> UnresolvedRef(R1C1Cell(R1C1))``() =
    "=r1c1" |> parse |> should equal (UnresolvedRef(R1C1Cell("R1C1")))

[<Fact>]
let ``"=r[-1]c[-1]" -> UnresolvedRef(R1C1Cell(R[-1]C[-1]))``() =
    "=r[-1]c[-1]" |> parse |> should equal (UnresolvedRef(R1C1Cell("R[-1]C[-1]")))
