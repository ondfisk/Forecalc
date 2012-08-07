module EvalTests

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
let ``Error("#REF!") -> ErrorValue("#REF!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Error("#REF")
    eval cell expr workbook |> should equal (ErrorValue "#REF")

[<Test>]
let ``UnresolvedRef(_) -> Fail``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = UnresolvedRef(A1Cell("A1"))
    (fun () -> eval cell expr workbook |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``Negate(Float -42.0) -> FloatValue 42.0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Float -42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``Negate(Boolean false) -> FloatValue 0.0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Boolean false)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Negate(Boolean true) -> FloatValue -0.0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Boolean true)
    eval cell expr workbook |> should equal (FloatValue -1.0)

[<Test>]
let ``Negate(String "42") -> ErrorValue "#VALUE!"``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(String "42")
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

[<Test>]
let ``Negate(Error "#NAME?") -> ErrorValue "#NAME?"``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Error "#NAME?")
    eval cell expr workbook |> should equal (ErrorValue "#NAME?")

[<Test>]
let ``Eq(String "42", String "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(String "42", String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(Boolean false, String "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Boolean false, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Eq(Error("#REF!", String "42") -> ErrorValue("#REF!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Error "#REF!", String "42")
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``Eq(String "42", Error("#REF!") -> ErrorValue("#REF!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(String "42", Error "#REF!")
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``Eq(String "42", EscapedString("42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(String "42", EscapedString "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``NotEq(String "42", String "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(String "42", String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(Boolean false, String "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Boolean false, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``NotEq(Error("#REF!", String "42") -> ErrorValue("#REF!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Error "#REF!", String "42")
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``NotEq(String "42", Error("#REF!") -> ErrorValue("#REF!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(String "42", Error "#REF!")
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``NotEq(String "42", EscapedString("42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(String "42", EscapedString "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Lt(Float 0.0, Float 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Float 0.0, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not String<Float -> Lt(String "42", Float 42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(String "42", Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Float<String -> Lt(Float 42.0, String "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Float 42.0, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)
 
[<Test>]
let ``String<Bool -> Lt(String "42", Boolean true) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(String "42", Boolean true)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not Bool<String -> Lt(Boolean true, String "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Boolean true, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``false<true -> Lt(Boolean false, Boolean true) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Boolean false, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not true<false -> Lt(Boolean true, Boolean false) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Boolean true, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Lt(Error("#REF!", String "42") -> ErrorValue("#REF!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Error "#REF!", String "42")
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``Lt(String "42", Error("#REF!") -> ErrorValue("#REF!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(String "42", Error "#REF!")
    eval cell expr workbook |> should equal (ErrorValue "#REF!")