module EvalBinaryTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``4.0&2.0 -> "42"``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Float 4.0, Float 2.0)
    eval cell expr workbook |> should equal (StringValue "42")

[<Test>]
let ``Error(Value) & 42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Error Value, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``42.0 & Error(Value) -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Float 42.0, Error Value)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``42.0&Null -> String("42")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Float 42.0, Null)
    eval cell expr workbook |> should equal (StringValue "42")

[<Test>]
let ``Null&42.0 -> String("42")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Null, Float 42.0)
    eval cell expr workbook |> should equal (StringValue "42")

[<Test>]
let ``"Life, " & "the " & "Universe " & "and " & "Everything = " & 42.0 -> "LifeT, the Universe and Everything = 42"``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Concat(Concat(Concat(Concat(String "Life, ",String "the "),String "Universe "), String "and "),String "Everything = "),Float 42.0)
    eval cell expr workbook |> should equal (StringValue "Life, the Universe and Everything = 42")

[<Test>]
let ``true&false -> "TRUEFALSE"``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Boolean true, Boolean false)
    eval cell expr workbook |> should equal (StringValue "TRUEFALSE")

[<Test>]
let ``20+22 -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 20.0, Float 22.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``"42"+0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``0+"42" -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``true+41.0 -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Boolean true, Float 41.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``41.0+true -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 41.0, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``false+42.0 -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Boolean false, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``42.0+false -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 42.0, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``#REF! + 42 -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Error Reference, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``42 + #REF! -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 42.0, Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``43-1 -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 43.0, Float 1.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``"42"-0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``0-"42" -> Error(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``true--41.0 -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Boolean true, Float -41.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``43.0-true -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 43.0, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``false--42.0 -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Boolean false, Float -42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``42.0-false -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 42.0, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``#REF! - 42 -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Error Reference, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``42 - #REF! -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 42.0, Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``6*7 -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 6.0, Float 7.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``"42"*0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``0*"42" -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``true*42.0 -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Boolean true, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``42.0*true -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 42.0, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``false*42.0 -> 0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Boolean false, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``42.0*false -> 0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 42.0, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``#REF! * 42 -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Error Reference, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``42 * #REF! -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 42.0, Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``126.0/3.0 -> 42.0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 126.0, Float 3.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``"42"/0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``0/"42" -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``true/0.5 -> 2.0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Boolean true, Float 0.5)
    eval cell expr workbook |> should equal (FloatValue 2.0)

[<Test>]
let ``42.0/true -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 42.0, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``false/42.0 -> 0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Boolean false, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``42.0/false -> #DIV/0!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 42.0, Boolean false)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``42.0/0.0 -> #DIV/0!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 42.0, Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``#REF! / 42 -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Error Reference, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``42 / #REF! -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 42.0, Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``2^8 -> 256``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 2.0, Float 8.0)
    eval cell expr workbook |> should equal (FloatValue 256.0)

[<Test>]
let ``"42"^0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``0^"42" -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``true^42.0 -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Boolean true, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 1.0)

[<Test>]
let ``42.0^true -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 42.0, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``false^42.0 -> 0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Boolean false, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``42.0^false -> 1``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 42.0, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 1.0)

[<Test>]
let ``#REF!^42 -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Error Reference, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``42^#REF! -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 42.0, Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Null+Null -> Add(Null, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Null, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Null+"" -> Add(Null, "") -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Null, String "")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``""+Null -> Add("", Null) -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(String "", Null)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Null+"42" -> Add(Null, "42") -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Null, String "42")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``"42"+Null -> Add("42", Null) -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(String "42", Null)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Null+0.0 -> Add(Null, 0.0) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Null, Float 0.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``0.0+Null -> Add(0.0, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 0.0, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Null+42.0 -> Add(Null, -42.0) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Null, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``42.0+Null -> Add(42.0, Null) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 42.0, Null)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``Null+-42.0 -> Add(Null, -42.0) -> FloatValue(-42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Null, Float -42.0)
    eval cell expr workbook |> should equal (FloatValue -42.0)

[<Test>]
let ``-42.0+Null -> Add(-42.0, Null) -> FloatValue(-42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float -42.0, Null)
    eval cell expr workbook |> should equal (FloatValue -42.0) 

[<Test>]
let ``Null+true -> Add(Null, true) -> FloatValue(1.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Null, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 1.0)

[<Test>]
let ``true+Null -> Add(true, Null) -> FloatValue(1.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Boolean true, Null)
    eval cell expr workbook |> should equal (FloatValue 1.0) 

[<Test>]
let ``Null+false -> Add(Null, false) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Null, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``false+Null -> Add(false, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Boolean false, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Null-Null -> Sub(Null, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Null, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Null-"" -> Sub(Null, "") -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Null, String "")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``""-Null -> Sub("", Null) -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(String "", Null)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Null-"42" -> Sub(Null, "42") -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Null, String "42")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``"42"-Null -> Sub("42", Null) -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(String "42", Null)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Null-0.0 -> Sub(Null, 0.0) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Null, Float 0.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``0.0-Null -> Sub(0.0, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 0.0, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Null-42.0 -> Sub(Null, 42.0) -> FloatValue(-42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Null, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue -42.0)

[<Test>]
let ``42.0-Null -> Sub(42.0, Null) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 42.0, Null)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``Null--42.0 -> Sub(Null, -42.0) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Null, Float -42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``-42.0-Null -> Sub(-42.0, Null) -> FloatValue(-42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float -42.0, Null)
    eval cell expr workbook |> should equal (FloatValue -42.0) 

[<Test>]
let ``Null-true -> Sub(Null, true) -> FloatValue(-1.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Null, Boolean true)
    eval cell expr workbook |> should equal (FloatValue -1.0)

[<Test>]
let ``true-Null -> Sub(true, Null) -> FloatValue(1.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Boolean true, Null)
    eval cell expr workbook |> should equal (FloatValue 1.0) 

[<Test>]
let ``Null-false -> Sub(Null, false) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Null, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``false-Null -> Sub(false, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Boolean false, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0) 

[<Test>]
let ``Null*Null -> Mul(Null, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Null, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Null*"" -> Mul(Null, "") -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Null, String "")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``""*Null -> Mul("", Null) -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(String "", Null)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Null*"42" -> Mul(Null, "42") -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Null, String "42")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``"42"*Null -> Mul("42", Null) -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(String "42", Null)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Null*0.0 -> Mul(Null, 0.0) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Null, Float 0.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``0.0*Null -> Mul(0.0, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 0.0, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Null*42.0 -> Mul(Null, 42.0) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Null, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``42.0*Null -> Mul(42.0, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 42.0, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Null*-42.0 -> Mul(Null, -42.0) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Null, Float -42.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``-42.0*Null -> Mul(-42.0, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float -42.0, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0) 

[<Test>]
let ``Null*true -> Mul(Null, true) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Null, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``true*Null -> Mul(true, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Boolean true, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0) 

[<Test>]
let ``Null*false -> Mul(Null, false) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Null, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``false*Null -> Mul(false, Null) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Boolean false, Null)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Null/Null -> Div(Null, Null) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Null, Null)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Null/"" -> Div(Null, "") -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Null, String "")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``""/Null -> Div("", Null) -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(String "", Null)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Null/"42" -> Div(Null, "42") -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Null, String "42")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``"42"/Null -> Div("42", Null) -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(String "42", Null)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Null/0.0 -> Div(Null, 0.0) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Null, Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``0.0/Null -> Div(0.0, Null) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 0.0, Null)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Null/1.0 -> Div(Null, 1.0) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Null, Float 1.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``1.0/Null -> Div(1.0, Null) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 1.0, Null)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Null/-1.0 -> Div(Null, -1.0) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Null, Float -1.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``-1.0/Null -> Div(-1.0, Null) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float -1.0, Null)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Null/true -> Div(Null, true) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Null, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``true/Null -> Div(true, Null) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Boolean true, Null)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Null/false -> Div(Null, false) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Null, Boolean false)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``false/Null -> Div(false, Null) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Boolean false, Null)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Null^Null -> Pow(Null, Null) -> ErrorValue(Number)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Null, Null)
    eval cell expr workbook |> should equal (ErrorValue Number)

[<Test>]
let ``Null^"" -> Pow(Null, "") -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Null, String "")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``""^Null -> Pow("", Null) -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(String "", Null)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Null^"42" -> Pow(Null, "42") -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Null, String "42")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``"42"^Null -> Pow("42", Null) -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(String "42", Null)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Null^0.0 -> Pow(Null, 0.0) -> ErrorValue(Number)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Null, Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Number)

[<Test>]
let ``0.0^Null -> Pow(0.0, Null) -> ErrorValue(Number)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 0.0, Null)
    eval cell expr workbook |> should equal (ErrorValue Number)

[<Test>]
let ``Null^1.0 -> Pow(Null, 1.0) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Null, Float 1.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``1.0^Null -> Pow(1.0, Null) -> FloatValue(1.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 1.0, Null)
    eval cell expr workbook |> should equal (FloatValue 1.0)

[<Test>]
let ``Null^-1.0 -> Pow(Null, -1.0) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Null, Float -1.0)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``-1^Null -> Pow(-1, Null) -> FloatValue(1.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float -1.0, Null)
    eval cell expr workbook |> should equal (FloatValue 1.0) 

[<Test>]
let ``Null^true -> Pow(Null, true) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Null, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``true^Null -> Pow(true, Null) -> FloatValue(1.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Boolean true, Null)
    eval cell expr workbook |> should equal (FloatValue 1.0) 

[<Test>]
let ``Null^false -> Pow(Null, false) -> ErrorValue(Number)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Null, Boolean false)
    eval cell expr workbook |> should equal (ErrorValue Number)

[<Test>]
let ``false^Null -> Pow(false, Null) -> ErrorValue(Number)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Boolean false, Null)
    eval cell expr workbook |> should equal (ErrorValue Number)

[<Test>]
let ``0.0^-1.0 -> Pow(0.0, -1.0) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 0.0, Float -1.0)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``0.0^0.0 -> Pow(0.0, 0.0) -> ErrorValue(Number)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 0.0, Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Number)
    
[<Test>]
let ``false^-1.0 -> Pow(false, -1.0) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Boolean false, Float -1.0)
    eval cell expr workbook |> should equal (ErrorValue DivZero)