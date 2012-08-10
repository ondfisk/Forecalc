module EvalBinaryTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``4.0&2.0 -> "42"``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Float 4.0, Float 2.0)
    eval cell expr workbook |> should equal (StringValue "42")

[<Test>]
let ``Error(Value) & 42.0 -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Error Value, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``42.0 & Error(Value) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Float 42.0, Error Value)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``42.0&Blank -> String("42")``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Float 42.0, Blank)
    eval cell expr workbook |> should equal (StringValue "42")

[<Test>]
let ``Blank&42.0 -> String("42")``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Blank, Float 42.0)
    eval cell expr workbook |> should equal (StringValue "42")

[<Test>]
let ``"Life, " & "the " & "Universe " & "and " & "Everything = " & 42.0 -> "LifeT, the Universe and Everything = 42"``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Concat(Concat(Concat(Concat(String "Life, ",String "the "),String "Universe "), String "and "),String "Everything = "),Float 42.0)
    eval cell expr workbook |> should equal (StringValue "Life, the Universe and Everything = 42")

[<Test>]
let ``true&false -> "TRUEFALSE"``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Boolean true, Boolean false)
    eval cell expr workbook |> should equal (StringValue "TRUEFALSE")

[<Test>]
let ``20+22 -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 20.0, Float 22.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``"42"+0 -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``0+"42" -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``true+41.0 -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Boolean true, Float 41.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``41.0+true -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 41.0, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``false+42.0 -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Boolean false, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``42.0+false -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 42.0, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``#REF! + 42 -> ErrorValue(Reference)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Error Reference, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``42 + #REF! -> ErrorValue(Reference)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 42.0, Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``43-1 -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 43.0, Float 1.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``"42"-0 -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``0-"42" -> Error(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``true--41.0 -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Boolean true, Float -41.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``43.0-true -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 43.0, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``false--42.0 -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Boolean false, Float -42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``42.0-false -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 42.0, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``#REF! - 42 -> ErrorValue(Reference)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Error Reference, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``42 - #REF! -> ErrorValue(Reference)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 42.0, Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``6*7 -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 6.0, Float 7.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``"42"*0 -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``0*"42" -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``true*42.0 -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Boolean true, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``42.0*true -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 42.0, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``false*42.0 -> 0``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Boolean false, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``42.0*false -> 0``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 42.0, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``#REF! * 42 -> ErrorValue(Reference)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Error Reference, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``42 * #REF! -> ErrorValue(Reference)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 42.0, Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``126.0/3.0 -> 42.0``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 126.0, Float 3.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``"42"/0 -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``0/"42" -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``true/0.5 -> 2.0``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Boolean true, Float 0.5)
    eval cell expr workbook |> should equal (FloatValue 2.0)

[<Test>]
let ``42.0/true -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 42.0, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``false/42.0 -> 0``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Boolean false, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``42.0/false -> #DIV/0!``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 42.0, Boolean false)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``42.0/0.0 -> #DIV/0!``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 42.0, Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``#REF! / 42 -> ErrorValue(Reference)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Error Reference, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``42 / #REF! -> ErrorValue(Reference)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 42.0, Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``2^8 -> 256``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 2.0, Float 8.0)
    eval cell expr workbook |> should equal (FloatValue 256.0)

[<Test>]
let ``"42"^0 -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``0^"42" -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``true^42.0 -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Boolean true, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 1.0)

[<Test>]
let ``42.0^true -> 42``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 42.0, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``false^42.0 -> 0``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Boolean false, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``42.0^false -> 1``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 42.0, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 1.0)

[<Test>]
let ``#REF!^42 -> ErrorValue(Reference)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Error Reference, Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``42^#REF! -> ErrorValue(Reference)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 42.0, Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Blank+Blank -> Add(Blank, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Blank, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Blank+"" -> Add(Blank, "") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Blank, String "")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``""+Blank -> Add("", Blank) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(String "", Blank)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Blank+"42" -> Add(Blank, "42") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Blank, String "42")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``"42"+Blank -> Add("42", Blank) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(String "42", Blank)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Blank+0.0 -> Add(Blank, 0.0) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Blank, Float 0.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``0.0+Blank -> Add(0.0, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 0.0, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Blank+42.0 -> Add(Blank, -42.0) -> FloatValue(42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Blank, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``42.0+Blank -> Add(42.0, Blank) -> FloatValue(42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 42.0, Blank)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``Blank+-42.0 -> Add(Blank, -42.0) -> FloatValue(-42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Blank, Float -42.0)
    eval cell expr workbook |> should equal (FloatValue -42.0)

[<Test>]
let ``-42.0+Blank -> Add(-42.0, Blank) -> FloatValue(-42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float -42.0, Blank)
    eval cell expr workbook |> should equal (FloatValue -42.0) 

[<Test>]
let ``Blank+true -> Add(Blank, true) -> FloatValue(1.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Blank, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 1.0)

[<Test>]
let ``true+Blank -> Add(true, Blank) -> FloatValue(1.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Boolean true, Blank)
    eval cell expr workbook |> should equal (FloatValue 1.0) 

[<Test>]
let ``Blank+false -> Add(Blank, false) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Blank, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``false+Blank -> Add(false, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Boolean false, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Blank-Blank -> Sub(Blank, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Blank, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Blank-"" -> Sub(Blank, "") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Blank, String "")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``""-Blank -> Sub("", Blank) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(String "", Blank)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Blank-"42" -> Sub(Blank, "42") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Blank, String "42")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``"42"-Blank -> Sub("42", Blank) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(String "42", Blank)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Blank-0.0 -> Sub(Blank, 0.0) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Blank, Float 0.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``0.0-Blank -> Sub(0.0, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 0.0, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Blank-42.0 -> Sub(Blank, 42.0) -> FloatValue(-42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Blank, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue -42.0)

[<Test>]
let ``42.0-Blank -> Sub(42.0, Blank) -> FloatValue(42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 42.0, Blank)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``Blank--42.0 -> Sub(Blank, -42.0) -> FloatValue(42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Blank, Float -42.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``-42.0-Blank -> Sub(-42.0, Blank) -> FloatValue(-42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float -42.0, Blank)
    eval cell expr workbook |> should equal (FloatValue -42.0) 

[<Test>]
let ``Blank-true -> Sub(Blank, true) -> FloatValue(-1.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Blank, Boolean true)
    eval cell expr workbook |> should equal (FloatValue -1.0)

[<Test>]
let ``true-Blank -> Sub(true, Blank) -> FloatValue(1.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Boolean true, Blank)
    eval cell expr workbook |> should equal (FloatValue 1.0) 

[<Test>]
let ``Blank-false -> Sub(Blank, false) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Blank, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``false-Blank -> Sub(false, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Boolean false, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0) 

[<Test>]
let ``Blank*Blank -> Mul(Blank, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Blank, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Blank*"" -> Mul(Blank, "") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Blank, String "")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``""*Blank -> Mul("", Blank) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(String "", Blank)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Blank*"42" -> Mul(Blank, "42") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Blank, String "42")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``"42"*Blank -> Mul("42", Blank) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(String "42", Blank)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Blank*0.0 -> Mul(Blank, 0.0) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Blank, Float 0.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``0.0*Blank -> Mul(0.0, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 0.0, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Blank*42.0 -> Mul(Blank, 42.0) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Blank, Float 42.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``42.0*Blank -> Mul(42.0, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 42.0, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Blank*-42.0 -> Mul(Blank, -42.0) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Blank, Float -42.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``-42.0*Blank -> Mul(-42.0, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float -42.0, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0) 

[<Test>]
let ``Blank*true -> Mul(Blank, true) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Blank, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``true*Blank -> Mul(true, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Boolean true, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0) 

[<Test>]
let ``Blank*false -> Mul(Blank, false) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Blank, Boolean false)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``false*Blank -> Mul(false, Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Boolean false, Blank)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``Blank/Blank -> Div(Blank, Blank) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Blank, Blank)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Blank/"" -> Div(Blank, "") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Blank, String "")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``""/Blank -> Div("", Blank) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(String "", Blank)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Blank/"42" -> Div(Blank, "42") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Blank, String "42")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``"42"/Blank -> Div("42", Blank) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(String "42", Blank)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Blank/0.0 -> Div(Blank, 0.0) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Blank, Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``0.0/Blank -> Div(0.0, Blank) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 0.0, Blank)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Blank/1.0 -> Div(Blank, 1.0) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Blank, Float 1.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``1.0/Blank -> Div(1.0, Blank) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 1.0, Blank)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Blank/-1.0 -> Div(Blank, -1.0) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Blank, Float -1.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``-1.0/Blank -> Div(-1.0, Blank) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float -1.0, Blank)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Blank/true -> Div(Blank, true) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Blank, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``true/Blank -> Div(true, Blank) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Boolean true, Blank)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Blank/false -> Div(Blank, false) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Blank, Boolean false)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``false/Blank -> Div(false, Blank) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Boolean false, Blank)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``Blank^Blank -> Pow(Blank, Blank) -> ErrorValue(Number)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Blank, Blank)
    eval cell expr workbook |> should equal (ErrorValue Number)

[<Test>]
let ``Blank^"" -> Pow(Blank, "") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Blank, String "")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``""^Blank -> Pow("", Blank) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(String "", Blank)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Blank^"42" -> Pow(Blank, "42") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Blank, String "42")
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``"42"^Blank -> Pow("42", Blank) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(String "42", Blank)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``Blank^0.0 -> Pow(Blank, 0.0) -> ErrorValue(Number)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Blank, Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Number)

[<Test>]
let ``0.0^Blank -> Pow(0.0, Blank) -> ErrorValue(Number)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 0.0, Blank)
    eval cell expr workbook |> should equal (ErrorValue Number)

[<Test>]
let ``Blank^1.0 -> Pow(Blank, 1.0) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Blank, Float 1.0)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``1.0^Blank -> Pow(1.0, Blank) -> FloatValue(1.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 1.0, Blank)
    eval cell expr workbook |> should equal (FloatValue 1.0)

[<Test>]
let ``Blank^-1.0 -> Pow(Blank, -1.0) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Blank, Float -1.0)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``-1^Blank -> Pow(-1, Blank) -> FloatValue(1.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float -1.0, Blank)
    eval cell expr workbook |> should equal (FloatValue 1.0) 

[<Test>]
let ``Blank^true -> Pow(Blank, true) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Blank, Boolean true)
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``true^Blank -> Pow(true, Blank) -> FloatValue(1.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Boolean true, Blank)
    eval cell expr workbook |> should equal (FloatValue 1.0) 

[<Test>]
let ``Blank^false -> Pow(Blank, false) -> ErrorValue(Number)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Blank, Boolean false)
    eval cell expr workbook |> should equal (ErrorValue Number)

[<Test>]
let ``false^Blank -> Pow(false, Blank) -> ErrorValue(Number)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Boolean false, Blank)
    eval cell expr workbook |> should equal (ErrorValue Number)

[<Test>]
let ``0.0^-1.0 -> Pow(0.0, -1.0) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 0.0, Float -1.0)
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``0.0^0.0 -> Pow(0.0, 0.0) -> ErrorValue(Number)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 0.0, Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue Number)
    
[<Test>]
let ``false^-1.0 -> Pow(false, -1.0) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Boolean false, Float -1.0)
    eval cell expr workbook |> should equal (ErrorValue DivZero)