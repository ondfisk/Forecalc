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
let ``Error("#VALUE!") & 42.0 -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Error "#VALUE!", Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

[<Test>]
let ``42.0 & Error("#VALUE!") -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Float 42.0, Error "#VALUE!")
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

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
let ``"42"+0 -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

[<Test>]
let ``0+"42" -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

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
let ``#REF! + 42 -> #REF!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Error "#REF!", Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``42 + #REF! -> #REF!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 42.0, Error "#REF!")
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``43-1 -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 43.0, Float 1.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``"42"-0 -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

[<Test>]
let ``0-"42" -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

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
let ``#REF! - 42 -> #REF!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Error "#REF!", Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``42 - #REF! -> #REF!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 42.0, Error "#REF!")
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``6*7 -> 42``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 6.0, Float 7.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``"42"*0 -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

[<Test>]
let ``0*"42" -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

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
let ``#REF! * 42 -> #REF!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Error "#REF!", Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``42 * #REF! -> #REF!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 42.0, Error "#REF!")
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``126.0/3.0 -> 42.0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 126.0, Float 3.0)
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``"42"/0 -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

[<Test>]
let ``0/"42" -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

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
    eval cell expr workbook |> should equal (ErrorValue "#DIV/0!")

[<Test>]
let ``42.0/0.0 -> #DIV/0!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 42.0, Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue "#DIV/0!")

[<Test>]
let ``#REF! / 42 -> #REF!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Error "#REF!", Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``42 / #REF! -> #REF!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 42.0, Error "#REF!")
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``2^8 -> 256``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 2.0, Float 8.0)
    eval cell expr workbook |> should equal (FloatValue 256.0)

[<Test>]
let ``"42"^0 -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(String "42.0", Float 0.0)
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

[<Test>]
let ``0^"42" -> Error("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 0.0, String "42.0")
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

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
let ``#REF!^ 42 -> #REF!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Error "#REF!", Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``42^ #REF! -> #REF!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 42.0, Error "#REF!")
    eval cell expr workbook |> should equal (ErrorValue "#REF!")