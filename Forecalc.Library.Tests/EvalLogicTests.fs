module EvalLogicTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``Eq(String "42", String "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(String "42", String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(String "Ford Prefect", String "ford prefect") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(String "Ford Prefect", String "ford prefect")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(Boolean false, String "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Boolean false, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Eq(Error("#REF!", String "42") -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Error Reference, String "42")
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Eq(String "42", Error(Reference) -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(String "42", Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Eq(String "42", EscapedString("42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(String "42", EscapedString "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(Blank, String "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Blank, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Eq(String "42", Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(String "42", Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Eq(Blank, String "") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Blank, String "")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(String "", Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(String "", Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(Blank, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Blank, Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(Blank, Float 0.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Blank, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue true)
 
[<Test>]
let ``Eq(Float 0.0, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Float 0.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(Blank, Float 42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Blank, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue false)
    
[<Test>]
let ``Eq(Blank, Boolean false) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Blank, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue true)   
     
[<Test>]
let ``Eq(Blank, Boolean true) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Blank, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Eq(Float 42.0, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Float 42.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue false)
    
[<Test>]
let ``Eq(Boolean false, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Boolean false, Blank)
    eval cell expr workbook |> should equal (BooleanValue true)   
     
[<Test>]
let ``Eq(Boolean true, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Boolean true, Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(String "42", String "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(String "42", String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(String "Ford Prefect", String "ford prefect") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(String "Ford Prefect", String "ford prefect")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(Boolean false, String "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Boolean false, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``NotEq(Error("#REF!", String "42") -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Error Reference, String "42")
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``NotEq(String "42", Error(Reference) -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(String "42", Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``NotEq(String "42", EscapedString("42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(String "42", EscapedString "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(Blank, String "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Blank, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``NotEq(String "42", Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(String "42", Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``NotEq(Blank, String "") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Blank, String "")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(String "", Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(String "", Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(Blank, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Blank, Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(Blank, Float 0.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Blank, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(Blank, Float 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Blank, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)
    
[<Test>]
let ``NotEq(Blank, Boolean false) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Blank, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue false)   
     
[<Test>]
let ``NotEq(Blank, Boolean true) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Blank, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``NotEq(Float 0.0, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Float 0.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(Float 42.0, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Float 42.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue true)
    
[<Test>]
let ``NotEq(Boolean false, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Boolean false, Blank)
    eval cell expr workbook |> should equal (BooleanValue false)   
     
[<Test>]
let ``NotEq(Boolean true, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Boolean true, Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

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
let ``Lt(Error("#REF!", String "42") -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Error Reference, String "42")
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Lt(String "42", Error(Reference) -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(String "42", Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Gently<Dirk -> Lt(String "Gently", String("Dirk") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(String "Gently", String "Dirk")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Dirk<Gently -> Lt(String "Dirk", String("Gently") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(String "Dirk", String "Gently")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Gt(Float 0.0, Float 42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Float 0.0, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``String>Float -> Gt(String "42", Float 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(String "42", Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not Float>String -> Gt(Float 42.0, String "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Float 42.0, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)
 
[<Test>]
let ``Not String>Bool -> Gt(String "42", Boolean true) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(String "42", Boolean true)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Bool>String -> Gt(Boolean true, String "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Boolean true, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not false>true -> Gt(Boolean false, Boolean true) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Boolean false, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``true>false -> Gt(Boolean true, Boolean false) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Boolean true, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Gt(Error("#REF!", String "42") -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Error Reference, String "42")
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Gt(String "42", Error(Reference) -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(String "42", Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Gently>Dirk -> Lt(String "Gently", String("Dirk") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(String "Gently", String "Dirk")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not Dirk>Gently -> Gt(String "Dirk", String("Gently") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(String "Dirk", String "Gently")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Lte(Float 0.0, Float 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Float 0.0, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not String<=Float -> Lte(String "42", Float 42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(String "42", Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Float<=String -> Lte(Float 42.0, String "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Float 42.0, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)
 
[<Test>]
let ``String<=Bool -> Lte(String "42", Boolean true) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(String "42", Boolean true)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not Bool<=String -> Lte(Boolean true, String "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Boolean true, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``false<=true -> Lte(Boolean false, Boolean true) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Boolean false, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not true<=false -> Lte(Boolean true, Boolean false) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Boolean true, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Lte(Error("#REF!", String "42") -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Error Reference, String "42")
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Lte(String "42", Error(Reference) -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(String "42", Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Gently<=Dirk -> Lte(String "Gently", String("Dirk") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(String "Gently", String "Dirk")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Dirk<=Gently -> Lte(String "Dirk", String("Gently") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(String "Dirk", String "Gently")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Dirk Gently<=dirk gently -> Lte(String "Dirk Gently", String("dirk gently") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(String "Dirk Gently", String "dirk tently")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``42<=42 -> Lte(Float 42.0, Float 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Float 42.0, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Gte(Float 0.0, Float 42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Float 0.0, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``String>=Float -> Gte(String "42", Float 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(String "42", Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not Float>=String -> Gte(Float 42.0, String "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Float 42.0, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)
 
[<Test>]
let ``Not String>=Bool -> Gte(String "42", Boolean true) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(String "42", Boolean true)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Bool>=String -> Gte(Boolean true, String "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Boolean true, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not false>=true -> Gte(Boolean false, Boolean true) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Boolean false, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``true>=false -> Gte(Boolean true, Boolean false) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Boolean true, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Gte(Error("#REF!", String "42") -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Error Reference, String "42")
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Gte(String "42", Error(Reference) -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(String "42", Error Reference)
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Gently>=Dirk -> Lt(String "Gently", String("Dirk") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(String "Gently", String "Dirk")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Not Dirk>=Gently -> Gte(String "Dirk", String("Gently") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(String "Dirk", String "Gently")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Dirk Gently>=dirk gently -> Gte(String "Dirk Gently", String("dirk gently") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(String "Dirk Gently", String "dirk gently")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``42>=42 -> Gte(Float 42.0, Float 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Float 42.0, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Blank<Blank -> Lt(Blank, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Blank, Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Blank<"" -> Lt(Blank, "") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Blank, String "")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``""<Blank -> Lt("", Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(String "", Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Blank<"42" -> Lt(Blank, "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Blank, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``"42"<Blank -> Lt("42", Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(String "42", Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Blank<0.0 -> Lt(Blank, 0.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Blank, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``0.0<Blank -> Lt(0.0, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Float 0.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Blank<42.0 -> Lt(Blank, 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Blank, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``42.0<Blank -> Lt(42.0, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Float 42.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Blank<-42.0 -> Lt(Blank, -42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Blank, Float -42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``-42.0<Blank -> Lt(-42.0, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Float -42.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue true) 

[<Test>]
let ``Blank<true -> Lt(Blank, true) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Blank, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``true<Blank -> Lt(true, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Boolean true, Blank)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Blank<false -> Lt(Blank, false) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Blank, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``false<Blank -> Lt(false, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Boolean false, Blank)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Blank>Blank -> Gt(Blank, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Blank, Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Blank>"" -> Gt(Blank, "") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Blank, String "")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``"">Blank -> Gt("", Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(String "", Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Blank>"42" -> Gt(Blank, "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Blank, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``"42">Blank -> Gt("42", Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(String "42", Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Blank>0.0 -> Gt(Blank, 0.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Blank, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``0.0>Blank -> Gt(0.0, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Float 0.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Blank>42.0 -> Gt(Blank, 42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Blank, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``42.0>Blank -> Gt(42.0, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Float 42.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Blank>-42.0 -> Gt(Blank, -42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Blank, Float -42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``-42.0>Blank -> Gt(-42.0, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Float -42.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Blank>true -> Gt(Blank, true) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Blank, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``true>Blank -> Gt(true, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Boolean true, Blank)
    eval cell expr workbook |> should equal (BooleanValue true) 

[<Test>]
let ``Blank>false -> Gt(Blank, false) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Blank, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``false>Blank -> Gt(false, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Boolean false, Blank)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Blank<=Blank -> Lte(Blank, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Blank, Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Blank<="" -> Lte(Blank, "") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Blank, String "")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``""<=Blank -> Lte("", Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(String "", Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Blank<="42" -> Lte(Blank, "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Blank, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``"42"<=Blank -> Lte("42", Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(String "42", Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Blank<=0.0 -> Lte(Blank, 0.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Blank, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``0.0<=Blank -> Lte(0.0, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Float 0.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Blank<=42.0 -> Lte(Blank, 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Blank, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``42.0<=Blank -> Lte(42.0, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Float 42.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Blank<=-42.0 -> Lte(Blank, -42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Blank, Float -42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``-42.0<=Blank -> Lte(-42.0, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Float -42.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue true) 

[<Test>]
let ``Blank<=true -> Lte(Blank, true) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Blank, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``true<=Blank -> Lte(true, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Boolean true, Blank)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Blank<=false -> Lte(Blank, false) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Blank, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``false<=Blank -> Lte(false, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Boolean false, Blank)
    eval cell expr workbook |> should equal (BooleanValue true) 

[<Test>]
let ``Blank>=Blank -> Gte(Blank, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Blank, Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Blank>="" -> Gte(Blank, "") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Blank, String "")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``"">=Blank -> Gte("", Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(String "", Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Blank>="42" -> Gte(Blank, "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Blank, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``"42">=Blank -> Gte("42", Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(String "42", Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Blank>=0.0 -> Gte(Blank, 0.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Blank, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``0.0>=Blank -> Gte(0.0, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Float 0.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Blank>=42.0 -> Gte(Blank, 42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Blank, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``42.0>=Blank -> Gte(42.0, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Float 42.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Blank>=-42.0 -> Gte(Blank, -42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Blank, Float -42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``-42.0>=Blank -> Gte(-42.0, Blank) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Float -42.0, Blank)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Blank>=true -> Gte(Blank, true) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Blank, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``true>=Blank -> Gte(true, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Boolean true, Blank)
    eval cell expr workbook |> should equal (BooleanValue true) 

[<Test>]
let ``Blank>=false -> Gte(Blank, false) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Blank, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``false>=Blank -> Gte(false, Blank) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Boolean false, Blank)
    eval cell expr workbook |> should equal (BooleanValue true) 