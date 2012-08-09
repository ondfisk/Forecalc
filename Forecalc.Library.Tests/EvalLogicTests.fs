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
let ``Eq(Null, String "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Null, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Eq(String "42", Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(String "42", Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Eq(Null, String "") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Null, String "")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(String "", Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(String "", Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(Null, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Null, Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(Null, Float 0.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Null, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue true)
 
[<Test>]
let ``Eq(Float 0.0, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Float 0.0, Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Eq(Null, Float 42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Null, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue false)
    
[<Test>]
let ``Eq(Null, Boolean false) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Null, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue true)   
     
[<Test>]
let ``Eq(Null, Boolean true) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Null, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Eq(Float 42.0, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Float 42.0, Null)
    eval cell expr workbook |> should equal (BooleanValue false)
    
[<Test>]
let ``Eq(Boolean false, NULL) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Boolean false, Null)
    eval cell expr workbook |> should equal (BooleanValue true)   
     
[<Test>]
let ``Eq(Boolean true, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheetrue" ; Row = 1 ; Col = 1 }
    let expr = Eq(Boolean true, Null)
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
let ``NotEq(Null, String "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Null, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``NotEq(String "42", Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(String "42", Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``NotEq(Null, String "") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Null, String "")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(String "", Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(String "", Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(Null, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Null, Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(Null, Float 0.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Null, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(Null, Float 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Null, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)
    
[<Test>]
let ``NotEq(Null, Boolean false) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Null, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue false)   
     
[<Test>]
let ``NotEq(Null, Boolean true) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Null, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``NotEq(Float 0.0, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Float 0.0, Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``NotEq(Float 42.0, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Float 42.0, Null)
    eval cell expr workbook |> should equal (BooleanValue true)
    
[<Test>]
let ``NotEq(Boolean false, NULL) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Boolean false, Null)
    eval cell expr workbook |> should equal (BooleanValue false)   
     
[<Test>]
let ``NotEq(Boolean true, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Boolean true, Null)
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
let ``Null<Null -> Lt(Null, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Null, Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Null<"" -> Lt(Null, "") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Null, String "")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``""<Null -> Lt("", Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(String "", Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Null<"42" -> Lt(Null, "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Null, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``"42"<Null -> Lt("42", Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(String "42", Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Null<0.0 -> Lt(Null, 0.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Null, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``0.0<Null -> Lt(0.0, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Float 0.0, Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Null<42.0 -> Lt(Null, 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Null, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``42.0<Null -> Lt(42.0, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Float 42.0, Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Null<-42.0 -> Lt(Null, -42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Null, Float -42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``-42.0<Null -> Lt(-42.0, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Float -42.0, Null)
    eval cell expr workbook |> should equal (BooleanValue true) 

[<Test>]
let ``Null<true -> Lt(Null, true) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Null, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``true<Null -> Lt(true, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Boolean true, Null)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Null<false -> Lt(Null, false) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Null, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``false<Null -> Lt(false, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Boolean false, Null)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Null>Null -> Gt(Null, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Null, Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Null>"" -> Gt(Null, "") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Null, String "")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``"">Null -> Gt("", Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(String "", Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Null>"42" -> Gt(Null, "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Null, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``"42">Null -> Gt("42", Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(String "42", Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Null>0.0 -> Gt(Null, 0.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Null, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``0.0>Null -> Gt(0.0, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Float 0.0, Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Null>42.0 -> Gt(Null, 42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Null, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``42.0>Null -> Gt(42.0, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Float 42.0, Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Null>-42.0 -> Gt(Null, -42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Null, Float -42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``-42.0>Null -> Gt(-42.0, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Float -42.0, Null)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Null>true -> Gt(Null, true) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Null, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``true>Null -> Gt(true, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Boolean true, Null)
    eval cell expr workbook |> should equal (BooleanValue true) 

[<Test>]
let ``Null>false -> Gt(Null, false) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Null, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``false>Null -> Gt(false, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Boolean false, Null)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Null<=Null -> Lte(Null, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Null, Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Null<="" -> Lte(Null, "") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Null, String "")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``""<=Null -> Lte("", Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(String "", Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Null<="42" -> Lte(Null, "42") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Null, String "42")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``"42"<=Null -> Lte("42", Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(String "42", Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Null<=0.0 -> Lte(Null, 0.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Null, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``0.0<=Null -> Lte(0.0, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Float 0.0, Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Null<=42.0 -> Lte(Null, 42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Null, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``42.0<=Null -> Lte(42.0, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Float 42.0, Null)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``Null<=-42.0 -> Lte(Null, -42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Null, Float -42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``-42.0<=Null -> Lte(-42.0, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Float -42.0, Null)
    eval cell expr workbook |> should equal (BooleanValue true) 

[<Test>]
let ``Null<=true -> Lte(Null, true) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Null, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``true<=Null -> Lte(true, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Boolean true, Null)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Null<=false -> Lte(Null, false) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Null, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``false<=Null -> Lte(false, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Boolean false, Null)
    eval cell expr workbook |> should equal (BooleanValue true) 

[<Test>]
let ``Null>=Null -> Gte(Null, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Null, Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Null>="" -> Gte(Null, "") -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Null, String "")
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``"">=Null -> Gte("", Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(String "", Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Null>="42" -> Gte(Null, "42") -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Null, String "42")
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``"42">=Null -> Gte("42", Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(String "42", Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Null>=0.0 -> Gte(Null, 0.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Null, Float 0.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``0.0>=Null -> Gte(0.0, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Float 0.0, Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Null>=42.0 -> Gte(Null, 42.0) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Null, Float 42.0)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``42.0>=Null -> Gte(42.0, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Float 42.0, Null)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``Null>=-42.0 -> Gte(Null, -42.0) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Null, Float -42.0)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``-42.0>=Null -> Gte(-42.0, Null) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Float -42.0, Null)
    eval cell expr workbook |> should equal (BooleanValue false) 

[<Test>]
let ``Null>=true -> Gte(Null, true) -> BooleanValue(false)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Null, Boolean true)
    eval cell expr workbook |> should equal (BooleanValue false)

[<Test>]
let ``true>=Null -> Gte(true, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Boolean true, Null)
    eval cell expr workbook |> should equal (BooleanValue true) 

[<Test>]
let ``Null>=false -> Gte(Null, false) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Null, Boolean false)
    eval cell expr workbook |> should equal (BooleanValue true)

[<Test>]
let ``false>=Null -> Gte(false, Null) -> BooleanValue(true)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Boolean false, Null)
    eval cell expr workbook |> should equal (BooleanValue true) 