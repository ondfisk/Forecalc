module EvalRefTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``Invalid absolute reference -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Ref(Cell({ Sheet = None ; Row = 0 ; RowAbs = true ; Col = 0 ; ColAbs = true }))
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Invalid relative reference -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Ref(Cell({ Sheet = None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }))
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Valid absolute reference -> Value``() =
    let workbook = QT4.create<CellContent>()
    workbook.[41, 41] <- Some({ Expr = Float 42.0 ; Value = FloatValue 42.0 ; Volatile = false })
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Ref(Cell({ Sheet = None ; Row = 42 ; RowAbs = true ; Col = 42 ; ColAbs = true }))
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``Valid relative reference -> Value()``() =
    let workbook = QT4.create<CellContent>()
    workbook.[41, 41] <- Some({ Expr = Float 42.0 ; Value = FloatValue 42.0 ; Volatile = false })
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Ref(Cell({ Sheet = None ; Row = 41 ; RowAbs = false ; Col = 41 ; ColAbs = false }))
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``=-range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Negate(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=range=42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0=range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Eq(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)
    
[<Test>]
let ``=range<>42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0<>range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = NotEq(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=range<42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0<range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lt(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=range>42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0>range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gt(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=range<=42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0<=range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Lte(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=range>=42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0>=range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Gte(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=range+42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0+range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Add(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=range-42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0-range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Sub(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=range*42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0*range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Mul(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=range/42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0/range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Div(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=range^42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0^range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Pow(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=range&42.0 -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})), Float 42.0)
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``=42.0&range -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Concat(Float 42.0, Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 2 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 10 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))
    eval cell expr workbook |> should equal (ErrorValue Value)
    
[<Test>]
let ``Circular cell reference -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Ref(Cell({ Sheet = None ; Row = 0 ; RowAbs = false ; Col = 0 ; ColAbs = false }))
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``Circular cell range -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))
    eval cell expr workbook |> should equal (ErrorValue Reference)