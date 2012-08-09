module EvalFunTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``Unknown function -> ErrorValue(Parse)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUPER", [ String "" ; Null ; Null ])
    eval cell expr workbook |> should equal (ErrorValue Parse)

[<Test>]
let ``IF(non-boolean;_;_) -> ErrorValue("ReferenceVALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ String "" ; Null ; Null ])
    eval cell expr workbook |> should equal (ErrorValue Value)

[<Test>]
let ``IF() -> ErrorValue(Parse)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [])
    eval cell expr workbook |> should equal (ErrorValue Parse)

[<Test>]
let ``IF(true, 42.0, null) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Boolean true ; Float 42.0 ; Null ])
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``IF(false, null, 42.0) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Boolean false ; Null ; Float 42.0 ])
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``IF(null, null, 42.0) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Null ; Null ; Float 42.0 ])
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``IF(0.0, null, 42.0) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Float 0.0 ; Null ; Float 42.0 ])
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``IF(1.0, 42.0, null) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Float 1.0 ; Float 42.0 ; Null ])
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``SUM() -> ErrorValue(Parse)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [])
    eval cell expr workbook |> should equal (ErrorValue Parse)

[<Test>]
let ``SUM("") -> ErrorValue(Value)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [ String "" ])
    eval cell expr workbook |> should equal (ErrorValue Parse)

[<Test>]
let ``SUM(true, false) -> FloatValue 1.0``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [ Boolean true ; Boolean false ])
    eval cell expr workbook |> should equal (FloatValue 1.0)

[<Test>]
let ``SUM(B1:B5) -> FloatValue(42.0)``() =
    let workbook = QT4.create<CellContent>()
    workbook.[1, 0] <- Some({ Expr = Float 40.0 ; Value = FloatValue 40.0 ; Volatile = false })
    workbook.[1, 1] <- Some({ Expr = String "Zero" ; Value = StringValue "Zero" ; Volatile = false })
    workbook.[1, 2] <- Some({ Expr = Boolean true ; Value = BooleanValue true ; Volatile = false })
    workbook.[1, 3] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
    workbook.[1, 4] <- Some({ Expr = Null ; Value = NullValue ; Volatile = false })
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ;  Col = 1 ; ColAbs = false ; Row = 0 ; RowAbs = false } ; BottomRight = { Sheet = None ; Col = 1 ; ColAbs = false ; Row = 4 ; RowAbs = false } } ))])
    eval cell expr workbook |> should equal (FloatValue 42.0)

[<Test>]
let ``SUM(42.0, Error(Number)) -> ErrorValue(Number)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [Float 42.0 ; Error Number])
    eval cell expr workbook |> should equal (ErrorValue Number)

[<Test>]
let ``SUM(A1) -> ErrorValue(Reference)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [Ref(Cell({ Sheet = None ; Col = 0 ; ColAbs = false ; Row = 0 ; RowAbs = false }))])
    eval cell expr workbook |> should equal (ErrorValue Reference)

[<Test>]
let ``SUM(B1) -> ErrorValue("ReferenceDIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    workbook.[1, 0] <- Some({ Expr = Error DivZero ; Value = ErrorValue DivZero ; Volatile = false })
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [Ref(Cell({ Sheet = None ; Col = 1 ; ColAbs = false ; Row = 0 ; RowAbs = false }))])
    eval cell expr workbook |> should equal (ErrorValue DivZero)

[<Test>]
let ``COUNT() -> ErrorValue(Parse)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [])
    eval cell expr workbook |> should equal (ErrorValue Parse)

[<Test>]
let ``COUNT(42.0, true) -> FloatValue(1)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [ Float 42.0 ; Boolean true ])
    eval cell expr workbook |> should equal (FloatValue 1.0)

[<Test>]
let ``COUNT("ReferenceNUM!) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [ Error Number ])
    eval cell expr workbook |> should equal (FloatValue 0.0)

[<Test>]
let ``COUNT(B1:B5) -> FloatValue(2.0)``() =
    let workbook = QT4.create<CellContent>()
    workbook.[1, 0] <- Some({ Expr = Float 40.0 ; Value = FloatValue 40.0 ; Volatile = false })
    workbook.[1, 1] <- Some({ Expr = String "Zero" ; Value = StringValue "Zero" ; Volatile = false })
    workbook.[1, 2] <- Some({ Expr = Boolean true ; Value = BooleanValue true ; Volatile = false })
    workbook.[1, 3] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
    workbook.[1, 4] <- Some({ Expr = Null ; Value = NullValue ; Volatile = false })
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ;  Col = 1 ; ColAbs = false ; Row = 0 ; RowAbs = false } ; BottomRight = { Sheet = None ; Col = 1 ; ColAbs = false ; Row = 4 ; RowAbs = false } } ))])
    eval cell expr workbook |> should equal (FloatValue 2.0)

[<Test>]
let ``COUNT(B1:B5) -> FloatValue(3.0)``() =
    let workbook = QT4.create<CellContent>()
    workbook.[1, 0] <- Some({ Expr = Float 40.0 ; Value = FloatValue 40.0 ; Volatile = false })
    workbook.[1, 1] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
    workbook.[1, 2] <- Some({ Expr = Boolean true ; Value = BooleanValue true ; Volatile = false })
    workbook.[1, 3] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
    workbook.[1, 4] <- Some({ Expr = Error Number ; Value = ErrorValue Number ; Volatile = false })
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ;  Col = 1 ; ColAbs = false ; Row = 0 ; RowAbs = false } ; BottomRight = { Sheet = None ; Col = 1 ; ColAbs = false ; Row = 4 ; RowAbs = false } } ))])
    eval cell expr workbook |> should equal (FloatValue 3.0)