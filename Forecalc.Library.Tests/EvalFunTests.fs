module EvalFunTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``Unknown function -> ErrorValue("#PARSE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUPER", [ String "" ; Null ; Null ])
    eval cell expr workbook |> should equal (ErrorValue "#PARSE!")

[<Test>]
let ``IF(non-boolean;_;_) -> ErrorValue("#VALUE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ String "" ; Null ; Null ])
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

[<Test>]
let ``IF() -> ErrorValue("#PARSE!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [])
    eval cell expr workbook |> should equal (ErrorValue "#PARSE!")

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
let ``SUM() -> #PARSE!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [])
    eval cell expr workbook |> should equal (ErrorValue "#PARSE!")

[<Test>]
let ``SUM("") -> #VALUE!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [ String "" ])
    eval cell expr workbook |> should equal (ErrorValue "#VALUE!")

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
let ``SUM(42.0,#NUM!) -> ErrorValue("#NUM!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [Float 42.0 ; Error "#NUM!"])
    eval cell expr workbook |> should equal (ErrorValue "#NUM!")

[<Test>]
let ``SUM(A1) -> ErrorValue("#REF!")``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [Ref(Cell({ Sheet = None ; Col = 0 ; ColAbs = false ; Row = 0 ; RowAbs = false }))])
    eval cell expr workbook |> should equal (ErrorValue "#REF!")

[<Test>]
let ``SUM(B1) -> ErrorValue("#DIV/0!")``() =
    let workbook = QT4.create<CellContent>()
    workbook.[1, 0] <- Some({ Expr = Error "#DIV/0!" ; Value = ErrorValue "#DIV/0!" ; Volatile = false })
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [Ref(Cell({ Sheet = None ; Col = 1 ; ColAbs = false ; Row = 0 ; RowAbs = false }))])
    eval cell expr workbook |> should equal (ErrorValue "#DIV/0!")

[<Test>]
let ``COUNT() -> #PARSE!``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [])
    eval cell expr workbook |> should equal (ErrorValue "#PARSE!")

[<Test>]
let ``COUNT(42.0, true) -> FloatValue(1)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [ Float 42.0 ; Boolean true ])
    eval cell expr workbook |> should equal (FloatValue 1.0)

[<Test>]
let ``COUNT("#NUM!) -> FloatValue(0.0)``() =
    let workbook = QT4.create<CellContent>()
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [ Error "#NUM!" ])
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
    workbook.[1, 4] <- Some({ Expr = Error "#NUM!" ; Value = ErrorValue "#NUM!" ; Volatile = false })
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ;  Col = 1 ; ColAbs = false ; Row = 0 ; RowAbs = false } ; BottomRight = { Sheet = None ; Col = 1 ; ColAbs = false ; Row = 4 ; RowAbs = false } } ))])
    eval cell expr workbook |> should equal (FloatValue 3.0)