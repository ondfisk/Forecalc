module EvalFunTests

open System.Collections.Generic
open Xunit
open FsUnit.Xunit
open Forecalc.Library
open Forecalc.Library.Eval

[<Fact>]
let ``Unknown function -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUPER", [ String "" ; Blank ; Blank ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)

[<Fact>]
let ``IF(non-boolean;_;_) -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ String "" ; Blank ; Blank ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Value)

[<Fact>]
let ``IF() -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)

[<Fact>]
let ``IF(true, 42.0, Blank) -> FloatValue(42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Boolean true ; Float 42.0 ; Blank ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 42.0)

[<Fact>]
let ``IF(false, Blank, 42.0) -> FloatValue(42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Boolean false ; Blank ; Float 42.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 42.0)

[<Fact>]
let ``IF(Blank, Blank, 42.0) -> FloatValue(42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Blank ; Blank ; Float 42.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 42.0)

[<Fact>]
let ``IF(0.0, Blank, 42.0) -> FloatValue(42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Float 0.0 ; Blank ; Float 42.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 42.0)

[<Fact>]
let ``IF(1.0, 42.0, Blank) -> FloatValue(42.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Float 1.0 ; Float 42.0 ; Blank ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 42.0)

[<Fact>]
let ``IF(Sheet2:A1, 42.0, Blank) -> FloatValue(42.0)``() =
    let worksheet = QT4.create<CellContent>()
    worksheet.[0, 0] <- Some({ Expr = Boolean true ; Value = BooleanValue true ; Volatile = false })
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ; "Sheet2" , worksheet ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("IF", [ Ref(Cell({ Sheet = Some "Sheet2" ; Col = 0 ; ColAbs = false ; Row = 0 ; RowAbs = false })) ; Float 42.0 ; Blank ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 42.0)

[<Fact>]
let ``SUM() -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)

[<Fact>]
let ``SUM("") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [ String "" ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

[<Fact>]
let ``SUM(true, false) -> FloatValue 1.0``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [ Boolean true ; Boolean false ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 1.0)

[<Fact>]
let ``SUM(B1:B5) -> FloatValue(42.0)``() =
    let worksheet = QT4.create<CellContent>()
    worksheet.[1, 0] <- Some({ Expr = Float 40.0 ; Value = FloatValue 40.0 ; Volatile = false })
    worksheet.[1, 1] <- Some({ Expr = String "Zero" ; Value = StringValue "Zero" ; Volatile = false })
    worksheet.[1, 2] <- Some({ Expr = Boolean true ; Value = BooleanValue true ; Volatile = false })
    worksheet.[1, 3] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
    worksheet.[1, 4] <- Some({ Expr = Blank ; Value = NullValue ; Volatile = false })
    let workbook = Map.ofList [ "Sheet1", worksheet ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ;  Col = 1 ; ColAbs = false ; Row = 0 ; RowAbs = false } ; BottomRight = { Sheet = None ; Col = 1 ; ColAbs = false ; Row = 4 ; RowAbs = false } } ))])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 42.0)

[<Fact>]
let ``SUM(Sheet2!B1:B5) -> FloatValue(42.0)``() =
    let worksheet = QT4.create<CellContent>()
    worksheet.[1, 0] <- Some({ Expr = Float 40.0 ; Value = FloatValue 40.0 ; Volatile = false })
    worksheet.[1, 1] <- Some({ Expr = String "Zero" ; Value = StringValue "Zero" ; Volatile = false })
    worksheet.[1, 2] <- Some({ Expr = Boolean true ; Value = BooleanValue true ; Volatile = false })
    worksheet.[1, 3] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
    worksheet.[1, 4] <- Some({ Expr = Blank ; Value = NullValue ; Volatile = false })
    let workbook = Map.ofList [ "Sheet1", QT4.create<CellContent>() ; "Sheet2", worksheet ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [Ref(Range({ Sheet = Some "Sheet2" ; TopLeft = { Sheet = Some "Sheet2" ;  Col = 1 ; ColAbs = false ; Row = 0 ; RowAbs = false } ; BottomRight = { Sheet = Some "Sheet2" ; Col = 1 ; ColAbs = false ; Row = 4 ; RowAbs = false } } ))])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 42.0)

[<Fact>]
let ``SUM(42.0, Error(Number)) -> ErrorValue(Number)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [Float 42.0 ; Error Number])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Number)

[<Fact>]
let ``SUM(B1) -> ErrorValue(DivZero)``() =
    let worksheet = QT4.create<CellContent>()
    worksheet.[1, 0] <- Some({ Expr = Error DivZero ; Value = ErrorValue DivZero ; Volatile = false })
    let workbook = Map.ofList [ "Sheet1", worksheet ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SUM", [Ref(Cell({ Sheet = None ; Col = 1 ; ColAbs = false ; Row = 0 ; RowAbs = false }))])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue DivZero)

[<Fact>]
let ``COUNT() -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)

[<Fact>]
let ``COUNT(42.0, true) -> FloatValue(1)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [ Float 42.0 ; Boolean true ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 1.0)

[<Fact>]
let ``COUNT("ReferenceNUM!) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [ Error Number ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

[<Fact>]
let ``COUNT(B1:B5) -> FloatValue(2.0)``() =
    let worksheet = QT4.create<CellContent>()
    worksheet.[1, 0] <- Some({ Expr = Float 40.0 ; Value = FloatValue 40.0 ; Volatile = false })
    worksheet.[1, 1] <- Some({ Expr = String "Zero" ; Value = StringValue "Zero" ; Volatile = false })
    worksheet.[1, 2] <- Some({ Expr = Boolean true ; Value = BooleanValue true ; Volatile = false })
    worksheet.[1, 3] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
    worksheet.[1, 4] <- Some({ Expr = Blank ; Value = NullValue ; Volatile = false })
    let workbook = Map.ofList [ "Sheet1", worksheet ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ;  Col = 1 ; ColAbs = false ; Row = 0 ; RowAbs = false } ; BottomRight = { Sheet = None ; Col = 1 ; ColAbs = false ; Row = 4 ; RowAbs = false } } ))])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 2.0)

[<Fact>]
let ``COUNT(B1:B5) -> FloatValue(3.0)``() =
    let worksheet = QT4.create<CellContent>()
    worksheet.[1, 0] <- Some({ Expr = Float 40.0 ; Value = FloatValue 40.0 ; Volatile = false })
    worksheet.[1, 1] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
    worksheet.[1, 2] <- Some({ Expr = Boolean true ; Value = BooleanValue true ; Volatile = false })
    worksheet.[1, 3] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
    worksheet.[1, 4] <- Some({ Expr = Error Number ; Value = ErrorValue Number ; Volatile = false })
    let workbook = Map.ofList [ "Sheet1", worksheet ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COUNT", [Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ;  Col = 1 ; ColAbs = false ; Row = 0 ; RowAbs = false } ; BottomRight = { Sheet = None ; Col = 1 ; ColAbs = false ; Row = 4 ; RowAbs = false } } ))])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 3.0)

[<Fact>]
let ``=RAND(42) -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("RAND", [ Float 42.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)

[<Fact>]
let ``=RAND() -> FloatValue(0.0-1.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let dirty = Set.empty<AbsCell>
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("RAND", [])
    match eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) with
        | FloatValue(v) ->
            v |> should be (greaterThanOrEqualTo 0.0)
            v |> should be (lessThanOrEqualTo 1.0)
        | _ -> failwith "Test failed"
