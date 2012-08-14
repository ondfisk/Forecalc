module WorkbookTests

open System.Collections.Generic
open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Workbook

[<Test>]
let ``makeDirtySet (empty workbook) -> Set.empty<CellContent>``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    workbook |> makeDirtySet |> should equal (Set.empty<CellContent>)

[<Test>]
let ``makeDirtySet (only non-volatile cells) -> Set.empty<CellContent>``() =
    let sheet1 = QT4.create<CellContent>()
    sheet1.[0, 0] <- Some({ Expr = Float 40.0 ; Value = FloatValue 40.0 ; Volatile = false })
    sheet1.[0, 1] <- Some({ Expr = Boolean true ; Value = BooleanValue true ; Volatile = false })
    sheet1.[0, 2] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
    sheet1.[0, 3] <- Some({ Expr = Blank ; Value = NullValue ; Volatile = false })
    let sheet2 = QT4.create<CellContent>()
    sheet2.[0, 0] <- Some({ Expr = Float 42.0 ; Value = FloatValue 42.0 ; Volatile = false })
    sheet2.[0, 1] <- Some({ Expr = Boolean false ; Value = BooleanValue false ; Volatile = false })
    sheet2.[0, 2] <- Some({ Expr = String "Dirty" ; Value = StringValue "Dirty" ; Volatile = false })
    sheet2.[0, 3] <- Some({ Expr = Blank ; Value = NullValue ; Volatile = false })
    let workbook = Map.ofList [ "Sheet1", sheet1 ; "Sheet2", sheet2 ]
    workbook |> makeDirtySet |> should equal (Set.empty<CellContent>)

[<Test>]
let ``makeDirtySet (volatile cells) -> Set of (volatile cells)``() =
    let sheet1 = QT4.create<CellContent>()
    sheet1.[0, 0] <- Some({ Expr = Float 40.0 ; Value = FloatValue 40.0 ; Volatile = false })
    sheet1.[0, 1] <- Some({ Expr = Fun("RAND", []) ; Value = NullValue ; Volatile = true })
    let sheet2 = QT4.create<CellContent>()
    sheet2.[0, 0] <- Some({ Expr = Float 42.0 ; Value = FloatValue 42.0 ; Volatile = false })
    sheet2.[0, 1] <- Some({ Expr = Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true } ; BottomRight = { Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})) ; Value = NullValue ; Volatile = true })
    sheet2.[0, 2] <- Some({ Expr = String "Dirty" ; Value = StringValue "Dirty" ; Volatile = false })
    sheet2.[0, 3] <- Some({ Expr = Ref(Cell({ Sheet = None ; Row = 0 ; RowAbs = false ; Col = 0 ; ColAbs = false })) ; Value = NullValue ; Volatile = true })
    let workbook = Map.ofList [ "Sheet1", sheet1 ; "Sheet2", sheet2 ]
    workbook |> makeDirtySet |> should equal (Set.ofList [ { Sheet = "Sheet1" ; Col = 1 ; Row = 2 } ; { Sheet = "Sheet2" ; Col = 1 ; Row = 2 } ; { Sheet = "Sheet2" ; Col = 1 ; Row = 4 } ])

[<Test>]
let ``recalculate circular -> fail``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Ref(Cell({ Sheet = None ; Row = 0 ; RowAbs = false ; Col = 0 ; ColAbs = false }))
    let computing = HashSet<AbsCell>([ cell ])
    (fun () -> Eval.eval cell expr workbook (HashSet<AbsCell>()) computing |> ignore) |> should throw typeof<System.Exception>
    