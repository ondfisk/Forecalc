module WorkbookTests

open System.Collections.Generic
open Xunit
open FsUnit.Xunit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Workbook

[<Fact>]
let ``makeDirtySet (empty workbook) -> Set.empty<CellContent>``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    workbook |> makeDirtySet |> Assert.Empty

[<Fact>]
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
    workbook |> makeDirtySet |> Assert.Empty

[<Fact>]
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

[<Fact>]
let ``recalculate updates dependent cells``() =
    let sheet1 = QT4.create<CellContent>()
    sheet1.[0, 0] <- Some({ Expr = Float 9.0 ; Value = FloatValue 9.0 ; Volatile = false })
    sheet1.[0, 1] <- Some({ Expr = Float 10.0 ; Value = FloatValue 10.0 ; Volatile = false })
    sheet1.[0, 2] <- Some({ Expr = Float 11.0 ; Value = FloatValue 11.0 ; Volatile = false })
    sheet1.[0, 3] <- Some({ Expr = Fun("SUM", [ Ref(Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = -3 ; RowAbs = false ; Col = 0 ; ColAbs = false } ; BottomRight = { Sheet = None ; Row = -1 ; RowAbs = false ; Col = 0 ; ColAbs = false }})) ]) ; Value = FloatValue 30.0 ; Volatile = true })
    let workbook = Map.ofList [ "Sheet1", sheet1 ]
    let cell = { Sheet = "Sheet1" ; Col = 1 ; Row = 2 }
    let expr = "=2*11"
    recalculate cell expr workbook
    sheet1.[0, 1].Value.Value |> should equal (FloatValue 22.0)
    sheet1.[0, 3].Value.Value |> should equal (FloatValue 42.0)


[<Fact>]
let ``toArray when sheet not existing should fail``() =
    let sheet1 = QT4.create<CellContent>() 
    let workbook = Map.ofList [ "Sheet1", sheet1 ]
    (fun () -> workbook |> Workbook.toArray "Sheet2" (1,1) (1,1) |> ignore) |> should throw typeof<System.Exception>

[<Fact>]
let ``toArray returns a full array of cell``() =
    let sheet1 = QT4.create<CellContent>() 
    let workbook = Map.ofList [ "Sheet1", sheet1 ]
    workbook |> Workbook.toArray "Sheet1" (1,1) (20,100) |> should equal (Array2D.create<CellValue> 20 100 NullValue)