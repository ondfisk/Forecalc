module Program 

open System
open Forecalc.Library
open Forecalc.Library.Ast

let worksheet = QT4.create<CellContent>()
worksheet.[1, 0] <- Some({ Expr = Float 1.0 ; Value = FloatValue 1.0 ; Volatile = false })
worksheet.[1, 1] <- Some({ Expr = Float 2.0 ; Value = FloatValue 2.0 ; Volatile = false })
worksheet.[1, 2] <- Some({ Expr = Float 3.0 ; Value = FloatValue 3.0 ; Volatile = false })
let workbook = Map.ofList [ "Sheet1", QT4.create<CellContent>() ; "Sheet2", worksheet ]
let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
let expr = Ref(Range({ Sheet = Some "Sheet2" ; TopLeft = { Sheet = Some "Sheet2" ; Row = 0 ; RowAbs = false ; Col = 1 ; ColAbs = false } ; BottomRight = { Sheet = Some "Sheet2" ; Row = 2 ; RowAbs = false ; Col = 1 ; ColAbs = false }}))
//eval cell expr workbook |> should equal (ValueList([ FloatValue 1.0 ; FloatValue 2.0 ; FloatValue 3.0 ]))

let res = Eval.eval cell expr workbook

printfn "%A" res