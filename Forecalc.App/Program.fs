module Program 

open System
open Forecalc.Library
open Forecalc.Library.Ast


let workbook = QT4.create<CellContent>()
let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
let expr = Div(Boolean true, Float 0.0238095238095238)
Eval.eval cell expr workbook |> printfn "%A"