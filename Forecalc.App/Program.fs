module Program 

open System
open Forecalc.Library
open Forecalc.Library.Ast


//let expr = Parser.parse "=\"Life, \"&\"the \"&\"Universe \"&\"and \"&\"Everything = \"&42.0"


let workbook = QT4.create<CellContent>()
let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
let expr = Concat(Concat(Concat(Concat(Concat(String "Life, ",String "the "),String "Universe "), String "and "),String "Everything = "),Float 42.0)
Eval.eval cell expr workbook |> printfn "%A"