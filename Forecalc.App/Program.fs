module Program 

open Ast
open Forecalc.Library

let cell = { Sheet = "Sheet1" ; Cell = { Row = 3 ; RowAbs = false ; Col = 3 ; ColAbs = false } }

let res = A1Cell("E5") |> Parser.resolveRef cell

printfn "%A" res