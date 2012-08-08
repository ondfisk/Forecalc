module Program 

open System
open Forecalc.Library
open Forecalc.Library.Ast


let res = Parser.parse "=A1:B2" |> ReferenceResolver.resolveRefs { Sheet = "Sheet1" ; Col = 1 ; Row = 1 }

printfn "%A" res