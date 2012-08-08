module Program 

open System
open Forecalc.Library
open Forecalc.Library.Ast


let res = Parser.parse "=B1:A2" |> ReferenceResolver.resolveRefs { Sheet = "Sheet1" ; Col = 1 ; Row = 1 }

printfn "%A" res