module Program 

open System
open Forecalc.Library
open Forecalc.Library.Ast


let res = Parser.parse "=#value!"

printfn "%A" res