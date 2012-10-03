module Program 

open System
open System.Collections.Generic
open System.Configuration
open System.Reflection
open System.IO
open System.ComponentModel.Composition
open System.ComponentModel.Composition.Hosting
open Forecalc.Library
open Forecalc.Library.Eval
open Ast

[<EntryPoint>]
let main args =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("COS", [ Float 3.141593 ])
    let res = eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>())
    printfn "%A" (res = FloatValue 1.0)
    0