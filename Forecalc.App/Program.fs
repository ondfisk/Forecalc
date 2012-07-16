module Program 

open System
open Ast
open Forecalc.Library.ReferenceResolver

let fixQuotes (s : string) =
    let s' = s.[1..String.length s - 2]
    let s'' = s'.Replace("\"\"", "\"")
    s''

let fixSheet (s : string) =
    let s' = s.[0..String.length s - 2]
    s'

let fixAposSheet (s : string) =
    let s' = s.[1..String.length s - 3]
    s'

let res1 = fixSheet "dingo!"
let res2 = fixAposSheet "'dingo'!"

printfn "%s" res1
printfn "%s" res2

