module Program 

open System
open Ast
open Forecalc.Library.ReferenceResolver

let columnFromAlpha (c : string) =
    let rec inner acc list =
        match list with
            | [] -> acc + 1
            | x::xs -> inner ((acc + 1) * 26 + int x - 65) xs
    c.ToUpper().ToCharArray() 
        |> Array.toList 
        |> inner -1

let col = "RC"

printfn "%s: %i" col (columnFromAlpha(col)) 