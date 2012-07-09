module Program 

open Ast
open Forecalc.Library
open System.Text.RegularExpressions

//let cell = { Sheet = "Sheet1" ; Cell = { Row = 3 ; RowAbs = false ; Col = 3 ; ColAbs = false } }
//
//let res = A1SheetRange("Sheet", "A1", "B2") |> Parser.resolveRef cell
//
//printfn "%A" res

let alphaToNumeric (a : string) =
    let array = a.ToUpper().ToCharArray() |> Array.map (fun c -> int c - 64) 
    for c in [ 0 .. array.Length - 1 ] do
        array.[c] <- array.[c] + c * 25
    Array.sum array


let columnFromAlpha (c: string) =
    let rec innerSum index acc list =
        match list with
            | [] -> acc
            | x::xs -> innerSum (index + 1) (acc + x + index * 25) xs
    c.ToUpper().ToCharArray() 
        |> Array.toList 
        |> List.map (fun c -> int c - 64)
        |> innerSum 0 0

printfn "A: %i = %i" (columnFromAlpha "A") (alphaToNumeric "A")
printfn "AP: %i = %i" (columnFromAlpha "AP") (alphaToNumeric "AP")
printfn "Z: %i = %i" (columnFromAlpha "Z") (alphaToNumeric "Z")
printfn "AA: %i = %i" (columnFromAlpha "AA") (alphaToNumeric "AA")
  
let pattern = @"^(\$?)([A-Z]+)(\$?)(\d+)$"
let ref = "A1"

let groups regex str =
    let p = new Regex(regex)
    let g = p.Match(str).Groups
    let a = Array.create<string> (g.Count - 1) ""
    for i in [ 0 .. g.Count - 2 ] do
        a.[i] <- g.[i+1].Value
    a

let g = groups @"^(\$?)([A-Z]+)(\$?)(\d+)$" ref

printfn "%A" g
