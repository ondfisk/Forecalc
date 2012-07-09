module Program 

open Ast
open Forecalc.Library

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