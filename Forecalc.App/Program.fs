module Program 

open System
open Ast
open Forecalc.Library.ReferenceResolver

let columnFromAlphaOld (c : string) =
    let rec inner acc = function
        | [] -> acc + 1
        | head :: tail -> inner ((acc + 1) * 26 + int head - 65) tail
    c.ToUpper().ToCharArray() 
        |> Array.toList 
        |> inner -1

let columnFromAlpha (c : string) =
    c.ToUpper().ToCharArray()
        |> Array.toList
        |> List.rev
        |> List.mapi (fun i c -> (int c - 64) * int (26.0 ** float i))
        |> List.sum


printfn "1:     %A" (columnFromAlpha "A")
printfn "2:     %A" (columnFromAlpha "B")
printfn "26:    %A" (columnFromAlpha "Z")
printfn "27:    %A" (columnFromAlpha "AA")
printfn "42:    %A" (columnFromAlpha "AP")
printfn "471:   %A" (columnFromAlpha "RC")
printfn "16384: %A" (columnFromAlpha "XFD")