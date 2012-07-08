open System
open System.Text.RegularExpressions

let pattern = new Regex(@"^(\$?)([A-Z]+)(\$?)(\d+)$")
let a1cell = "A1"

let m = pattern.Match(a1cell)

let g = m.Groups

let first = g.[1]

let alphaToNumeric (a : string) =
    let array = a.ToCharArray() |> Array.map (fun c -> int c - 64) 
    for c in [ 0 .. array.Length - 1 ] do
        array.[c] <- array.[c] + c * 25
    Array.sum array