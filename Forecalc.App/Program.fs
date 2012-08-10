module Program 

open System
open Forecalc.Library
open Forecalc.Library.Ast

type Types =
    | A
    | B
    | C
    | List of Types list

let list = [ A ; B ; List([ A ; B ; C ]) ]

let res = 
    list 
    |> List.collect (fun l -> match l with | List(t) -> t | _ -> [l])
    
printfn "List: %A" list 
printfn "Result: %A" res 