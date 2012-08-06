module Program 

open System
open Ast
open Forecalc.Library


let quadtree = QuadTree.create<int>()
quadtree.[1512, 6423] <- Some(42)

quadtree |> QuadTree.iteri (printfn "%i %i %A")
