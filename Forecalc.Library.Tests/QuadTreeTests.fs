module QuadTreeTests

open System
open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library

let quadtree = QuadTree.create<int> 

[<Test>]
let ``QuadTree.get (-1, 0) should fail``() =
    (fun () -> quadtree |> QuadTree.get (-1, 0) |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.get (0, -1) should fail``() =
    (fun () -> quadtree |> QuadTree.get (0, -1) |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.get (65536, 0) should fail``() =
    (fun () -> quadtree |> QuadTree.get (65536, 0) |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.get (0, 1048576) should fail``() =
    (fun () -> quadtree |> QuadTree.get (0, 1048576) |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``QuadTree.set (-1, 0) 42 should fail``() =
    (fun () -> quadtree |> QuadTree.set (-1, 0) 42 |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.set (0, -1) 42 should fail``() =
    (fun () -> quadtree |> QuadTree.set (0, -1) 42 |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.set (65536, 0) 42 should fail``() =
    (fun () -> quadtree |> QuadTree.set (65536, 0) 42 |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.set (0, 1048576) 42 should fail``() =
    (fun () -> quadtree |> QuadTree.set (0, 1048576) 42 |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``QuadTree.toSeq returns sequence``() =
    quadtree |> QuadTree.set (0, 0) 0
    quadtree |> QuadTree.set (42, 42) 42
    quadtree |> QuadTree.set (1000, 1000) 1000
    quadtree |> QuadTree.set (10000, 1000000) 1000000
    quadtree |> QuadTree.toSeq |> should equal (List.toSeq [ 0 ; 42 ; 1000 ; 1000000 ])

