module QuadTreeTests

open System
open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library

[<Test>]
let ``QuadTree.get (-1, 0) should fail``() =
    let quadtree = QuadTree.create<int> 
    (fun () -> quadtree |> QuadTree.get (-1, 0) |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.get (0, -1) should fail``() =
    let quadtree = QuadTree.create<int> 
    (fun () -> quadtree |> QuadTree.get (0, -1) |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.get (65536, 0) should fail``() =
    let quadtree = QuadTree.create<int> 
    (fun () -> quadtree |> QuadTree.get (65536, 0) |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.get (0, 1048576) should fail``() =
    let quadtree = QuadTree.create<int> 
    (fun () -> quadtree |> QuadTree.get (0, 1048576) |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``QuadTree.set (-1, 0) 42 should fail``() =
    let quadtree = QuadTree.create<int> 
    (fun () -> quadtree |> QuadTree.set (-1, 0) (Some 42) |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.set (0, -1) 42 should fail``() =
    let quadtree = QuadTree.create<int> 
    (fun () -> quadtree |> QuadTree.set (0, -1) (Some 42) |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.set (65536, 0) 42 should fail``() =
    let quadtree = QuadTree.create<int> 
    (fun () -> quadtree |> QuadTree.set (65536, 0) (Some 42) |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.set (0, 1048576) 42 should fail``() =
    let quadtree = QuadTree.create<int> 
    (fun () -> quadtree |> QuadTree.set (0, 1048576) (Some 42) |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``QuadTree.mapInPlace (*) 2 should multiply elements by 2``() =
    let quadtree = QuadTree.create<int> 
    quadtree |> QuadTree.set (0, 0) (Some(21))
    quadtree |> QuadTree.mapInPlace ((*) 2)
    quadtree |> QuadTree.get (0, 0) |> should equal (Some 42)

[<Test>]
let ``QuadTree.iter should call all elements``() =
    let quadtree = QuadTree.create<int> 
    quadtree |> QuadTree.set (0, 0) (Some 9)
    quadtree |> QuadTree.set (42, 42) (Some 10)
    quadtree |> QuadTree.set (1000, 1000) (Some 11)
    quadtree |> QuadTree.set (10000, 1000000) (Some 12)
    let i = ref 0
    quadtree |> QuadTree.iter (fun x -> i := !i + x)
    !i |> should equal 42

[<Test>]
let ``QuadTree.toSeq returns sequence``() =
    let quadtree = QuadTree.create<int> 
    quadtree |> QuadTree.set (0, 0) (Some 9)
    quadtree |> QuadTree.set (42, 42) (Some 10)
    quadtree |> QuadTree.set (1000, 1000) (Some 11)
    quadtree |> QuadTree.set (10000, 1000000) (Some 12)
    quadtree |> QuadTree.toSeq |> should equal (List.toSeq [ 9 ; 10 ; 11 ; 12 ])
