module QuadTreeTests

open System
open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library

[<Test>]
let ``QuadTree.get (-1, 0) should fail``() =
    let quadtree = QuadTree.create<int>() 
    (fun () -> quadtree.[-1, 0] |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.get (0, -1) should fail``() =
    let quadtree = QuadTree.create<int>() 
    (fun () -> quadtree.[0, -1] |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.get (65536, 0) should fail``() =
    let quadtree = QuadTree.create<int>() 
    (fun () -> quadtree.[65536, 0] |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.get (0, 1048576) should fail``() =
    let quadtree = QuadTree.create<int>() 
    (fun () -> quadtree.[0, 1048576] |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``QuadTree.set (-1, 0) 42 should fail``() =
    let quadtree = QuadTree.create<int>() 
    (fun () -> quadtree.[-1, 0] <- Some 42) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.set (0, -1) 42 should fail``() =
    let quadtree = QuadTree.create<int>() 
    (fun () -> quadtree.[0, -1] <- Some 42) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.set (65536, 0) 42 should fail``() =
    let quadtree = QuadTree.create<int>() 
    (fun () -> quadtree.[65536, 0] <- Some 42) |> should throw typeof<System.Exception>
 
[<Test>]
let ``QuadTree.set (0, 1048576) 42 should fail``() =
    let quadtree = QuadTree.create<int>() 
    (fun () -> quadtree.[0, 1048576] <- Some 42) |> should throw typeof<System.Exception>

[<Test>]
let ``QuadTree.apply (*) 2 should multiply elements by 2``() =
    let quadtree = QuadTree.create<int>() 
    quadtree.[18, 4323] <- Some 21
    quadtree |> QuadTree.apply ((*) 2)
    quadtree.[18, 4323] |> should equal (Some 42)

[<Test>]
let ``QuadTree.iter should call all elements``() =
    let quadtree = QuadTree.create<int>() 
    quadtree.[0, 0] <- Some 9
    quadtree.[42, 42] <- Some 10
    quadtree.[1000, 1000] <- Some 11
    quadtree.[10000, 1000000] <- Some 12
    let i = ref 0
    quadtree |> QuadTree.iter (fun x -> i := !i + x)
    !i |> should equal 42

[<Test>]
let ``QuadTree.toSeq returns sequence``() =
    let quadtree = QuadTree.create<int>() 
    quadtree.[0, 0] <- Some 9
    quadtree.[42, 42] <- Some 10
    quadtree.[1000, 1000] <- Some 11
    quadtree.[10000, 1000000] <- Some 12
    quadtree |> QuadTree.toSeq |> should equal (List.toSeq [ 9 ; 10 ; 11 ; 12 ])

[<Test>]
let ``set None should reset value``() =
    let quadtree = QuadTree.create<int>()
    quadtree.[42, 42] <- Some(42)
    quadtree.[42, 42] <- None
    quadtree.[42, 42] |> should equal None

[<Test>]
let ``set None on non-existing cell should do nothing``() =
    let quadtree = QuadTree.create<int>()
    quadtree.[42, 42] <- None
    quadtree.[42, 42] |> should equal None

[<Test>]
let ``QuadTree.map (*) 2 should multiply elements by 2``() =
    let quadtree = QuadTree.create<int>() 
    quadtree.[42, 42] <- Some(21)
    let result = quadtree |> QuadTree.map ((*) 2)
    result.[42, 42] |> should equal (Some 42)

[<Test>]
let ``QuadTree.filter (fun x -> x % 2 = 0) should strip odd items``() =
    let quadtree = QuadTree.create<int>()
    quadtree.[1, 1] <- Some(1)
    quadtree.[42, 42] <- Some(42)
    let result = quadtree |> QuadTree.filter (fun x -> x % 2 = 0)
    result.[1, 1] |> should equal None
    result.[42, 42] |> should equal (Some 42)

[<Test>]
let ``QuadTree.iteri should call all elements with index``() =
    let quadtree = QuadTree.create<int>() 
    quadtree.[1512, 18243] <- Some 42
    let res = ref ""
    quadtree |> QuadTree.iteri (fun c r v -> res := sprintf "c: %i, r: %i, v: %i" c r v)
    !res |> should equal "c: 1512, r: 18243, v: 42"

[<Test>]
let ``QuadTree.mapi carries correct index``() =
    let quadtree = QuadTree.create<int>() 
    quadtree.[1512, 18243] <- Some(42)
    let result = quadtree |> QuadTree.mapi (fun c r v -> sprintf "c: %i, r: %i, v: %i" c r v)
    result.[1512, 18243] |> should equal (Some "c: 1512, r: 18243, v: 42")

    
[<Test>]
let ``Empty quadtree has length 0``() =
    let quadtree = QuadTree.create<int>()
    quadtree.Length |> should equal 0

[<Test>]
let ``3 element QuadTree.length has length 3``() =
    let quadtree = QuadTree.create<int>()
    quadtree.[0, 0] <- Some 9
    quadtree.[42, 42] <- Some 10
    quadtree.[1000, 1000] <- Some 11
    quadtree.Length |> should equal 3

[<Test>]
let ``Quadtree.length ignores None elements``() =
    let quadtree = QuadTree.create<int>()
    quadtree.[0, 0] <- Some 9
    quadtree.[42, 42] <- Some 10
    quadtree.[1000, 1000] <- Some 11
    quadtree.[42, 42] <- None
    quadtree.[1000, 1000] <- None
    quadtree.Length |> should equal 1

[<Test>]
let ``Empty quadtree IsEmpty``() =
    let quadtree = QuadTree.create<int>()
    quadtree.IsEmpty |> should be True

[<Test>]
let ``Non-empty quadtree not IsEmpty``() =
    let quadtree = QuadTree.create<int>()
    quadtree.[42, 42] <- Some 42
    quadtree.IsEmpty |> should be False