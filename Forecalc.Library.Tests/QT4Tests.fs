module QT4Tests

open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.QT4

[<Test>]
let ``[-1, 0] should fail``() =
    let qt4 = create<int>() 
    (fun () -> qt4.[-1, 0] |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``[0, -1] should fail``() =
    let qt4 = create<int>() 
    (fun () -> qt4.[0, -1] |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``[65536, 0] should fail``() =
    let qt4 = create<int>() 
    (fun () -> qt4.[65536, 0] |> ignore) |> should throw typeof<System.Exception>
 
[<Test>]
let ``[0, 1048576] should fail``() =
    let qt4 = create<int>() 
    (fun () -> qt4.[0, 1048576] |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``[-1, 0] 42 should fail``() =
    let qt4 = create<int>() 
    (fun () -> qt4.[-1, 0] <- Some 42) |> should throw typeof<System.Exception>
 
[<Test>]
let ``[0, -1] 42 should fail``() =
    let qt4 = create<int>() 
    (fun () -> qt4.[0, -1] <- Some 42) |> should throw typeof<System.Exception>
 
[<Test>]
let ``[65536, 0] 42 should fail``() =
    let qt4 = create<int>() 
    (fun () -> qt4.[65536, 0] <- Some 42) |> should throw typeof<System.Exception>
 
[<Test>]
let ``[0, 1048576] 42 should fail``() =
    let qt4 = create<int>() 
    (fun () -> qt4.[0, 1048576] <- Some 42) |> should throw typeof<System.Exception>

[<Test>]
let ``apply (*) 2 should multiply elements by 2``() =
    let qt4 = create<int>() 
    qt4.[18, 4323] <- Some 21
    qt4 |> apply ((*) 2)
    qt4.[18, 4323] |> should equal (Some 42)

[<Test>]
let ``iter should call all elements``() =
    let qt4 = create<int>() 
    qt4.[0, 0] <- Some 9
    qt4.[42, 42] <- Some 10
    qt4.[1000, 1000] <- Some 11
    qt4.[10000, 1000000] <- Some 12
    let i = ref 0
    qt4 |> iter (fun x -> i := !i + x)
    !i |> should equal 42

[<Test>]
let ``toSeq returns sequence``() =
    let qt4 = create<int>() 
    qt4.[0, 0] <- Some 9
    qt4.[42, 42] <- Some 10
    qt4.[1000, 1000] <- Some 11
    qt4.[10000, 1000000] <- Some 12
    qt4 |> toSeq |> should equal (List.toSeq [ 9 ; 10 ; 11 ; 12 ])

[<Test>]
let ``set None should reset value``() =
    let qt4 = create<int>()
    qt4.[42, 42] <- Some(42)
    qt4.[42, 42] <- None
    qt4.[42, 42] |> should equal None

[<Test>]
let ``set None on non-existing cell should do nothing``() =
    let qt4 = create<int>()
    qt4.[42, 42] <- None
    qt4.[42, 42] |> should equal None

[<Test>]
let ``map (*) 2 should multiply elements by 2``() =
    let qt4 = create<int>() 
    qt4.[42, 42] <- Some(21)
    let result = qt4 |> map ((*) 2)
    result.[42, 42] |> should equal (Some 42)

[<Test>]
let ``filter (fun x -> x % 2 = 0) should strip odd items``() =
    let qt4 = create<int>()
    qt4.[1, 1] <- Some(1)
    qt4.[42, 42] <- Some(42)
    let result = qt4 |> filter (fun x -> x % 2 = 0)
    result.[1, 1] |> should equal None
    result.[42, 42] |> should equal (Some 42)

[<Test>]
let ``iteri should call all elements with index``() =
    let qt4 = create<int>() 
    qt4.[1512, 18243] <- Some 42
    let res = ref ""
    qt4 |> iteri (fun c r v -> res := sprintf "c: %i, r: %i, v: %i" c r v)
    !res |> should equal "c: 1512, r: 18243, v: 42"

[<Test>]
let ``mapi carries correct index``() =
    let qt4 = create<int>() 
    qt4.[1512, 18243] <- Some(42)
    let result = qt4 |> mapi (fun c r v -> sprintf "c: %i, r: %i, v: %i" c r v)
    result.[1512, 18243] |> should equal (Some "c: 1512, r: 18243, v: 42")

    
[<Test>]
let ``Empty qt4 has length 0``() =
    let qt4 = create<int>()
    qt4.Length |> should equal 0

[<Test>]
let ``3 element length has length 3``() =
    let qt4 = create<int>()
    qt4.[0, 0] <- Some 9
    qt4.[42, 42] <- Some 10
    qt4.[1000, 1000] <- Some 11
    qt4.Length |> should equal 3

[<Test>]
let ``length ignores None elements``() =
    let qt4 = create<int>()
    qt4.[0, 0] <- Some 9
    qt4.[42, 42] <- Some 10
    qt4.[1000, 1000] <- Some 11
    qt4.[42, 42] <- None
    qt4.[1000, 1000] <- None
    qt4.Length |> should equal 1

[<Test>]
let ``Empty qt4 IsEmpty``() =
    let qt4 = create<int>()
    qt4.IsEmpty |> should be True

[<Test>]
let ``Non-empty qt4 not IsEmpty``() =
    let qt4 = create<int>()
    qt4.[42, 42] <- Some 42
    qt4.IsEmpty |> should be False

[<Test>]
let ``rebuild returns a new qt4 of the same size``() =
    let qt4 = create<int>()
    qt4.[42, 42] <- Some 42
    let result = qt4 |> rebuild
    result.Length |> should equal qt4.Length

[<Test>]
let ``isEmpty is the same as qt4.IsEmpty``() =
    let qt4 = create<int>()
    qt4 |> QT4.isEmpty |> should equal qt4.IsEmpty


[<Test>]
let ``length is the same as qt4.Length``() =
    let qt4 = create<int>()
    qt4 |> QT4.length |> should equal qt4.Length

[<Test>]
let ``get(42, 42) calls indexer``() =
    let qt4 = create<int>()
    qt4.[42, 42] <- Some(42)
    qt4 |> get 42 42 |> should equal (Some 42)

[<Test>]
let ``set(42, 42) calls indexer``() =
    let qt4 = create<int>()
    qt4 |> set 42 42 (Some 42)
    qt4.[42, 42] |> should equal (Some 42)

[<Test>]
let ``range (42,42) (42,42) returns elements in range``() =
    let qt4 = create<int>()
    qt4.[1, 1] <- Some(1)
    qt4.[42, 42] <- Some(42)
    let result = qt4 |> range (42,42) (42,42) |> Seq.toList
    result.Length |> should equal 1
    result.Head |> should equal 42