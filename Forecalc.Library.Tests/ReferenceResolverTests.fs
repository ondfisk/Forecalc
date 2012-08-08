module ReferenceResolverTests

open System
open NUnit.Framework
open FsUnit
open Forecalc.Library.Ast
open Forecalc.Library.ReferenceResolver

// Cell = C3 / ~R3C3
let cell = { Sheet = "Sheet1" ; Row = 3 ; Col = 3 }

[<Test>]
let ``columnFromAlpha "A" -> 1``() =
    columnFromAlpha "A" |> should equal 1

[<Test>]
let ``columnFromAlpha "B" -> 2``() =
    columnFromAlpha "B" |> should equal 2

[<Test>]
let ``columnFromAlpha "Z" -> 26``() =
    columnFromAlpha "Z" |> should equal 26

[<Test>]
let ``columnFromAlpha "AA" -> 27``() =
    columnFromAlpha "AA" |> should equal 27

[<Test>]
let ``columnFromAlpha "AP" -> 42``() =
    columnFromAlpha "AP" |> should equal 42

[<Test>]
let ``columnFromAlpha "RC" -> 471``() =
    columnFromAlpha "RC" |> should equal 471

[<Test>]
let ``columnFromAlpha "XFD" -> 16384``() =
    columnFromAlpha "XFD" |> should equal 16384

[<Test>]
let ``A1 -> { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false }``() =
    A1Cell("A1") |> resolveRef cell |> should equal (Cell({ Sheet = Option.None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false }))

[<Test>]
let ``E5 -> { Row = 2 ; RowAbs = false ; Col = 2 ; ColAbs = false }``() =
    A1Cell("E5") |> resolveRef cell |> should equal (Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = false ; Col = 2 ; ColAbs = false }))

[<Test>]
let ``$A1 -> { Row = -2 ; RowAbs = false ; Col = 1 ; ColAbs = true }``() =
    A1Cell("$A1") |> resolveRef cell |> should equal (Cell({ Sheet = Option.None ; Row = -2 ; RowAbs = false ; Col = 1 ; ColAbs = true }))
        
[<Test>]
let ``A$1 -> { Row = 1 ; RowAbs = true ; Col = -2 ; ColAbs = false }``() =
    A1Cell("A$1") |> resolveRef cell |> should equal (Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = -2 ; ColAbs = false }))

[<Test>]
let ``$A$1 -> { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }``() =
    A1Cell("$A$1") |> resolveRef cell |> should equal (Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))

[<Test>]
let ``A1:B2 -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }``() =
    A1Range("A1", "B2") |> resolveRef cell |> should equal (Range({ Sheet = Option.None ; TopLeft = { Sheet = Option.None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Option.None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
        
[<Test>]
let ``B2:A1 -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }``() =
    A1Range("B2", "A1") |> resolveRef cell |> should equal (Range({ Sheet = Option.None ; TopLeft = { Sheet = Option.None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Option.None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
      
[<Test>]
let ``A2:B1 -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }``() =
    A1Range("A2", "B1") |> resolveRef cell |> should equal (Range({ Sheet = Option.None ; TopLeft = { Sheet = Option.None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Option.None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))

[<Test>]
let ``B1:A2 -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }``() =
    A1Range("B1", "A2") |> resolveRef cell |> should equal (Range({ Sheet = Option.None ; TopLeft = { Sheet = Option.None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Option.None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
              
[<Test>]
let ``Sheet2!A1 -> { Sheet = "Sheet2" ;  Cell = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } }``() =
    A1SheetRef("Sheet2", "A1") |> resolveRef cell |> should equal (Cell({ Sheet = Some "Sheet2" ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false }))

[<Test>]
let ``Sheet2!A1:B2 -> { Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    A1SheetRange("Sheet2", "A1", "B2") |> resolveRef cell |> should equal (Range({ Sheet = Some "Sheet2" ; TopLeft = { Sheet = Some "Sheet2" ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Some "Sheet2" ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
    
[<Test>]
let ``Sheet2!B2:A1 -> { Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    A1SheetRange("Sheet2", "B2", "A1") |> resolveRef cell |> should equal (Range({ Sheet = Some "Sheet2" ; TopLeft = { Sheet = Some "Sheet2" ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Some "Sheet2" ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
    
[<Test>]
let ``A1Cell(R1C1) -> fail``() =
    (fun () -> A1Cell("R1C1") |> resolveRef cell |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``R1C1 -> { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }``() =
    R1C1Cell("R1C1") |> resolveRef cell |> should equal (Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))

[<Test>]
let ``R[1]C1 -> { Row = 1 ; RowAbs = false ; Col = 1 ; ColAbs = true }``() =
    R1C1Cell("R[1]C1") |> resolveRef cell |> should equal (Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = false ; Col = 1 ; ColAbs = true }))
 
[<Test>]
let ``R1C[-1] -> { Row = 1 ; RowAbs = true ; Col = -1 ; ColAbs = false }``() =
    R1C1Cell("R1C[-1]") |> resolveRef cell |> should equal (Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = -1 ; ColAbs = false }))
         
[<Test>]
let ``RC -> { Row = 0 ; RowAbs = false ; Col = 0 ; ColAbs = false }``() =
    R1C1Cell("RC") |> resolveRef cell |> should equal (Cell({ Sheet = Option.None ; Row = 0 ; RowAbs = false ; Col = 0 ; ColAbs = false }))

[<Test>]
let ``RC[-1] -> { Row = 0 ; RowAbs = false ; Col = -1 ; ColAbs = false }``() =
    R1C1Cell("RC[-1]") |> resolveRef cell |> should equal (Cell({ Sheet = Option.None ; Row = 0 ; RowAbs = false ; Col = -1 ; ColAbs = false }))
        
[<Test>]
let ``R[+1]C -> { Row = 1 ; RowAbs = false ; Col = 0 ; ColAbs = false }``() =
    R1C1Cell("R[+1]C") |> resolveRef cell |> should equal (Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = false ; Col = 0 ; ColAbs = false }))

[<Test>]
let ``R[-2]C[-2]:R[-1]C[-1] -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    R1C1Range("R[-2]C[-2]", "R[-1]C[-1]") |> resolveRef cell |> should equal (Range({ Sheet = Option.None ;  TopLeft = { Sheet = Option.None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Option.None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
        
[<Test>]
let ``R[-1]C[-1]:R[-2]C[-2] -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    R1C1Range("R[-1]C[-1]", "R[-2]C[-2]") |> resolveRef cell |> should equal (Range({ Sheet = Option.None ;  TopLeft = { Sheet = Option.None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Option.None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
        
[<Test>]
let ``Sheet2!R1C1 -> { Sheet = "Sheet2" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true } }``() =
    R1C1SheetRef("Sheet2", "R1C1") |> resolveRef cell |> should equal (Cell({ Sheet = Some "Sheet2" ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))

[<Test>]
let ``Sheet2!R[-2]C[-2]:R[-1]C[-1] -> { Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    R1C1SheetRange("Sheet2", "R[-2]C[-2]", "R[-1]C[-1]") |> resolveRef cell |> should equal (Range({ Sheet = Some "Sheet2" ; TopLeft = { Sheet = Some "Sheet2" ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Some "Sheet2" ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))

[<Test>]
let ``Sheet2!R[-1]C[-1]:R[-2]C[-2] -> { Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    R1C1SheetRange("Sheet2", "R[-1]C[-1]", "R[-2]C[-2]") |> resolveRef cell |> should equal (Range({ Sheet = Some "Sheet2" ; TopLeft = { Sheet = Some "Sheet2" ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Some "Sheet2" ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))

[<Test>]
let ``R1C1Cell(A1) -> fail``() =
    (fun () -> R1C1Cell("A1") |> resolveRef cell |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``42.0 -> Float 42.0``() =
    Float 42.0 |> resolveRefs cell |> should equal (Float 42.0)

[<Test>]
let ``true -> Boolean true``() =
    Boolean true |> resolveRefs cell |> should equal (Boolean true)

[<Test>]
let ``"42" -> String "42"``() =
    String "42" |> resolveRefs cell |> should equal (String "42")

[<Test>]
let ``"42" -> EscapedString "42"``() =
    EscapedString "42" |> resolveRefs cell |> should equal (EscapedString "42")

[<Test>]
let ``-R1C1 -> Negate(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })))``() =
    Negate(UnresolvedRef(R1C1Cell("R1C1"))) |> resolveRefs cell |> should equal (Negate(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))))

[<Test>]
let ``R1C1=R2C2 -> Eq(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Eq(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Eq(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``R1C1<>R2C2 -> NotEq(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    NotEq(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (NotEq(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``R1C1<R2C2 -> Lt(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Lt(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Lt(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``R1C1<=R2C2 -> Lte(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Lte(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Lte(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``R1C1>R2C2 -> Gt(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Gt(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Gt(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``R1C1>=R2C2 -> Gte(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Gte(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Gte(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``R1C1&R2C2 -> Concat(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Concat(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Concat(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``R1C1+R2C2 -> Add(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Add(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Add(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``R1C1-R2C2 -> Sub(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Sub(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Sub(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``R1C1*R2C2 -> Mul(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Mul(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Mul(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``R1C1/R2C2 -> Div(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Div(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Div(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``R1C1^R2C2 -> Pow(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Pow(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Pow(Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = Option.None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Test>]
let ``Ref -> Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })``() =
    Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })) |> resolveRefs cell |> should equal (Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })))

[<Test>]
let ``R1C1 -> Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })``() =
    UnresolvedRef(R1C1Cell("R1C1")) |> resolveRefs cell |> should equal (Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })))

[<Test>]
let ``Sum(42.0, R1C1) -> Fun("Sum", [Float 42.0 ; Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))])``() =
    Fun("Sum", [Float 42.0 ; UnresolvedRef(R1C1Cell("R1C1"))]) |> resolveRefs cell |> should equal (Fun("Sum", [Float 42.0 ; Ref(Cell({ Sheet = Option.None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))]))

[<Test>]
let ``Error "42" -> Error "42"``() =
    Error "42" |> resolveRefs cell |> should equal (Error "42")
