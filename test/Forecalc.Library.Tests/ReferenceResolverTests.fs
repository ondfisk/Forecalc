module ReferenceResolverTests

open System
open Xunit
open FsUnit.Xunit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.ReferenceResolver

// Cell = C3 / ~R3C3
let cell = { Sheet = "Sheet1" ; Row = 3 ; Col = 3 }

[<Fact>]
let ``columnFromAlpha "A" -> 1``() =
    columnFromAlpha "A" |> should equal 1

[<Fact>]
let ``columnFromAlpha "B" -> 2``() =
    columnFromAlpha "B" |> should equal 2

[<Fact>]
let ``columnFromAlpha "Z" -> 26``() =
    columnFromAlpha "Z" |> should equal 26

[<Fact>]
let ``columnFromAlpha "AA" -> 27``() =
    columnFromAlpha "AA" |> should equal 27

[<Fact>]
let ``columnFromAlpha "AP" -> 42``() =
    columnFromAlpha "AP" |> should equal 42

[<Fact>]
let ``columnFromAlpha "RC" -> 471``() =
    columnFromAlpha "RC" |> should equal 471

[<Fact>]
let ``columnFromAlpha "XFD" -> 16384``() =
    columnFromAlpha "XFD" |> should equal 16384

[<Fact>]
let ``A1 -> { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false }``() =
    A1Cell("A1") |> resolveRef cell |> should equal (Cell({ Sheet = None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false }))

[<Fact>]
let ``E5 -> { Row = 2 ; RowAbs = false ; Col = 2 ; ColAbs = false }``() =
    A1Cell("E5") |> resolveRef cell |> should equal (Cell({ Sheet = None ; Row = 2 ; RowAbs = false ; Col = 2 ; ColAbs = false }))

[<Fact>]
let ``$A1 -> { Row = -2 ; RowAbs = false ; Col = 1 ; ColAbs = true }``() =
    A1Cell("$A1") |> resolveRef cell |> should equal (Cell({ Sheet = None ; Row = -2 ; RowAbs = false ; Col = 1 ; ColAbs = true }))
        
[<Fact>]
let ``A$1 -> { Row = 1 ; RowAbs = true ; Col = -2 ; ColAbs = false }``() =
    A1Cell("A$1") |> resolveRef cell |> should equal (Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = -2 ; ColAbs = false }))

[<Fact>]
let ``$A$1 -> { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }``() =
    A1Cell("$A$1") |> resolveRef cell |> should equal (Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))

[<Fact>]
let ``A1:B2 -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }``() =
    A1Range("A1", "B2") |> resolveRef cell |> should equal (Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
        
[<Fact>]
let ``B2:A1 -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }``() =
    A1Range("B2", "A1") |> resolveRef cell |> should equal (Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
      
[<Fact>]
let ``A2:B1 -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }``() =
    A1Range("A2", "B1") |> resolveRef cell |> should equal (Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))

[<Fact>]
let ``B1:A2 -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }``() =
    A1Range("B1", "A2") |> resolveRef cell |> should equal (Range({ Sheet = None ; TopLeft = { Sheet = None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
              
[<Fact>]
let ``Sheet2!A1 -> { Sheet = "Sheet2" ;  Cell = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } }``() =
    A1SheetRef("Sheet2", "A1") |> resolveRef cell |> should equal (Cell({ Sheet = Some "Sheet2" ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false }))

[<Fact>]
let ``Sheet2!A1:B2 -> { Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    A1SheetRange("Sheet2", "A1", "B2") |> resolveRef cell |> should equal (Range({ Sheet = Some "Sheet2" ; TopLeft = { Sheet = Some "Sheet2" ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Some "Sheet2" ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
    
[<Fact>]
let ``Sheet2!B2:A1 -> { Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    A1SheetRange("Sheet2", "B2", "A1") |> resolveRef cell |> should equal (Range({ Sheet = Some "Sheet2" ; TopLeft = { Sheet = Some "Sheet2" ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Some "Sheet2" ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
    
[<Fact>]
let ``A1Cell(R1C1) -> fail``() =
    (fun () -> A1Cell("R1C1") |> resolveRef cell |> ignore) |> should throw typeof<System.Exception>

[<Fact>]
let ``R1C1 -> { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }``() =
    R1C1Cell("R1C1") |> resolveRef cell |> should equal (Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))

[<Fact>]
let ``R[1]C1 -> { Row = 1 ; RowAbs = false ; Col = 1 ; ColAbs = true }``() =
    R1C1Cell("R[1]C1") |> resolveRef cell |> should equal (Cell({ Sheet = None ; Row = 1 ; RowAbs = false ; Col = 1 ; ColAbs = true }))
 
[<Fact>]
let ``R1C[-1] -> { Row = 1 ; RowAbs = true ; Col = -1 ; ColAbs = false }``() =
    R1C1Cell("R1C[-1]") |> resolveRef cell |> should equal (Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = -1 ; ColAbs = false }))
         
[<Fact>]
let ``RC -> { Row = 0 ; RowAbs = false ; Col = 0 ; ColAbs = false }``() =
    R1C1Cell("RC") |> resolveRef cell |> should equal (Cell({ Sheet = None ; Row = 0 ; RowAbs = false ; Col = 0 ; ColAbs = false }))

[<Fact>]
let ``RC[-1] -> { Row = 0 ; RowAbs = false ; Col = -1 ; ColAbs = false }``() =
    R1C1Cell("RC[-1]") |> resolveRef cell |> should equal (Cell({ Sheet = None ; Row = 0 ; RowAbs = false ; Col = -1 ; ColAbs = false }))
        
[<Fact>]
let ``R[+1]C -> { Row = 1 ; RowAbs = false ; Col = 0 ; ColAbs = false }``() =
    R1C1Cell("R[+1]C") |> resolveRef cell |> should equal (Cell({ Sheet = None ; Row = 1 ; RowAbs = false ; Col = 0 ; ColAbs = false }))

[<Fact>]
let ``R[-2]C[-2]:R[-1]C[-1] -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    R1C1Range("R[-2]C[-2]", "R[-1]C[-1]") |> resolveRef cell |> should equal (Range({ Sheet = None ;  TopLeft = { Sheet = None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
        
[<Fact>]
let ``R[-1]C[-1]:R[-2]C[-2] -> { TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    R1C1Range("R[-1]C[-1]", "R[-2]C[-2]") |> resolveRef cell |> should equal (Range({ Sheet = None ;  TopLeft = { Sheet = None ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = None ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
        
[<Fact>]
let ``Sheet2!R1C1 -> { Sheet = "Sheet2" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true } }``() =
    R1C1SheetRef("Sheet2", "R1C1") |> resolveRef cell |> should equal (Cell({ Sheet = Some "Sheet2" ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))

[<Fact>]
let ``Sheet2!R[-2]C[-2]:R[-1]C[-1] -> { Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    R1C1SheetRange("Sheet2", "R[-2]C[-2]", "R[-1]C[-1]") |> resolveRef cell |> should equal (Range({ Sheet = Some "Sheet2" ; TopLeft = { Sheet = Some "Sheet2" ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Some "Sheet2" ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))

[<Fact>]
let ``Sheet2!R[-1]C[-1]:R[-2]C[-2] -> { Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }``() =
    R1C1SheetRange("Sheet2", "R[-1]C[-1]", "R[-2]C[-2]") |> resolveRef cell |> should equal (Range({ Sheet = Some "Sheet2" ; TopLeft = { Sheet = Some "Sheet2" ; Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Sheet = Some "Sheet2" ; Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))

[<Fact>]
let ``R1C1Cell(A1) -> fail``() =
    (fun () -> R1C1Cell("A1") |> resolveRef cell |> ignore) |> should throw typeof<System.Exception>

[<Fact>]
let ``42.0 -> Float 42.0``() =
    Float 42.0 |> resolveRefs cell |> should equal (Float 42.0)

[<Fact>]
let ``true -> Boolean true``() =
    Boolean true |> resolveRefs cell |> should equal (Boolean true)

[<Fact>]
let ``"42" -> String "42"``() =
    String "42" |> resolveRefs cell |> should equal (String "42")

[<Fact>]
let ``"42" -> EscapedString "42"``() =
    EscapedString "42" |> resolveRefs cell |> should equal (EscapedString "42")

[<Fact>]
let ``-R1C1 -> Negate(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })))``() =
    Negate(UnresolvedRef(R1C1Cell("R1C1"))) |> resolveRefs cell |> should equal (Negate(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))))

[<Fact>]
let ``R1C1=R2C2 -> Eq(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Eq(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Eq(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``R1C1<>R2C2 -> NotEq(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    NotEq(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (NotEq(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``R1C1<R2C2 -> Lt(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Lt(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Lt(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``R1C1<=R2C2 -> Lte(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Lte(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Lte(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``R1C1>R2C2 -> Gt(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Gt(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Gt(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``R1C1>=R2C2 -> Gte(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Gte(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Gte(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``R1C1&R2C2 -> Concat(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Concat(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Concat(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``R1C1+R2C2 -> Add(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Add(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Add(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``R1C1-R2C2 -> Sub(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Sub(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Sub(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``R1C1*R2C2 -> Mul(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Mul(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Mul(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``R1C1/R2C2 -> Div(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Div(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Div(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``R1C1^R2C2 -> Pow(Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true })))``() =
    Pow(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveRefs cell |> should equal (Pow(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Ref(Cell({ Sheet = None ; Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }))))

[<Fact>]
let ``Ref -> Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })``() =
    Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })) |> resolveRefs cell |> should equal (Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })))

[<Fact>]
let ``R1C1 -> Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })``() =
    UnresolvedRef(R1C1Cell("R1C1")) |> resolveRefs cell |> should equal (Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })))

[<Fact>]
let ``Sum(42.0, R1C1) -> Fun("Sum", [Float 42.0 ; Ref(Cell({ Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))])``() =
    Fun("Sum", [Float 42.0 ; UnresolvedRef(R1C1Cell("R1C1"))]) |> resolveRefs cell |> should equal (Fun("Sum", [Float 42.0 ; Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))]))

[<Fact>]
let ``Error(Reference) -> Error(Reference)``() =
    Error(Reference) |> resolveRefs cell |> should equal (Error Reference)

[<Fact>]
let ``alphaFromColumn 1 -> "A"``() =
    alphaFromColumn 1 |> should equal "A"

[<Fact>]
let ``alphaFromColumn 2 -> "B"``() =
    alphaFromColumn 2 |> should equal "B"

[<Fact>]
let ``alphaFromColumn 26 -> "Z"``() =
    alphaFromColumn 26 |> should equal "Z"

[<Fact>]
let ``alphaFromColumn 27 -> "AA"``() =
    alphaFromColumn 27 |> should equal "AA"

[<Fact>]
let ``alphaFromColumn 42 -> "AP"``() =
    alphaFromColumn 42 |> should equal "AP"

[<Fact>]
let ``alphaFromColumn 471 -> "RC"``() =
    alphaFromColumn 471 |> should equal "RC"

[<Fact>]
let ``alphaFromColumn 16384 -> "XFD"``() =
    alphaFromColumn 16384 |> should equal "XFD"
