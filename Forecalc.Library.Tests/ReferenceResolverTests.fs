namespace Forecalc.Library.Tests

open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library.ReferenceResolver

[<TestFixture>] 
type ReferenceResolverTests () =
    // Cell = C3 / ~R3C3
    let cell = { Sheet = "Sheet1" ; Cell = { Row = 3 ; RowAbs = false ; Col = 3 ; ColAbs = false } }

    [<Test>]
    member this.``columnFromAlpha "A" -> 1``() =
        columnFromAlpha "A" |> should equal 1

    [<Test>]
    member this.``columnFromAlpha "B" -> 2``() =
        columnFromAlpha "B" |> should equal 2

    [<Test>]
    member this.``columnFromAlpha "Z" -> 26``() =
        columnFromAlpha "Z" |> should equal 26

    [<Test>]
    member this.``columnFromAlpha "AA" -> 27``() =
        columnFromAlpha "AA" |> should equal 27

    [<Test>]
    member this.``columnFromAlpha "AP" -> 42``() =
        columnFromAlpha "AP" |> should equal 42

    [<Test>]
    member this.``A1 -> { Sheet = "Sheet1" ;  Cell = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false }}``() =
        A1Cell("A1") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } }))

    [<Test>]
    member this.``E5 -> { Sheet = "Sheet1" ;  Cell = { Row = 2 ; RowAbs = false ; Col = 2 ; ColAbs = false }}``() =
        A1Cell("E5") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = false ; Col = 2 ; ColAbs = false } }))

    [<Test>]
    member this.``$A1 -> { Sheet = "Sheet1" ;  Cell = { Row = -2 ; RowAbs = false ; Col = 1 ; ColAbs = true }}``() =
        A1Cell("$A1") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = -2 ; RowAbs = false ; Col = 1 ; ColAbs = true } }))
        
    [<Test>]
    member this.``A$1 -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = true ; Col = -2 ; ColAbs = false }}``() =
        A1Cell("A$1") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = -2 ; ColAbs = false } }))

    [<Test>]
    member this.``$A$1 -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }}``() =
        A1Cell("$A$1") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true } }))

    [<Test>]
    member this.``A1:B2 -> { Sheet = "Sheet1" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }}``() =
        A1Range("A1", "B2") |> resolveRef cell |> should equal (RangeRef({ Sheet = "Sheet1" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
        
    [<Test>]
    member this.``Sheet2!A1 -> { Sheet = "Sheet2" ;  Cell = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false }}``() =
        A1SheetRef("Sheet2", "A1") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet2" ; Cell = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } }))

    [<Test>]
    member this.``Sheet2!A1:B2 -> { Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }}``() =
        A1SheetRange("Sheet2", "A1", "B2") |> resolveRef cell |> should equal (RangeRef({ Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))

    [<Test>]
    member this.``R1C1 -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }}``() =
        R1C1Cell("R1C1") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true } }))

    [<Test>]
    member this.``R[1]C1 -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = false ; Col = 1 ; ColAbs = true }}``() =
        R1C1Cell("R[1]C1") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = false ; Col = 1 ; ColAbs = true } }))
 
    [<Test>]
    member this.``R1C[-1] -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = true ; Col = -1 ; ColAbs = false }}``() =
        R1C1Cell("R1C[-1]") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = -1 ; ColAbs = false } }))
         
    [<Test>]
    member this.``RC -> { Sheet = "Sheet1" ;  Cell = { Row = 0 ; RowAbs = false ; Col = 0 ; ColAbs = false }}``() =
        R1C1Cell("RC") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 0 ; RowAbs = false ; Col = 0 ; ColAbs = false } }))

    [<Test>]
    member this.``RC[-1] -> { Sheet = "Sheet1" ;  Cell = { Row = 0 ; RowAbs = false ; Col = -1 ; ColAbs = false }}``() =
        R1C1Cell("RC[-1]") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 0 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
        
    [<Test>]
    member this.``R[+1]C -> { Sheet = "Sheet1" ;  Cell = { Row = 1 ; RowAbs = false ; Col = 0 ; ColAbs = false }}``() =
        R1C1Cell("R[+1]C") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = false ; Col = 0 ; ColAbs = false } }))

    [<Test>]
    member this.``R[-2]C[-2]:R[-1]C[-1] -> { Sheet = "Sheet1" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }}``() =
        R1C1Range("R[-2]C[-2]", "R[-1]C[-1]") |> resolveRef cell |> should equal (RangeRef({ Sheet = "Sheet1" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))
        
    [<Test>]
    member this.``Sheet2!R1C1 -> { Sheet = "Sheet2" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }}``() =
        R1C1SheetRef("Sheet2", "R1C1") |> resolveRef cell |> should equal (CellRef({ Sheet = "Sheet2" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true } }))

    [<Test>]
    member this.``Sheet2!R[-2]C[-2]:R[-1]C[-1] -> { Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false }}``() =
        R1C1SheetRange("Sheet2", "R[-2]C[-2]", "R[-1]C[-1]") |> resolveRef cell |> should equal (RangeRef({ Sheet = "Sheet2" ; TopLeft = { Row = -2 ; RowAbs = false ; Col = -2 ; ColAbs = false } ; BottomRight = { Row = -1 ; RowAbs = false ; Col = -1 ; ColAbs = false } }))

    [<Test>]
    member this.``R1C1 -> Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})``() =
        UnresolvedRef(R1C1Cell("R1C1")) |> resolveExpr cell |> should equal (Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})))

    [<Test>]
    member this.``42.0 -> Float 42.0``() =
        Float 42.0 |> resolveExpr cell |> should equal (Float 42.0)

    [<Test>]
    member this.``-R1C1 -> Negate(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})))``() =
        Negate(UnresolvedRef(R1C1Cell("R1C1"))) |> resolveExpr cell |> should equal (Negate(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1=R2C2 -> Eq(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        Eq(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (Eq(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1*R2C2 -> NotEq(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        NotEq(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (NotEq(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1*R2C2 -> Lt(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        Lt(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (Lt(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1*R2C2 -> Lte(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        Lte(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (Lte(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1*R2C2 -> Gt(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        Gt(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (Gt(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1*R2C2 -> Gte(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        Gte(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (Gte(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1*R2C2 -> Concat(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        Concat(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (Concat(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1*R2C2 -> Add(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        Add(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (Add(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1*R2C2 -> Sub(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        Sub(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (Sub(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1*R2C2 -> Mul(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        Mul(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (Mul(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1*R2C2 -> Div(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        Div(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (Div(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``R1C1*R2C2 -> Pow(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }})))``() =
        Pow(UnresolvedRef(R1C1Cell("R1C1")), UnresolvedRef(R1C1Cell("R2C2"))) |> resolveExpr cell |> should equal (Pow(Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }})), Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 2 ; RowAbs = true ; Col = 2 ; ColAbs = true }}))))

    [<Test>]
    member this.``Sum(42.0, R1C1) -> Fun("Sum", [Float 42.0 ; Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }}))])``() =
        Fun("Sum", [Float 42.0 ; UnresolvedRef(R1C1Cell("R1C1"))]) |> resolveExpr cell |> should equal (Fun("Sum", [Float 42.0 ; Ref(CellRef({ Sheet = "Sheet1" ; Cell = { Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }}))]))

