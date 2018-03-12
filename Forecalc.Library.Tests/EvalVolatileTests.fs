module EvalVolatileTests

open Xunit
open FsUnit.Xunit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Fact>]
let ``Float is not volatile()``() =
    let expr = Float 42.0
    expr |> isVolatile |> should be False

[<Fact>]
let ``Boolean is not volatile()``() =
    let expr = Boolean true
    expr |> isVolatile |> should be False

[<Fact>]
let ``String is not volatile()``() =
    let expr = String "42.0"
    expr |> isVolatile |> should be False

[<Fact>]
let ``EscapedString is not volatile()``() =
    let expr = EscapedString "42.0"
    expr |> isVolatile |> should be False

[<Fact>]
let ``Error is not volatile()``() =
    let expr = Error(Number)
    expr |> isVolatile |> should be False

[<Fact>]
let ``Negate(-42.0) is not volatile()``() =
    let expr = Negate(Float -42.0)
    expr |> isVolatile |> should be False

[<Fact>]
let ``Negate(R1C1) is volatile``() =
    let expr = Negate(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })))
    expr |> isVolatile |> should be True

[<Fact>]
let ``Eq(42.0, 42.0) is not volatile()``() =
    let expr = Eq(Float 42.0, Float 42.0)
    expr |> isVolatile |> should be False

[<Fact>]
let ``Eq(42.0, R1C1) is volatile``() =
    let expr = Eq(Float 42.0, Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })))
    expr |> isVolatile |> should be True

[<Fact>]
let ``NotEq(42.0, 42.0) is not volatile()``() =
    let expr = NotEq(Float 42.0, Float 42.0)
    expr |> isVolatile |> should be False

[<Fact>]
let ``NotEq(R1C1, true) is volatile``() =
    let expr = Eq(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Boolean true)
    expr |> isVolatile |> should be True

[<Fact>]
let ``Lt(0.0, 42.0) is not volatile()``() =
    let expr = Lt(Float 0.0, Float 42.0)
    expr |> isVolatile |> should be False

[<Fact>]
let ``Lt(R1C1, Error(Number)) is volatile``() =
    let expr = Lt(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Error Number)
    expr |> isVolatile |> should be True

[<Fact>]
let ``Lte("Arthur", "Dent") is not volatile()``() =
    let expr = Lte(String "Arthur", String "Dent")
    expr |> isVolatile |> should be False

[<Fact>]
let ``Lte(R1C1, Boolean false) is volatile``() =
    let expr = Lte(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Boolean false)
    expr |> isVolatile |> should be True

[<Fact>]
let ``Gt(42.0, 42.0) is not volatile()``() =
    let expr = Gt(Float 42.0, Float 42.0)
    expr |> isVolatile |> should be False

[<Fact>]
let ``Gt(42.0, R1C1) is volatile``() =
    let expr = Gt(Float 42.0, Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })))
    expr |> isVolatile |> should be True

[<Fact>]
let ``Gte(42.0, 42.0) is not volatile()``() =
    let expr = Gte(Float 42.0, Float 42.0)
    expr |> isVolatile |> should be False

[<Fact>]
let ``Gte(R1C1, true) is volatile``() =
    let expr = Gte(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Boolean true)
    expr |> isVolatile |> should be True

[<Fact>]
let ``Concat(0.0, 42.0) is not volatile()``() =
    let expr = Concat(Float 0.0, Float 42.0)
    expr |> isVolatile |> should be False

[<Fact>]
let ``Concat(R1C1, Error(Number)) is volatile``() =
    let expr = Concat(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Error Number)
    expr |> isVolatile |> should be True

[<Fact>]
let ``Add("Arthur", "Dent") is not volatile()``() =
    let expr = Add(String "Arthur", String "Dent")
    expr |> isVolatile |> should be False

[<Fact>]
let ``Add(R1C1, Boolean false) is volatile``() =
    let expr = Add(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Boolean false)
    expr |> isVolatile |> should be True

[<Fact>]
let ``Sub(42.0, 42.0) is not volatile()``() =
    let expr = Sub(Float 42.0, Float 42.0)
    expr |> isVolatile |> should be False

[<Fact>]
let ``Sub(42.0, R1C1) is volatile``() =
    let expr = Sub(Float 42.0, Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })))
    expr |> isVolatile |> should be True

[<Fact>]
let ``Mul(42.0, 42.0) is not volatile()``() =
    let expr = Mul(Float 42.0, Float 42.0)
    expr |> isVolatile |> should be False

[<Fact>]
let ``Mul(R1C1, true) is volatile``() =
    let expr = Mul(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Boolean true)
    expr |> isVolatile |> should be True

[<Fact>]
let ``Div(0.0, 42.0) is not volatile()``() =
    let expr = Div(Float 0.0, Float 42.0)
    expr |> isVolatile |> should be False

[<Fact>]
let ``Div(R1C1, Error(Number)) is volatile``() =
    let expr = Div(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Error Number)
    expr |> isVolatile |> should be True

[<Fact>]
let ``Pow("Arthur", "Dent") is not volatile()``() =
    let expr = Pow(String "Arthur", String "Dent")
    expr |> isVolatile |> should be False

[<Fact>]
let ``Pow(R1C1, Boolean false) is volatile``() =
    let expr = Pow(Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true })), Boolean false)
    expr |> isVolatile |> should be True

[<Fact>]
let ``Blank is not volatile``() =
    let expr = Blank
    expr |> isVolatile |> should be False

[<Fact>]
let ``Ref is volatile``() =
    let expr = Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))
    expr |> isVolatile |> should be True

[<Fact>]
let ``IF with no references is not volatile``() =
    let expr = Fun("IF", [ Float 1.0 ; Float 42.0 ; Blank ])
    expr |> isVolatile |> should be False

[<Fact>]
let ``RAND is volatile``() =
    let expr = Fun("RAND", [])
    expr |> isVolatile |> should be True

[<Fact>]
let ``NOW is volatile``() =
    let expr = Fun("NOW", [])
    expr |> isVolatile |> should be True

[<Fact>]
let ``SUM(R1C1) is volatile``() =
    let expr = Fun("SUM", [Ref(Cell({ Sheet = None ; Row = 1 ; RowAbs = true ; Col = 1 ; ColAbs = true }))])
    expr |> isVolatile |> should be True

[<Fact>]
let ``IsVolatile(UnresolvedRef) should fail``() =
    (fun () -> UnresolvedRef(A1Cell("A1")) |> isVolatile |> ignore) |> should throw typeof<System.Exception>