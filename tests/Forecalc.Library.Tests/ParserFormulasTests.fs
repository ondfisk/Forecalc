module ParserFormulasTests

open Xunit
open FsUnit.Xunit
open Forecalc.Library

[<Fact>]
let ``"=SUM(42)" -> Fun(SUM, [42.0])``() =
    "=SUM(42)" |> Expression.parse |> should equal (Fun("SUM", [Float 42.0]))

[<Fact>]
let ``"=F.DIST(42)" -> Fun(F.DIST, [42.0])``() =
    "=F.DIST(42)" |> Expression.parse |> should equal (Fun("F.DIST", [Float 42.0]))

[<Fact>]
let ``"=SUM(20,22)" -> Fun(SUM, [20.0 ; 22.0])``() =
    "=SUM(20,22)" |> Expression.parse |> should equal (Fun("SUM", [Float 20.0 ; Float 22.0]))

[<Fact>]
let ``"=sum(20,22)" -> Fun(SUM, [20.0 ; 22.0])``() =
    "=sum(20,22)" |> Expression.parse |> should equal (Fun("SUM", [Float 20.0 ; Float 22.0]))

[<Fact>]
let ``"=RAND()" -> Fun(RAND, [])``() =
    "=RAND()" |> Expression.parse |> should equal (Fun("RAND", []))

[<Fact>]
let ``"=LOG10(42)" -> Fun(LOG10, 42.0))``() =
    "=LOG10(42)" |> Expression.parse |> should equal (Fun("LOG10", [Float 42.0]))