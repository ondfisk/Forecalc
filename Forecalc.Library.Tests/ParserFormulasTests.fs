module ParserFormulasTests

open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library.Parser

[<Test>]
let ``"=SUM(42)" -> Fun(SUM, [42.0])``() =
    "=SUM(42)" |> parse |> should equal (Fun("SUM", [Float 42.0]))

[<Test>]
let ``"=SUM(20,22)" -> Fun(SUM, [20.0 ; 22.0])``() =
    "=SUM(20,22)" |> parse |> should equal (Fun("SUM", [Float 20.0 ; Float 22.0]))

[<Test>]
let ``"=sum(20,22)" -> Fun(SUM, [20.0 ; 22.0])``() =
    "=sum(20,22)" |> parse |> should equal (Fun("SUM", [Float 20.0 ; Float 22.0]))

[<Test>]
let ``"=RAND()" -> Fun(RAND, [])``() =
    "=RAND()" |> parse |> should equal (Fun("RAND", []))

[<Test>]
let ``"=LOG10(42)" -> Fun(LOG10, 42.0))``() =
    "=LOG10(42)" |> parse |> should equal (Fun("LOG10", [Float 42.0]))