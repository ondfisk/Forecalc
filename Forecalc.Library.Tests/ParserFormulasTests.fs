namespace Forecalc.Library.Tests

open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library.Parser

[<TestFixture>] 
type ParserFormulasTests () =  
    [<Test>]
    member this.``"=SUM(42)" -> Fun(SUM, [42.0])``() =
        "=SUM(42)" |> parse |> should equal (Fun("SUM", [Float 42.0]))

    [<Test>]
    member this.``"=SUM(20,22)" -> Fun(SUM, [20.0 ; 22.0])``() =
        "=SUM(20,22)" |> parse |> should equal (Fun("SUM", [Float 20.0 ; Float 22.0]))

    [<Test>]
    member this.``"=sum(20,22)" -> Fun(SUM, [20.0 ; 22.0])``() =
        "=sum(20,22)" |> parse |> should equal (Fun("SUM", [Float 20.0 ; Float 22.0]))

    [<Test>]
    member this.``"=RAND()" -> Fun(RAND, [])``() =
        "=RAND()" |> parse |> should equal (Fun("RAND", []))

    [<Test>]
    member this.``"=LOG10(42)" -> Fun(LOG10, 42.0))``() =
        "=LOG10(42)" |> parse |> should equal (Fun("LOG10", [Float 42.0]))