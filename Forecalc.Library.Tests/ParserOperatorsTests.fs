namespace Forecalc.Library.Tests

open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library

[<TestFixture>] 
type ParserOperatorsTests () =  
    [<Test>]
    member this.``"=42+1" -> =42+1``() =
        "=41+1" |> Parser.parse |> should equal (Add(Float 41.0, Float 1.0))

    [<Test>]
    member this.``"=43-1" -> =43-1``() =
        "=43-1" |> Parser.parse |> should equal (Sub(Float 43.0, Float 1.0))

    [<Test>]
    member this.``"=21*2" -> 21*2``() =
        "=21*2" |> Parser.parse |> should equal (Mul(Float 21.0, Float 2.0))

    [<Test>]
    member this.``"=84/2" -> 84/2``() =
        "=84/2" |> Parser.parse |> should equal (Div(Float 84.0, Float 2.0))

    [<Test>]
    member this.``"=6.4807407^2" -> 6.4807407^2``() =
        "=6.4807407^2" |> Parser.parse |> should equal (Pow(Float 6.4807407, Float 2.0))

    [<Test>]
    member this.``"=-42" -> -42.0``() =
        "=-42" |> Parser.parse |> should equal (Negate(Float 42.0))

    [<Test>]
    member this.``"=-42.0*-1.0" -> -42.0*-1.0``() =
        "=-42.0*-1.0" |> Parser.parse |> should equal (Negate(Mul(Float 42.0, Negate(Float 1.0))))

    [<Test>]
    member this.``"=2+20*2" -> 2.0+20.0*2.0``() =
        "=2+20*2"|> Parser.parse |> should equal (Add(Float 2.0, Mul(Float 20.0, Float 2.0)))

    [<Test>]
    member this.``"=20*2+2" -> 20.0*2.0+2.0``() =
        "=20*2+2"|> Parser.parse |> should equal (Add(Mul(Float 20.0, Float 2.0), Float 2.0))

    [<Test>]
    member this.``"=2*(10+1)" -> 2.0*(10.0+1.0)``() =
        "=2*(10+1)"|> Parser.parse |> should equal (Mul(Float 2.0, Add(Float 10.0, Float 1.0)))

    [<Test>]
    member this.``"=4&2" -> Concat(4.0, 2.0)``() =
        "=4&2" |> Parser.parse |> should equal (Concat(Float 4.0, Float 2.0))

    [<Test>]
    member this.``"=42=42" -> Eq(42.0, 42.0)``() =
        "=42=42" |> Parser.parse |> should equal (Eq(Float 42.0, Float 42.0))
    
    [<Test>]
    member this.``"=42<>42" -> NotEq(42.0, 42.0)``() =
        "=42<>42" |> Parser.parse |> should equal (NotEq(Float 42.0, Float 42.0))

    [<Test>]
    member this.``"=42<42" -> Lt(42.0, 42.0)``() =
        "=42<42" |> Parser.parse |> should equal (Lt(Float 42.0, Float 42.0))

    [<Test>]
    member this.``"=42<=42" -> Lte(42.0, 42.0)``() =
        "=42<=42" |> Parser.parse |> should equal (Lte(Float 42.0, Float 42.0))

    [<Test>]
    member this.``"=42>42" -> Gt(42.0, 42.0)``() =
        "=42>42" |> Parser.parse |> should equal (Gt(Float 42.0, Float 42.0))

    [<Test>]
    member this.``"=42>=42" -> Gte(42.0, 42.0)``() =
        "=42>=42" |> Parser.parse |> should equal (Gte(Float 42.0, Float 42.0))

    [<Test>]
    member this.``"=20+22<>84/2" -> NotEq(Add(20.0, 42.0),Div(84.0, 2.0))``() =
        "=20+22<>84/2" |> Parser.parse |> should equal (NotEq(Add(Float 20.0, Float 22.0), Div(Float 84.0, Float 2.0)))

    [<Test>]
    member this.``"=2^3^4" -> Pow(Pow(Float 2.0, Float 3.0), Float 4.0)``() =
        "=2^3^4" |> Parser.parse |> should equal (Pow(Pow(Float 2.0, Float 3.0), Float 4.0))