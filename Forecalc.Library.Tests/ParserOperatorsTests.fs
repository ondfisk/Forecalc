module ParserOperatorsTests

open NUnit.Framework
open FsUnit
open Forecalc.Library.Ast
open Forecalc.Library.Parser

[<Test>]
let ``"=42+1" -> =42+1``() =
    "=41+1" |> parse |> should equal (Add(Float 41.0, Float 1.0))

[<Test>]
let ``"=43-1" -> =43-1``() =
    "=43-1" |> parse |> should equal (Sub(Float 43.0, Float 1.0))

[<Test>]
let ``"=21*2" -> 21*2``() =
    "=21*2" |> parse |> should equal (Mul(Float 21.0, Float 2.0))

[<Test>]
let ``"=84/2" -> 84/2``() =
    "=84/2" |> parse |> should equal (Div(Float 84.0, Float 2.0))

[<Test>]
let ``"=6.4807407^2" -> 6.4807407^2``() =
    "=6.4807407^2" |> parse |> should equal (Pow(Float 6.4807407, Float 2.0))

[<Test>]
let ``"=-42" -> -42.0``() =
    "=-42" |> parse |> should equal (Negate(Float 42.0))

[<Test>]
let ``"=-42.0*-1.0" -> -42.0*-1.0``() =
    "=-42.0*-1.0" |> parse |> should equal (Negate(Mul(Float 42.0, Negate(Float 1.0))))

[<Test>]
let ``"=2+20*2" -> 2.0+20.0*2.0``() =
    "=2+20*2"|> parse |> should equal (Add(Float 2.0, Mul(Float 20.0, Float 2.0)))

[<Test>]
let ``"=20*2+2" -> 20.0*2.0+2.0``() =
    "=20*2+2"|> parse |> should equal (Add(Mul(Float 20.0, Float 2.0), Float 2.0))

[<Test>]
let ``"=2*(10+1)" -> 2.0*(10.0+1.0)``() =
    "=2*(10+1)"|> parse |> should equal (Mul(Float 2.0, Add(Float 10.0, Float 1.0)))

[<Test>]
let ``"=4&2" -> Concat(4.0, 2.0)``() =
    "=4&2" |> parse |> should equal (Concat(Float 4.0, Float 2.0))

[<Test>]
let ``"=42=42" -> Eq(42.0, 42.0)``() =
    "=42=42" |> parse |> should equal (Eq(Float 42.0, Float 42.0))
    
[<Test>]
let ``"=42<>42" -> NotEq(42.0, 42.0)``() =
    "=42<>42" |> parse |> should equal (NotEq(Float 42.0, Float 42.0))

[<Test>]
let ``"=42<42" -> Lt(42.0, 42.0)``() =
    "=42<42" |> parse |> should equal (Lt(Float 42.0, Float 42.0))

[<Test>]
let ``"=42<=42" -> Lte(42.0, 42.0)``() =
    "=42<=42" |> parse |> should equal (Lte(Float 42.0, Float 42.0))

[<Test>]
let ``"=42>42" -> Gt(42.0, 42.0)``() =
    "=42>42" |> parse |> should equal (Gt(Float 42.0, Float 42.0))

[<Test>]
let ``"=42>=42" -> Gte(42.0, 42.0)``() =
    "=42>=42" |> parse |> should equal (Gte(Float 42.0, Float 42.0))

[<Test>]
let ``"=20+22<>84/2" -> NotEq(Add(20.0, 42.0),Div(84.0, 2.0))``() =
    "=20+22<>84/2" |> parse |> should equal (NotEq(Add(Float 20.0, Float 22.0), Div(Float 84.0, Float 2.0)))

[<Test>]
let ``"=2^3^4" -> Pow(Pow(Float 2.0, Float 3.0), Float 4.0)``() =
    "=2^3^4" |> parse |> should equal (Pow(Pow(Float 2.0, Float 3.0), Float 4.0))