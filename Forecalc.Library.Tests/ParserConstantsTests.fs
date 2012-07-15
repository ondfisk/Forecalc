module ParserContantsTests

open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library.Parser

[<Test>]
let ``"42" -> 42``() =
    "42" |> parse |> should equal (Float 42.0)

[<Test>]
let ``"-42" -> -42``() =
    "-42" |> parse |> should equal (Float -42.0)

[<Test>]
let ``"42.0" -> 42.0``() =
    "42.0" |> parse |> should equal (Float 42.0)

[<Test>]
let ``"-42.0" -> -42.0``() =
    "-42.0" |> parse |> should equal (Float -42.0)

[<Test>]
let ``"Forty-two" -> Forty-two``() =
    "Forty-two" |> parse |> should equal (String "Forty-two")

[<Test>]
let ``"\"Forty-two\"" -> "Forty-two"``() =
    "\"Forty-two\"" |> parse |> should equal (String "\"Forty-two\"")

[<Test>]
let ``'Escaped string... -> Escaped string...``() =
    "'Escaped string..." |> parse |> should equal (EscapedString "Escaped string...")

[<Test>]
let ``"=42" -> 42``() =
    "=42" |> parse |> should equal (Float 42.0)

[<Test>]
let ``"=-42" -> -42``() =
    "=-42" |> parse |> should equal (Negate(Float 42.0))

[<Test>]
let ``"=42.0" -> 42.0``() =
    "=42.0" |> parse |> should equal (Float 42.0)

[<Test>]
let ``"=-42.0" -> -42.0``() =
    "=-42.0" |> parse |> should equal (Negate(Float 42.0))

[<Test>]
let ``"=\"Forty-two\"" -> "Forty-two"``() =
    "=\"Forty-two\"" |> parse |> should equal (String "Forty-two")

[<Test>]
let ``"=\"This bit is in \"\"quotes\"\"..." -> This bit is in "quotes"...``() =
    "=\"This bit is in \"\"quotes\"\"...\"" |> parse |> should equal (String "This bit is in \"quotes\"...")

[<Test>]
let ``TRUE -> true``() =
    "TRUE" |> parse |> should equal (Boolean true)

[<Test>]
let ``true -> true``() =
    "true" |> parse |> should equal (Boolean true)

[<Test>]
let ``FALSE -> false``() =
    "FALSE" |> parse |> should equal (Boolean false)
        
[<Test>]
let ``false -> false``() =
    "false" |> parse |> should equal (Boolean false)

[<Test>]
let ``=TRUE -> true``() =
    "=TRUE" |> parse |> should equal (Boolean true)

[<Test>]
let ``=true -> true``() =
    "=true" |> parse |> should equal (Boolean true)

[<Test>]
let ``=FALSE -> false``() =
    "=FALSE" |> parse |> should equal (Boolean false)
        
[<Test>]
let ``=false -> false``() =
    "=false" |> parse |> should equal (Boolean false)
