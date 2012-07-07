namespace CoreCalc.Library.Tests

open NUnit.Framework
open FsUnit
open Ast
open CoreCalc.Library

[<TestFixture>] 
type ParserConstantsTests () =
    [<Test>]
    member this.``"42" -> 42``() =
        "42" |> Parser.parse |> should equal (Float 42.0)

    [<Test>]
    member this.``"-42" -> -42``() =
        "-42" |> Parser.parse |> should equal (Float -42.0)

    [<Test>]
    member this.``"42.0" -> 42.0``() =
        "42.0" |> Parser.parse |> should equal (Float 42.0)

    [<Test>]
    member this.``"-42.0" -> -42.0``() =
        "-42.0" |> Parser.parse |> should equal (Float -42.0)

    [<Test>]
    member this.``"Forty-two" -> Forty-two``() =
        "Forty-two" |> Parser.parse |> should equal (String "Forty-two")

    [<Test>]
    member this.``"\"Forty-two\"" -> "Forty-two"``() =
        "\"Forty-two\"" |> Parser.parse |> should equal (String "\"Forty-two\"")

    [<Test>]
    member this.``"=42" -> 42``() =
        "=42" |> Parser.parse |> should equal (Float 42.0)

    [<Test>]
    member this.``"=-42" -> -42``() =
        "=-42" |> Parser.parse |> should equal (Negate(Float 42.0))

    [<Test>]
    member this.``"=42.0" -> 42.0``() =
        "=42.0" |> Parser.parse |> should equal (Float 42.0)

    [<Test>]
    member this.``"=-42.0" -> -42.0``() =
        "=-42.0" |> Parser.parse |> should equal (Negate(Float 42.0))

    [<Test>]
    member this.``"=\"Forty-two\"" -> "Forty-two"``() =
        "=\"Forty-two\"" |> Parser.parse |> should equal (String "Forty-two")

    [<Test>]
    member this.``"=\"This bit is in \"\"quotes\"\"..." -> This bit is in "quotes"...``() =
        "=\"This bit is in \"\"quotes\"\"...\"" |> Parser.parse |> should equal (String "This bit is in \"quotes\"...")

    [<Test>]
    member this.``TRUE -> true``() =
        "TRUE" |> Parser.parse |> should equal (Boolean true)

    [<Test>]
    member this.``true -> true``() =
        "true" |> Parser.parse |> should equal (Boolean true)

    [<Test>]
    member this.``FALSE -> false``() =
        "FALSE" |> Parser.parse |> should equal (Boolean false)
        
    [<Test>]
    member this.``false -> false``() =
        "false" |> Parser.parse |> should equal (Boolean false)

    [<Test>]
    member this.``=TRUE -> true``() =
        "=TRUE" |> Parser.parse |> should equal (Boolean true)

    [<Test>]
    member this.``=true -> true``() =
        "=true" |> Parser.parse |> should equal (Boolean true)

    [<Test>]
    member this.``=FALSE -> false``() =
        "=FALSE" |> Parser.parse |> should equal (Boolean false)
        
    [<Test>]
    member this.``=false -> false``() =
        "=false" |> Parser.parse |> should equal (Boolean false)
