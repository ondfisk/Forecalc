namespace Forecalc.Library.Tests

open NUnit.Framework
open FsUnit
open Ast
open Forecalc.Library.Parser

[<TestFixture>] 
type ParserConstantsTests () =
    [<Test>]
    member this.``"42" -> 42``() =
        "42" |> parse |> should equal (Float 42.0)

    [<Test>]
    member this.``"-42" -> -42``() =
        "-42" |> parse |> should equal (Float -42.0)

    [<Test>]
    member this.``"42.0" -> 42.0``() =
        "42.0" |> parse |> should equal (Float 42.0)

    [<Test>]
    member this.``"-42.0" -> -42.0``() =
        "-42.0" |> parse |> should equal (Float -42.0)

    [<Test>]
    member this.``"Forty-two" -> Forty-two``() =
        "Forty-two" |> parse |> should equal (String "Forty-two")

    [<Test>]
    member this.``"\"Forty-two\"" -> "Forty-two"``() =
        "\"Forty-two\"" |> parse |> should equal (String "\"Forty-two\"")

    [<Test>]
    member this.``"=42" -> 42``() =
        "=42" |> parse |> should equal (Float 42.0)

    [<Test>]
    member this.``"=-42" -> -42``() =
        "=-42" |> parse |> should equal (Negate(Float 42.0))

    [<Test>]
    member this.``"=42.0" -> 42.0``() =
        "=42.0" |> parse |> should equal (Float 42.0)

    [<Test>]
    member this.``"=-42.0" -> -42.0``() =
        "=-42.0" |> parse |> should equal (Negate(Float 42.0))

    [<Test>]
    member this.``"=\"Forty-two\"" -> "Forty-two"``() =
        "=\"Forty-two\"" |> parse |> should equal (String "Forty-two")

    [<Test>]
    member this.``"=\"This bit is in \"\"quotes\"\"..." -> This bit is in "quotes"...``() =
        "=\"This bit is in \"\"quotes\"\"...\"" |> parse |> should equal (String "This bit is in \"quotes\"...")

    [<Test>]
    member this.``TRUE -> true``() =
        "TRUE" |> parse |> should equal (Boolean true)

    [<Test>]
    member this.``true -> true``() =
        "true" |> parse |> should equal (Boolean true)

    [<Test>]
    member this.``FALSE -> false``() =
        "FALSE" |> parse |> should equal (Boolean false)
        
    [<Test>]
    member this.``false -> false``() =
        "false" |> parse |> should equal (Boolean false)

    [<Test>]
    member this.``=TRUE -> true``() =
        "=TRUE" |> parse |> should equal (Boolean true)

    [<Test>]
    member this.``=true -> true``() =
        "=true" |> parse |> should equal (Boolean true)

    [<Test>]
    member this.``=FALSE -> false``() =
        "=FALSE" |> parse |> should equal (Boolean false)
        
    [<Test>]
    member this.``=false -> false``() =
        "=false" |> parse |> should equal (Boolean false)
