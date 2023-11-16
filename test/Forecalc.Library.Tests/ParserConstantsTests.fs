module ParserContantsTests

open Xunit
open FsUnit.Xunit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.ExpressionParser

[<Fact>]
let ``"42" -> 42``() =
    "42" |> parse |> should equal (Float 42.0)

[<Fact>]
let ``"-42" -> -42``() =
    "-42" |> parse |> should equal (Float -42.0)

[<Fact>]
let ``"42.0" -> 42.0``() =
    "42.0" |> parse |> should equal (Float 42.0)

[<Fact>]
let ``"-42.0" -> -42.0``() =
    "-42.0" |> parse |> should equal (Float -42.0)

[<Fact>]
let ``"4,2" -> "4,2``() =
    "4,2" |> parse |> should equal (String "4,2")

[<Fact>]
let ``"-4,2" -> "-4,2"``() =
    "-4,2" |> parse |> should equal (String "-4,2")

[<Fact>]
let ``"Forty-two" -> Forty-two``() =
    "Forty-two" |> parse |> should equal (String "Forty-two")

[<Fact>]
let ``="" -> String ""``() =
    "=\"\"" |> parse |> should equal (String "")

[<Fact>]
let ``"\"Forty-two\"" -> "Forty-two"``() =
    "\"Forty-two\"" |> parse |> should equal (String "\"Forty-two\"")

[<Fact>]
let ``'Escaped string... -> Escaped string...``() =
    "'Escaped string..." |> parse |> should equal (EscapedString "Escaped string...")

[<Fact>]
let ``#DIV/0! -> Error(DivZero)``() =
    "#DIV/0!" |> parse |> should equal (Error DivZero)

[<Fact>]
let ``#N/A! -> String("#N/A!")``() =
    "#N/A!" |> parse |> should equal (String "#N/A!")

[<Fact>]
let ``#NAME? -> Error(Name)``() =
    "#NAME?" |> parse |> should equal (Error Name)

[<Fact>]
let ``#NULL! -> Error(Null)``() =
    "#NULL!" |> parse |> should equal (Error Error.Null)

[<Fact>]
let ``#NUM! -> Error(Number)``() =
    "#NUM!" |> parse |> should equal (Error Number)

[<Fact>]
let ``#REF! -> Error(Reference)``() =
    "#REF!" |> parse |> should equal (Error Reference)

[<Fact>]
let ``#VALUE! -> Error(Value)``() =
    "#VALUE!" |> parse |> should equal (Error Value)

[<Fact>]
let ``#value! -> Error(Value)``() =
    "#value!" |> parse |> should equal (Error Value)

[<Fact>]
let ``#Invalid_Error_Message! -> String("#Invalid_Error_Message!")``() =
    "#Invalid_Error_Message!" |> parse |> should equal (String "#Invalid_Error_Message!")

[<Fact>]
let ``=#DIV/0! -> Error("#DIV/0!")``() =
    "=#DIV/0!" |> parse |> should equal (Error DivZero)

[<Fact>]
let ``=#N/A! -> Error(Parse)``() =
    "=#N/A!" |> parse |> should equal (Error Parse)

[<Fact>]
let ``=#NAME? -> Error(Name)``() =
    "=#NAME?" |> parse |> should equal (Error Name)

[<Fact>]
let ``=#NULL! -> Error(Null)``() =
    "=#NULL!" |> parse |> should equal (Error Null)

[<Fact>]
let ``=#NUM! -> Error(Number)``() =
    "=#NUM!" |> parse |> should equal (Error Number)

[<Fact>]
let ``=#REF! -> Error(Reference)``() =
    "=#REF!" |> parse |> should equal (Error Reference)

[<Fact>]
let ``=#VALUE! -> Error(Value)``() =
    "=#VALUE!" |> parse |> should equal (Error Value)

[<Fact>]
let ``=#value! -> Error(Value)``() =
    "=#value!" |> parse |> should equal (Error Value)

[<Fact>]
let ``=#Invalid_Error_Message! -> Error(Parse)``() =
    "=#Invalid_Error_Message!" |> parse |> should equal (Error Parse)

[<Fact>]
let ``=this is not a valid formula -> Error(Parse)``() =
    "=this is not a valid formula" |> parse |> should equal (Error Parse)

[<Fact>]
let ``"=42" -> 42``() =
    "=42" |> parse |> should equal (Float 42.0)

[<Fact>]
let ``"=-42" -> -42``() =
    "=-42" |> parse |> should equal (Negate(Float 42.0))

[<Fact>]
let ``"=42.0" -> 42.0``() =
    "=42.0" |> parse |> should equal (Float 42.0)

[<Fact>]
let ``"=-42.0" -> -42.0``() =
    "=-42.0" |> parse |> should equal (Negate(Float 42.0))

[<Fact>]
let ``"=\"Forty-two\"" -> "Forty-two"``() =
    "=\"Forty-two\"" |> parse |> should equal (String "Forty-two")

[<Fact>]
let ``"=\"This bit is in \"\"quotes\"\"..." -> This bit is in "quotes"...``() =
    "=\"This bit is in \"\"quotes\"\"...\"" |> parse |> should equal (String "This bit is in \"quotes\"...")

[<Fact>]
let ``TRUE -> true``() =
    "TRUE" |> parse |> should equal (Boolean true)

[<Fact>]
let ``true -> true``() =
    "true" |> parse |> should equal (Boolean true)

[<Fact>]
let ``FALSE -> false``() =
    "FALSE" |> parse |> should equal (Boolean false)

[<Fact>]
let ``false -> false``() =
    "false" |> parse |> should equal (Boolean false)

[<Fact>]
let ``=TRUE -> true``() =
    "=TRUE" |> parse |> should equal (Boolean true)

[<Fact>]
let ``=true -> true``() =
    "=true" |> parse |> should equal (Boolean true)

[<Fact>]
let ``=FALSE -> false``() =
    "=FALSE" |> parse |> should equal (Boolean false)

[<Fact>]
let ``=false -> false``() =
    "=false" |> parse |> should equal (Boolean false)