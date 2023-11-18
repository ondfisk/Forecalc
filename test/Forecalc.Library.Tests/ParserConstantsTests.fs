module ParserContantsTests

open Xunit
open FsUnit.Xunit
open Forecalc.Library

[<Fact>]
let ``"42" -> 42``() =
    "42" |> Expression.parse |> should equal (Float 42.0)

[<Fact>]
let ``"-42" -> -42``() =
    "-42" |> Expression.parse |> should equal (Float -42.0)

[<Fact>]
let ``"42.0" -> 42.0``() =
    "42.0" |> Expression.parse |> should equal (Float 42.0)

[<Fact>]
let ``"-42.0" -> -42.0``() =
    "-42.0" |> Expression.parse |> should equal (Float -42.0)

[<Fact>]
let ``"4,2" -> "4,2``() =
    "4,2" |> Expression.parse |> should equal (String "4,2")

[<Fact>]
let ``"-4,2" -> "-4,2"``() =
    "-4,2" |> Expression.parse |> should equal (String "-4,2")

[<Fact>]
let ``"Forty-two" -> Forty-two``() =
    "Forty-two" |> Expression.parse |> should equal (String "Forty-two")

[<Fact>]
let ``="" -> String ""``() =
    "=\"\"" |> Expression.parse |> should equal (String "")

[<Fact>]
let ``"\"Forty-two\"" -> "Forty-two"``() =
    "\"Forty-two\"" |> Expression.parse |> should equal (String "\"Forty-two\"")

[<Fact>]
let ``'Escaped string... -> Escaped string...``() =
    "'Escaped string..." |> Expression.parse |> should equal (EscapedString "Escaped string...")

[<Fact>]
let ``#DIV/0! -> Error(DivZero)``() =
    "#DIV/0!" |> Expression.parse |> should equal (Error DivZero)

[<Fact>]
let ``#N/A! -> String("#N/A!")``() =
    "#N/A!" |> Expression.parse |> should equal (String "#N/A!")

[<Fact>]
let ``#NAME? -> Error(Name)``() =
    "#NAME?" |> Expression.parse |> should equal (Error Name)

[<Fact>]
let ``#NULL! -> Error(Null)``() =
    "#NULL!" |> Expression.parse |> should equal (Error Error.Null)

[<Fact>]
let ``#NUM! -> Error(Number)``() =
    "#NUM!" |> Expression.parse |> should equal (Error Number)

[<Fact>]
let ``#REF! -> Error(Reference)``() =
    "#REF!" |> Expression.parse |> should equal (Error Reference)

[<Fact>]
let ``#VALUE! -> Error(Value)``() =
    "#VALUE!" |> Expression.parse |> should equal (Error Value)

[<Fact>]
let ``#value! -> Error(Value)``() =
    "#value!" |> Expression.parse |> should equal (Error Value)

[<Fact>]
let ``#Invalid_Error_Message! -> String("#Invalid_Error_Message!")``() =
    "#Invalid_Error_Message!" |> Expression.parse |> should equal (String "#Invalid_Error_Message!")

[<Fact>]
let ``=#DIV/0! -> Error("#DIV/0!")``() =
    "=#DIV/0!" |> Expression.parse |> should equal (Error DivZero)

[<Fact>]
let ``=#N/A! -> Error(Parse)``() =
    "=#N/A!" |> Expression.parse |> should equal (Error Parse)

[<Fact>]
let ``=#NAME? -> Error(Name)``() =
    "=#NAME?" |> Expression.parse |> should equal (Error Name)

[<Fact>]
let ``=#NULL! -> Error(Null)``() =
    "=#NULL!" |> Expression.parse |> should equal (Error Null)

[<Fact>]
let ``=#NUM! -> Error(Number)``() =
    "=#NUM!" |> Expression.parse |> should equal (Error Number)

[<Fact>]
let ``=#REF! -> Error(Reference)``() =
    "=#REF!" |> Expression.parse |> should equal (Error Reference)

[<Fact>]
let ``=#VALUE! -> Error(Value)``() =
    "=#VALUE!" |> Expression.parse |> should equal (Error Value)

[<Fact>]
let ``=#value! -> Error(Value)``() =
    "=#value!" |> Expression.parse |> should equal (Error Value)

[<Fact>]
let ``=#Invalid_Error_Message! -> Error(Parse)``() =
    "=#Invalid_Error_Message!" |> Expression.parse |> should equal (Error Parse)

[<Fact>]
let ``=this is not a valid formula -> Error(Parse)``() =
    "=this is not a valid formula" |> Expression.parse |> should equal (Error Parse)

[<Fact>]
let ``"=42" -> 42``() =
    "=42" |> Expression.parse |> should equal (Float 42.0)

[<Fact>]
let ``"=-42" -> -42``() =
    "=-42" |> Expression.parse |> should equal (Negate(Float 42.0))

[<Fact>]
let ``"=42.0" -> 42.0``() =
    "=42.0" |> Expression.parse |> should equal (Float 42.0)

[<Fact>]
let ``"=-42.0" -> -42.0``() =
    "=-42.0" |> Expression.parse |> should equal (Negate(Float 42.0))

[<Fact>]
let ``"=\"Forty-two\"" -> "Forty-two"``() =
    "=\"Forty-two\"" |> Expression.parse |> should equal (String "Forty-two")

[<Fact>]
let ``"=\"This bit is in \"\"quotes\"\"..." -> This bit is in "quotes"...``() =
    "=\"This bit is in \"\"quotes\"\"...\"" |> Expression.parse |> should equal (String "This bit is in \"quotes\"...")

[<Fact>]
let ``TRUE -> true``() =
    "TRUE" |> Expression.parse |> should equal (Boolean true)

[<Fact>]
let ``true -> true``() =
    "true" |> Expression.parse |> should equal (Boolean true)

[<Fact>]
let ``FALSE -> false``() =
    "FALSE" |> Expression.parse |> should equal (Boolean false)

[<Fact>]
let ``false -> false``() =
    "false" |> Expression.parse |> should equal (Boolean false)

[<Fact>]
let ``=TRUE -> true``() =
    "=TRUE" |> Expression.parse |> should equal (Boolean true)

[<Fact>]
let ``=true -> true``() =
    "=true" |> Expression.parse |> should equal (Boolean true)

[<Fact>]
let ``=FALSE -> false``() =
    "=FALSE" |> Expression.parse |> should equal (Boolean false)

[<Fact>]
let ``=false -> false``() =
    "=false" |> Expression.parse |> should equal (Boolean false)