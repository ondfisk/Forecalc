module ParserContantsTests

open NUnit.Framework
open FsUnit
open Forecalc.Library.Ast
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
let ``#DIV/0! -> Error("#DIV/0!")``() =
    "#DIV/0!" |> parse |> should equal (Error "#DIV/0!")

[<Test>]
let ``#N/A! -> Error("#N/A!")``() =
    "#N/A!" |> parse |> should equal (Error "#N/A!")
   
[<Test>]
let ``#NAME? -> Error("#NAME?")``() =
    "#NAME?" |> parse |> should equal (Error "#NAME?")
     
[<Test>]
let ``#NULL! -> Error("#NULL!")``() =
    "#NULL!" |> parse |> should equal (Error "#NULL!")

[<Test>]
let ``#NUM! -> Error("#NUM!")``() =
    "#NUM!" |> parse |> should equal (Error "#NUM!")

[<Test>]
let ``#REF! -> Error("#REF!")``() =
    "#REF!" |> parse |> should equal (Error "#REF!")

[<Test>]
let ``#VALUE! -> Error("#VALUE!")``() =
    "#VALUE!" |> parse |> should equal (Error "#VALUE!")

[<Test>]
let ``#value! -> Error("#VALUE!")``() =
    "#value!" |> parse |> should equal (Error "#VALUE!")

[<Test>]
let ``#Invalid_Error_Message! -> String("#Invalid_Error_Message!")``() =
    "#Invalid_Error_Message!" |> parse |> should equal (String "#Invalid_Error_Message!")

[<Test>]
let ``=#DIV/0! -> Error("#DIV/0!")``() =
    "=#DIV/0!" |> parse |> should equal (Error "#DIV/0!")

[<Test>]
let ``=#N/A! -> Error("#N/A!")``() =
    "=#N/A!" |> parse |> should equal (Error "#N/A!")
   
[<Test>]
let ``=#NAME? -> Error("#NAME?")``() =
    "=#NAME?" |> parse |> should equal (Error "#NAME?")
     
[<Test>]
let ``=#NULL! -> Error("#NULL!")``() =
    "=#NULL!" |> parse |> should equal (Error "#NULL!")

[<Test>]
let ``=#NUM! -> Error("#NUM!")``() =
    "=#NUM!" |> parse |> should equal (Error "#NUM!")

[<Test>]
let ``=#REF! -> Error("#REF!")``() =
    "=#REF!" |> parse |> should equal (Error "#REF!")

[<Test>]
let ``=#VALUE! -> Error("#VALUE!")``() =
    "=#VALUE!" |> parse |> should equal (Error "#VALUE!")

[<Test>]
let ``=#value! -> Error("#VALUE!")``() =
    "=#value!" |> parse |> should equal (Error "#VALUE!")

[<Test>]
let ``=#Invalid_Error_Message! -> Error("#NAME?")``() =
    "=#Invalid_Error_Message!" |> parse |> should equal (Error "#NAME?")

[<Test>]
let ``=this is not a valid formula -> Error("#PARSE!")``() =
    "=this is not a valid formula" |> parse |> should equal (Error "#PARSE!")

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