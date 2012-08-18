module ParserContantsTests

open NUnit.Framework
open FsUnit
open Forecalc.Library
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
let ``"4,2" -> "4,2``() =
    "4,2" |> parse |> should equal (String "4,2")

[<Test>]
let ``"-4,2" -> "-4,2"``() =
    "-4,2" |> parse |> should equal (String "-4,2")

[<Test>]
let ``"Forty-two" -> Forty-two``() =
    "Forty-two" |> parse |> should equal (String "Forty-two")

[<Test>]
let ``="" -> String ""``() =
    "=\"\"" |> parse |> should equal (String "")

[<Test>]
let ``"\"Forty-two\"" -> "Forty-two"``() =
    "\"Forty-two\"" |> parse |> should equal (String "\"Forty-two\"")

[<Test>]
let ``'Escaped string... -> Escaped string...``() =
    "'Escaped string..." |> parse |> should equal (EscapedString "Escaped string...")

[<Test>]
let ``#DIV/0! -> Error(DivZero)``() =
    "#DIV/0!" |> parse |> should equal (Error DivZero)

[<Test>]
let ``#N/A! -> String("#N/A!")``() =
    "#N/A!" |> parse |> should equal (String "#N/A!")
   
[<Test>]
let ``#NAME? -> Error(Name)``() =
    "#NAME?" |> parse |> should equal (Error Name)
     
[<Test>]
let ``#NULL! -> Error(Null)``() =
    "#NULL!" |> parse |> should equal (Error Error.Null)

[<Test>]
let ``#NUM! -> Error(Number)``() =
    "#NUM!" |> parse |> should equal (Error Number)

[<Test>]
let ``#REF! -> Error(Reference)``() =
    "#REF!" |> parse |> should equal (Error Reference)

[<Test>]
let ``#VALUE! -> Error(Value)``() =
    "#VALUE!" |> parse |> should equal (Error Value)

[<Test>]
let ``#value! -> Error(Value)``() =
    "#value!" |> parse |> should equal (Error Value)

[<Test>]
let ``#Invalid_Error_Message! -> String("#Invalid_Error_Message!")``() =
    "#Invalid_Error_Message!" |> parse |> should equal (String "#Invalid_Error_Message!")

[<Test>]
let ``=#DIV/0! -> Error("#DIV/0!")``() =
    "=#DIV/0!" |> parse |> should equal (Error DivZero)

[<Test>]
let ``=#N/A! -> Error(Parse)``() =
    "=#N/A!" |> parse |> should equal (Error Parse)
   
[<Test>]
let ``=#NAME? -> Error(Name)``() =
    "=#NAME?" |> parse |> should equal (Error Name)
     
[<Test>]
let ``=#NULL! -> Error(Null)``() =
    "=#NULL!" |> parse |> should equal (Error Null)

[<Test>]
let ``=#NUM! -> Error(Number)``() =
    "=#NUM!" |> parse |> should equal (Error Number)

[<Test>]
let ``=#REF! -> Error(Reference)``() =
    "=#REF!" |> parse |> should equal (Error Reference)

[<Test>]
let ``=#VALUE! -> Error(Value)``() =
    "=#VALUE!" |> parse |> should equal (Error Value)

[<Test>]
let ``=#value! -> Error(Value)``() =
    "=#value!" |> parse |> should equal (Error Value)

[<Test>]
let ``=#Invalid_Error_Message! -> Error(Parse)``() =
    "=#Invalid_Error_Message!" |> parse |> should equal (Error Parse)

[<Test>]
let ``=this is not a valid formula -> Error(Parse)``() =
    "=this is not a valid formula" |> parse |> should equal (Error Parse)

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