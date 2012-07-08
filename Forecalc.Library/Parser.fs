namespace Forecalc.Library

open System
open System.Globalization
open System.Threading
open Microsoft.FSharp.Text.Lexing
open Ast
open LexerSpecification
open ParserSpecification

module Parser =

    Thread.CurrentThread.CurrentCulture <- CultureInfo("en-US")
    Thread.CurrentThread.CurrentUICulture <- CultureInfo("en-US")

    let isFloat s =
        let b, _ = Double.TryParse(s)
        b

    let isBoolean s =
        let b, _ = bool.TryParse(s)
        b

    let toString (s : string) =
        match s with
            | _ when s.StartsWith("'") -> s.Substring 1
            | _ -> s
            
    let isFormula (s : string) =
        s.StartsWith("=")

    let parse (expr : string) =
        match expr with
            | _ when isFormula expr ->
                    let e = expr.Substring 1
                    try
                        let lexbuff = LexBuffer<char>.FromString(e)
                        let expression = ParserSpecification.start LexerSpecification.tokenize lexbuff
                        expression
                    with
                        | ex -> Error ex.Message
            | _ when isFloat expr -> Float (float expr)
            | _ when isBoolean expr -> Boolean (bool.Parse expr)
            | _ -> String (toString expr)
