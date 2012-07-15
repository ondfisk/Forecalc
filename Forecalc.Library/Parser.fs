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
    
    let parse (expr : string) =
        match expr with
            | _ when expr.StartsWith("=") ->
                    let e = expr.Substring 1
                    try
                        let lexbuff = LexBuffer<char>.FromString(e)
                        let expression = ParserSpecification.start LexerSpecification.tokenize lexbuff
                        expression
                    with
                        | ex -> Error ex.Message
            | _ when (Double.TryParse >> fst) expr -> Float (float expr)
            | _ when (bool.TryParse >> fst) expr -> Boolean (bool.Parse expr)
            | _ when expr.StartsWith("'") -> EscapedString(expr.Substring 1)
            | _ -> String expr

            