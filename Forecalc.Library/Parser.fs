namespace Forecalc.Library

open System
open System.Globalization
open System.Threading
open Microsoft.FSharp.Text.Lexing
open Forecalc.Library.Ast
open LexerSpecification
open ParserSpecification

module Parser =

    Thread.CurrentThread.CurrentCulture <- CultureInfo("en-US")
    Thread.CurrentThread.CurrentUICulture <- CultureInfo("en-US")
    
    let parse (expr : string) =
        match expr with
            | _ when expr.StartsWith("=") ->
                    let e = expr.[1..]
                    try
                        let lexbuff = LexBuffer<char>.FromString(e)
                        let expression = ParserSpecification.start LexerSpecification.tokenize lexbuff
                        expression
                    with
                        | ex -> Error(Parse)
            | _ when (Double.TryParse >> fst) expr -> Float (float expr)
            | _ when (bool.TryParse >> fst) expr -> Boolean (bool.Parse expr)
            | _ when expr.StartsWith("#") -> 
                let error = expr.ToUpper()
                match error with
                    | "#DIV/0!" -> Error(DivZero)
                    | "#NAME?" -> Error(Name)
                    | "#NULL!" -> Error(Error.Null)
                    | "#NUM!" -> Error(Number)
                    | "#REF!" -> Error(Reference)
                    | "#VALUE!" -> Error(Value)
                    | _ -> String(expr)
            | _ when expr.StartsWith("'") -> EscapedString(expr.[1..])
            | _ -> String expr

            