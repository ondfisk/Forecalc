namespace Forecalc.Library

open System
open System.Globalization
open System.Threading
open Microsoft.FSharp.Text.Lexing
open Ast
open LexerSpecification
open ParserSpecification

module Parser =

    let culture = CultureInfo("en-US")
    Thread.CurrentThread.CurrentCulture <- culture
    Thread.CurrentThread.CurrentUICulture <- culture
    
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
            | _ when fst (Double.TryParse(expr, NumberStyles.Float, culture)) -> Float (float expr)
            | _ when (bool.TryParse >> fst) expr -> Boolean (bool.Parse expr)
            | _ when expr.StartsWith("#") -> 
                let error = expr.ToUpper()
                match error with
                    | "#DIV/0!" -> Error(DivZero)
                    | "#NAME?" -> Error(Name)
                    | "#NULL!" -> Error(Null)
                    | "#NUM!" -> Error(Number)
                    | "#REF!" -> Error(Reference)
                    | "#VALUE!" -> Error(Value)
                    | _ -> String(expr)
            | _ when expr.StartsWith("'") -> EscapedString(expr.[1..])
            | _ -> String expr

            