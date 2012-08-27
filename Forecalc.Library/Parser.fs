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
        let expr = expr.Trim()
        if expr.StartsWith("=") then
            let e = expr.[1..]
            try
                let lexbuff = LexBuffer<char>.FromString(e)
                let expression = ParserSpecification.start LexerSpecification.tokenize lexbuff
                expression
            with
                | ex -> Error(Parse)
        else if fst (Double.TryParse(expr, NumberStyles.Float, culture)) then
             Float (float expr)
        else if (bool.TryParse >> fst) expr then
            Boolean (bool.Parse expr)
        else if expr.StartsWith("#") then
            match expr.ToUpper() with
                | "#DIV/0!" -> Error(DivZero)
                | "#NAME?" -> Error(Name)
                | "#NULL!" -> Error(Null)
                | "#NUM!" -> Error(Number)
                | "#REF!" -> Error(Reference)
                | "#VALUE!" -> Error(Value)
                | _ -> String(expr)
        else if expr.StartsWith("'") then
            EscapedString(expr.[1..])
        else
            String expr            