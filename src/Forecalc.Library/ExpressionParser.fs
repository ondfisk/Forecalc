namespace Forecalc.Library

open System
open System.Globalization
open System.Threading
open FSharp.Text.Lexing
open Ast

module ExpressionParser =

    let culture = CultureInfo("en-US")
    Thread.CurrentThread.CurrentCulture <- culture
    Thread.CurrentThread.CurrentUICulture <- culture

    let parse (expr : string) =
        let expr = expr.Trim()
        if expr.StartsWith("=") then
            let e = expr.[1..]
            try
                let lexbuff = LexBuffer<char>.FromString(e)
                let expression = Parser.start Lexer.tokenize lexbuff
                expression
            with
                | ex -> Error(Parse)
        else if Double.TryParse(expr, NumberStyles.Float, culture) |> fst then
             Float (float expr)
        else if bool.TryParse(expr) |> fst then
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
