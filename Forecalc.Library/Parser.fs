namespace Forecalc.Library

open System
open System.Globalization
open System.Text.RegularExpressions
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

    let alphaToNumeric (a : string) =
        let array = a.ToUpper().ToCharArray() |> Array.map (fun c -> int c - 64) 
        for c in [ 0 .. array.Length - 1 ] do
            array.[c] <- array.[c] + c * 25
        Array.sum array

//    let resolveA1 (ref : string) =
//        let alphaToNumeric (a : string) =
//            a |> Array.sumBy (fun c -> c - 64)
//        let pattern = new Regex(@"^(\$?)([A-Z]+)(\$?)(\d+)$")
//        let groups = pattern.Match(ref).Groups
//        let columnAbsolute = groups.[1] = "$"
//        let rowAbsolute = groups.[3] = "$"
//        let row = int groups.[4]
//        

            

            