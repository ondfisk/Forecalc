namespace Forecalc.Library

open System
open System.Globalization
open System.Threading
open Microsoft.FSharp.Text.Lexing
open Ast
open LexerSpecification
open ParserSpecification

module Eval =

    let rec isConstant expr =
        match expr with
            | Float(_) 
            | Boolean(_) 
            | String(_) 
            | EscapedString(_) 
            | Error(_) -> false
            | Negate(e) -> isConstant expr
            | Eq(e1, e2)
            | NotEq(e1, e2)
            | Lt(e1, e2)
            | Lte(e1, e2)
            | Gt(e1, e2)
            | Gte(e1, e2)
            | Concat(e1, e2)
            | Add(e1, e2)
            | Sub(e1, e2)
            | Mul(e1, e2)
            | Div(e1, e2)
            | Pow(e1, e2) -> isConstant e1 || isConstant e2
            | UnresolvedRef(ref) -> true
            | Fun(name, list) -> List.map isConstant list |> List.exists (fun f -> f)
            | Ref(_) -> true

    let rec eval cell expr workbook =
        Value({ Float = 42.0 ; Boolean = true ; String = "42" ; Constant = true })