namespace Forecalc.Library

open System
open Forecalc.Library.Ast

type CellValue =
    | StringValue of string
    | BooleanValue of bool
    | FloatValue of float
    | ErrorValue of string 
    | NullValue

and CellContent = { Expr : Expr ; Value : CellValue ; Volatile : bool }

module Eval =

    let valueError = ErrorValue("#VALUE!")
    let nameError = ErrorValue("#NAME?")
    let numberError = ErrorValue("#NUM!")
    let refError = ErrorValue("#REF!")
    let divZeroError = ErrorValue("#DIV/0!")

    let rec isVolatile expr =
        let isVolatileFun = function
            | "NOW"
            | "RAND" -> true
            | _ -> false
        match expr with
            | Float(_) 
            | Boolean(_) 
            | String(_) 
            | EscapedString(_) 
            | Error(_) -> true
            | Negate(e) -> isVolatile expr
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
            | Pow(e1, e2) -> isVolatile e1 || isVolatile e2
            | UnresolvedRef(ref) -> false
            | Fun(name, list) -> isVolatileFun name || List.exists isVolatile list
            | Ref(_) -> true

    let toString = function
        | StringValue(v) -> v
        | BooleanValue(v) -> v.ToString().ToUpper()
        | FloatValue(v) -> v.ToString()
        | NullValue -> String.Empty
        | ErrorValue(v) -> v

    let toFloat = function
        | true -> 1.0
        | false -> 0.0

    let cellValue (cell : AbsCell) (ref : Cell) (workbook : QT4.qt4<CellContent>) =
        if ref.Sheet.IsSome then failwith "Sheet references are currently not supported"
        let c = 
            match ref.ColAbs with
                | true -> ref.Col - 1
                | false -> cell.Col + ref.Col - 1
        let r = 
            match ref.RowAbs with
                | true -> ref.Row - 1
                | false -> cell.Row + ref.Row - 1
        try
            match workbook.[c, r] with
                | None -> NullValue
                | Some(v) -> v.Value
        with
            | ex -> ErrorValue("#REF!")


    let rec eval (cell : AbsCell) (expr : Expr) (workbook : QT4.qt4<CellContent>) =
        match expr with
            | Float(v) -> FloatValue(v)
            | Boolean(v) -> BooleanValue(v)
            | String(v) -> StringValue(v)
            | EscapedString(v) -> StringValue(v)
            | Error(v) -> ErrorValue(v)
            | Negate(e) -> 
                match eval cell e workbook with
                    | FloatValue(v) -> FloatValue(v * -1.0)
                    | BooleanValue(true) -> FloatValue(-1.0)
                    | BooleanValue(false) -> FloatValue(0.0)
                    | NullValue -> FloatValue(0.0)
                    | StringValue(_) -> valueError
                    | ErrorValue(v) -> ErrorValue(v)
            | Eq(e1, e2) -> 
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | _ , ErrorValue(v) -> ErrorValue(v)
                    | StringValue(v1), StringValue(v2) -> BooleanValue(String.Compare(v1, v2, true) = 0)
                    | StringValue(""), NullValue -> BooleanValue(true)
                    | NullValue, StringValue("") -> BooleanValue(true)                    
                    | FloatValue(0.0), NullValue -> BooleanValue(true)
                    | NullValue, FloatValue(0.0) -> BooleanValue(true)                    
                    | BooleanValue(v), NullValue -> BooleanValue(v = false)
                    | NullValue, BooleanValue(v) -> BooleanValue(false = v)
                    | v1, v2 -> BooleanValue(v1 = v2)
            | NotEq(e1, e2) -> 
                match eval cell (Eq(e1, e2)) workbook with
                    | BooleanValue(v) -> BooleanValue(not v)
                    | v -> v
            | Lt(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | _, ErrorValue(v) -> ErrorValue(v)                    
                    | StringValue(_), BooleanValue(_) -> BooleanValue(true)  // string < bool
                    | BooleanValue(_), StringValue(_) -> BooleanValue(false)
                    | FloatValue(_), StringValue(_) -> BooleanValue(true)    // float < string
                    | StringValue(_), FloatValue(_) -> BooleanValue(false)
                    | FloatValue(_), BooleanValue(_) -> BooleanValue(true)   // float < bool
                    | BooleanValue(_), FloatValue(_) -> BooleanValue(false)
                    | BooleanValue(v1), BooleanValue(v2) -> BooleanValue(v1 < v2)
                    | FloatValue(v1), FloatValue(v2) -> BooleanValue(v1 < v2)
                    | StringValue(v1), StringValue(v2) -> BooleanValue(String.Compare(v1, v2, true) < 0)
                    | NullValue, NullValue -> BooleanValue(false)
                    | NullValue, BooleanValue(v) -> BooleanValue(false < v)
                    | BooleanValue(v), NullValue -> BooleanValue(v < false)
                    | NullValue, FloatValue(v) -> BooleanValue(0.0 < v)
                    | FloatValue(v), NullValue -> BooleanValue(v < 0.0)
                    | NullValue, StringValue(v) -> BooleanValue(String.Compare(String.Empty, v, true) < 0)
                    | StringValue(v), NullValue -> BooleanValue(String.Compare(v, String.Empty, true) < 0)
            | Lte(e1, e2) -> // swap + lt + not
                match eval cell (Lt(e2, e1)) workbook with
                    | BooleanValue(v) -> BooleanValue(not v)
                    | v -> v
            | Gt(e1, e2) -> eval cell (Lt(e2, e1)) workbook // swap + lt
            | Gte(e1, e2) -> // lt + not
                match eval cell (Lt(e1, e2)) workbook with
                    | BooleanValue(v) -> BooleanValue(not v)
                    | v -> v       
            | Concat(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | v1, v2 -> StringValue(String.Concat(toString v1, toString v2))
            | Add(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | StringValue(v), _ -> valueError
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, StringValue(v) -> valueError
                    | NullValue, NullValue -> FloatValue(0.0)
                    | NullValue, FloatValue(v) -> FloatValue(v)
                    | FloatValue(v), NullValue -> FloatValue(v)
                    | NullValue, BooleanValue(v) -> FloatValue(toFloat v)
                    | BooleanValue(v), NullValue -> FloatValue(toFloat v)
                    | FloatValue(v1), FloatValue(v2) -> FloatValue(v1 + v2)
                    | FloatValue(v1), BooleanValue(v2) -> FloatValue(v1 + toFloat v2)
                    | BooleanValue(v1), FloatValue(v2) -> FloatValue(toFloat v1 + v2)
                    | BooleanValue(v1), BooleanValue(v2) -> FloatValue(toFloat v1 + toFloat v2)
            | Sub(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | StringValue(v), _ -> valueError
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, StringValue(v) -> valueError
                    | NullValue, NullValue -> FloatValue(0.0)
                    | NullValue, FloatValue(v) -> FloatValue(-v)
                    | FloatValue(v), NullValue -> FloatValue(v)
                    | NullValue, BooleanValue(v) -> FloatValue(-toFloat v)
                    | BooleanValue(v), NullValue -> FloatValue(toFloat v)
                    | FloatValue(v1), FloatValue(v2) -> FloatValue(v1 - v2)
                    | FloatValue(v1), BooleanValue(v2) -> FloatValue(v1 - toFloat v2)
                    | BooleanValue(v1), FloatValue(v2) -> FloatValue(toFloat v1 - v2)
                    | BooleanValue(v1), BooleanValue(v2) -> FloatValue(toFloat v1 - toFloat v2)
            | Mul(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | StringValue(v), _ -> valueError
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, StringValue(v) -> valueError
                    | NullValue, _ -> FloatValue(0.0)
                    | _, NullValue -> FloatValue(0.0)
                    | FloatValue(v1), FloatValue(v2) -> FloatValue(v1 * v2)
                    | FloatValue(v1), BooleanValue(v2) -> FloatValue(v1 * toFloat v2)
                    | BooleanValue(v1), FloatValue(v2) -> FloatValue(toFloat v1 * v2)
                    | BooleanValue(v1), BooleanValue(v2) -> FloatValue(toFloat v1 * toFloat v2)
            | Div(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | StringValue(v), _ -> valueError
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, StringValue(v) -> valueError
                    | _, FloatValue(0.0) -> divZeroError
                    | _, BooleanValue(false) -> divZeroError
                    | _, NullValue -> divZeroError
                    | NullValue, FloatValue(_) -> FloatValue(0.0)
                    | NullValue, BooleanValue(true) -> FloatValue(0.0)
                    | FloatValue(v1), FloatValue(v2) -> FloatValue(v1 / v2)
                    | FloatValue(v1), BooleanValue(true) -> FloatValue(v1)
                    | BooleanValue(v1), FloatValue(v2) -> FloatValue(toFloat v1 / v2)
                    | BooleanValue(v1), BooleanValue(true) -> FloatValue(toFloat v1)
            | Pow(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | StringValue(v), _ -> valueError
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, StringValue(v) -> valueError
                    | NullValue, NullValue -> numberError
                    | NullValue, FloatValue(v) when v < 0.0 -> divZeroError
                    | NullValue, FloatValue(0.0) -> numberError
                    | NullValue, FloatValue(_) -> FloatValue(0.0)
                    | NullValue, BooleanValue(false) -> numberError
                    | NullValue, BooleanValue(true) -> FloatValue(0.0)
                    | FloatValue(0.0), NullValue -> numberError
                    | FloatValue(_), NullValue -> FloatValue(1.0)
                    | BooleanValue(false), NullValue -> numberError
                    | BooleanValue(true), NullValue -> FloatValue(1.0)
                    | FloatValue(0.0), FloatValue(v) when v < 0.0 -> divZeroError
                    | FloatValue(0.0), FloatValue(0.0) -> numberError
                    | FloatValue(v1), FloatValue(v2) -> FloatValue(v1 ** v2)
                    | FloatValue(0.0), BooleanValue(false) -> numberError
                    | FloatValue(_), BooleanValue(false) -> FloatValue(1.0)
                    | FloatValue(v), BooleanValue(true) -> FloatValue(v)
                    | BooleanValue(false), FloatValue(v) when v < 0.0 -> divZeroError
                    | BooleanValue(false), FloatValue(0.0) -> numberError
                    | BooleanValue(false), FloatValue(_) -> FloatValue(0.0)
                    | BooleanValue(true), FloatValue(_) -> FloatValue(1.0)
                    | BooleanValue(false), BooleanValue(false) -> numberError
                    | BooleanValue(false), BooleanValue(true) -> FloatValue(0.0)
                    | BooleanValue(true), BooleanValue(false) -> FloatValue(1.0)
                    | BooleanValue(true), BooleanValue(true) -> FloatValue(1.0)
            | Ref(e) ->
                match e with
                    | Cell(ref) -> cellValue cell ref workbook
                    | Range(ref) -> FloatValue(0.0)
            | UnresolvedRef(_) -> failwith "References must be resolved before calling eval"
            | Null -> NullValue
            | _ -> FloatValue(0.0)