namespace Forecalc.Library

open Forecalc.Library.Ast

type CellValue =
    | StringValue of string
    | BooleanValue of bool
    | FloatValue of float
    | ErrorValue of string 

and CellContent = { Expr : Expr ; Value : CellValue ; Volatile : bool }

module Eval =

    let valueError = ErrorValue("#VALUE!")
    let nameError = ErrorValue("#NAME?")
    let refError = ErrorValue("#REF!")
    let divError = ErrorValue("#DIV/0!")

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

    let rec eval (cell : AbsCell) (expr : Expr) (workbook : QT4.qt4<CellContent>) =
        match expr with
            | Float(value) -> FloatValue(value)
            | Boolean(value) -> BooleanValue(value)
            | String(value) -> StringValue(value)
            | EscapedString(value) -> StringValue(value)
            | Error(value) -> ErrorValue(value)
            | Negate(e) -> 
                match eval cell e workbook with
                    | FloatValue(value) -> FloatValue(value * -1.0)
                    | BooleanValue(true) -> FloatValue(-1.0)
                    | BooleanValue(false) -> FloatValue(0.0)
                    | StringValue(_) -> valueError
                    | ErrorValue(value) -> ErrorValue(value)
            | Eq(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(value), _ -> ErrorValue(value)
                    | _ , ErrorValue(value) -> ErrorValue(value)
                    | v1, v2 -> BooleanValue(v1 = v2)
            | NotEq(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(value), _ -> ErrorValue(value)
                    | _ , ErrorValue(value) -> ErrorValue(value)
                    | v1, v2 -> BooleanValue(v1 <> v2)
            | Lt(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(value), _ -> ErrorValue(value)
                    | _ , ErrorValue(value) -> ErrorValue(value)
                    | StringValue(_), BooleanValue(_) -> BooleanValue(true)
                    | BooleanValue(_), StringValue(_) -> BooleanValue(false)
                    | FloatValue(_), StringValue(_) -> BooleanValue(true)
                    | StringValue(_), FloatValue(_) -> BooleanValue(false)
                    | FloatValue(_), BooleanValue(_) -> BooleanValue(true)
                    | BooleanValue(_), FloatValue(_) -> BooleanValue(false)
                    | BooleanValue(v1), BooleanValue(v2) -> BooleanValue(v1 < v2)
                    | FloatValue(v1), FloatValue(v2) -> BooleanValue(v1 < v2)
                    | StringValue(v1), StringValue(v2) -> failwith "i dunno"
            | UnresolvedRef(_) -> failwith "References must be resolved before calling eval"
            | _ -> FloatValue(0.0)
