namespace Forecalc.Library

open Forecalc.Library.Ast

type CellValue =
    | StringValue of string
    | BooleanValue of bool
    | FloatValue of float
    | ErrorValue of string 

and CellContent = { Expr : Expr ; Value : CellValue ; Volatile : bool }

module Eval =

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
            | _ -> failwith "Not implemented"
