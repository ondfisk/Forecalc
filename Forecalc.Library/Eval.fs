namespace Forecalc.Library

open System
open Forecalc.Library.Ast

type CellValue =
    | StringValue of string
    | BooleanValue of bool
    | FloatValue of float
    | ErrorValue of Error 
    | NullValue
    | ValueList of CellValue list

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
            | Null -> false
            | Ref(_) -> true

    let toString = function
        | StringValue(v) -> v
        | BooleanValue(v) -> v.ToString().ToUpper()
        | FloatValue(v) -> v.ToString()
        | NullValue -> String.Empty
        | ErrorValue(v) ->  
            match v with
                | DivZero -> "#DIV/0!"
                | Name -> "#NAME?"
                | NotAvailable -> "#N/A!"
                | Error.Null -> "#NULL!"
                | Number -> "#NUM!"
                | Parse -> "#PARSE!"
                | Reference -> "#REF!"
                | Value -> "#VALUE!"
        | ValueList(_) -> "#VALUE!"

    let toFloat = function
        | true -> 1.0
        | false -> 0.0

    let cellValue (cell : AbsCell) (ref : Cell) (workbook : QT4.qt4<CellContent>) =
        if ref.Sheet.IsSome then failwith "Sheet references are currently not supported"
        let c = 
            match ref.ColAbs with
                | true -> ref.Col
                | false -> cell.Col + ref.Col
        let r = 
            match ref.RowAbs with
                | true -> ref.Row
                | false -> cell.Row + ref.Row
        if cell.Col = c && cell.Row = r then
            ErrorValue(Reference)
        else
            try
                match workbook.[c - 1, r - 1] with
                    | None -> NullValue
                    | Some(v) -> v.Value
            with
                | ex -> ErrorValue(Reference)

    let cellRange (cell : AbsCell) (ref : Range) (workbook : QT4.qt4<CellContent>) =
        if ref.Sheet.IsSome then failwith "Sheet references are currently not supported"
        let c1 = 
            match ref.TopLeft.ColAbs with
                | true -> ref.TopLeft.Col
                | false -> cell.Col + ref.TopLeft.Col
        let r1 = 
            match ref.TopLeft.RowAbs with
                | true -> ref.TopLeft.Row
                | false -> cell.Row + ref.TopLeft.Row
        let c2 = 
            match ref.BottomRight.ColAbs with
                | true -> ref.BottomRight.Col
                | false -> cell.Col + ref.BottomRight.Col
        let r2 = 
            match ref.BottomRight.RowAbs with
                | true -> ref.BottomRight.Row
                | false -> cell.Row + ref.BottomRight.Row
        if cell.Col >= c1 && cell.Col <= c2 && cell.Row >= r1 && cell.Row <= r2 then
            ErrorValue(Reference)
        else
            try
                workbook 
                    |> QT4.filteri (fun c r v -> c >= c1 - 1 && c < c2 && r >= r1 - 1 && r < r2)
                    |> QT4.toSeq
                    |> Seq.map (fun x -> x.Value)
                    |> Seq.toList
                    |> ValueList
            with
                | ex -> ErrorValue(Reference)

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
                    | StringValue(_) -> ErrorValue(Value)
                    | ErrorValue(v) -> ErrorValue(v)
                    | ValueList(_) -> ErrorValue(Value)
            | Eq(e1, e2) -> 
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | ValueList(_), _ -> ErrorValue(Value)
                    | _ , ErrorValue(v) -> ErrorValue(v)
                    | _, ValueList(_) -> ErrorValue(Value)
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
                    | ValueList(_), _ -> ErrorValue(Value)
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, ValueList(_) -> ErrorValue(Value)                    
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
                    | ValueList(_), _ -> ErrorValue(Value)
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, ValueList(_) -> ErrorValue(Value)
                    | v1, v2 -> StringValue(String.Concat(toString v1, toString v2))
            | Add(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | StringValue(_), _ -> ErrorValue(Value)
                    | ValueList(_), _ -> ErrorValue(Value)
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, StringValue(_) -> ErrorValue(Value)
                    | _, ValueList(_) -> ErrorValue(Value)
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
                    | StringValue(_), _ -> ErrorValue(Value)
                    | ValueList(_), _ -> ErrorValue(Value)
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, StringValue(_) -> ErrorValue(Value)
                    | _, ValueList(_) -> ErrorValue(Value)
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
                    | StringValue(_), _ -> ErrorValue(Value)
                    | ValueList(_), _ -> ErrorValue(Value)
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, StringValue(_) -> ErrorValue(Value)
                    | _, ValueList(_) -> ErrorValue(Value)
                    | NullValue, _ -> FloatValue(0.0)
                    | _, NullValue -> FloatValue(0.0)
                    | FloatValue(v1), FloatValue(v2) -> FloatValue(v1 * v2)
                    | FloatValue(v1), BooleanValue(v2) -> FloatValue(v1 * toFloat v2)
                    | BooleanValue(v1), FloatValue(v2) -> FloatValue(toFloat v1 * v2)
                    | BooleanValue(v1), BooleanValue(v2) -> FloatValue(toFloat v1 * toFloat v2)
            | Div(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | StringValue(_), _ -> ErrorValue(Value)
                    | ValueList(_), _ -> ErrorValue(Value)
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, StringValue(_) -> ErrorValue(Value)
                    | _, ValueList(_) -> ErrorValue(Value)
                    | _, FloatValue(0.0) -> ErrorValue(DivZero)
                    | _, BooleanValue(false) -> ErrorValue(DivZero)
                    | _, NullValue -> ErrorValue(DivZero)
                    | NullValue, FloatValue(_) -> FloatValue(0.0)
                    | NullValue, BooleanValue(true) -> FloatValue(0.0)
                    | FloatValue(v1), FloatValue(v2) -> FloatValue(v1 / v2)
                    | FloatValue(v1), BooleanValue(true) -> FloatValue(v1)
                    | BooleanValue(v1), FloatValue(v2) -> FloatValue(toFloat v1 / v2)
                    | BooleanValue(v1), BooleanValue(true) -> FloatValue(toFloat v1)
            | Pow(e1, e2) ->
                match (eval cell e1 workbook, eval cell e2 workbook) with
                    | ErrorValue(v), _ -> ErrorValue(v)
                    | StringValue(_), _ -> ErrorValue(Value)
                    | ValueList(_), _ -> ErrorValue(Value)
                    | _, ErrorValue(v) -> ErrorValue(v)
                    | _, StringValue(_) -> ErrorValue(Value)
                    | _, ValueList(_) -> ErrorValue(Value)
                    | NullValue, NullValue -> ErrorValue(Number)
                    | NullValue, FloatValue(v) when v < 0.0 -> ErrorValue(DivZero)
                    | NullValue, FloatValue(0.0) -> ErrorValue(Number)
                    | NullValue, FloatValue(_) -> FloatValue(0.0)
                    | NullValue, BooleanValue(false) -> ErrorValue(Number)
                    | NullValue, BooleanValue(true) -> FloatValue(0.0)
                    | FloatValue(0.0), NullValue -> ErrorValue(Number)
                    | FloatValue(_), NullValue -> FloatValue(1.0)
                    | BooleanValue(false), NullValue -> ErrorValue(Number)
                    | BooleanValue(true), NullValue -> FloatValue(1.0)
                    | FloatValue(0.0), FloatValue(v) when v < 0.0 -> ErrorValue(DivZero)
                    | FloatValue(0.0), FloatValue(0.0) -> ErrorValue(Number)
                    | FloatValue(v1), FloatValue(v2) -> FloatValue(v1 ** v2)
                    | FloatValue(0.0), BooleanValue(false) -> ErrorValue(Number)
                    | FloatValue(_), BooleanValue(false) -> FloatValue(1.0)
                    | FloatValue(v), BooleanValue(true) -> FloatValue(v)
                    | BooleanValue(false), FloatValue(v) when v < 0.0 -> ErrorValue(DivZero)
                    | BooleanValue(false), FloatValue(0.0) -> ErrorValue(Number)
                    | BooleanValue(false), FloatValue(_) -> FloatValue(0.0)
                    | BooleanValue(true), FloatValue(_) -> FloatValue(1.0)
                    | BooleanValue(false), BooleanValue(false) -> ErrorValue(Number)
                    | BooleanValue(false), BooleanValue(true) -> FloatValue(0.0)
                    | BooleanValue(true), BooleanValue(false) -> FloatValue(1.0)
                    | BooleanValue(true), BooleanValue(true) -> FloatValue(1.0)
            | Ref(e) ->
                match e with
                    | Cell(ref) -> cellValue cell ref workbook
                    | Range(ref) -> cellRange cell ref workbook
            | UnresolvedRef(_) -> failwith "References must be resolved before calling eval"
            | Null -> NullValue
            | Fun(name, list) -> evalFun name list cell workbook

    and evalFun name list cell workbook =
        match name with
            | "IF" -> 
                match list with
                    | [ e1 ; e2 ; e3 ] ->
                        match eval cell e1 workbook with
                            | NullValue
                            | FloatValue(0.0) 
                            | BooleanValue(false) -> eval cell e3 workbook
                            | FloatValue(_)
                            | BooleanValue(true) -> eval cell e2 workbook
                            | ValueList(_)
                            | StringValue(_) -> ErrorValue(Value)
                            | ErrorValue(v) -> ErrorValue(v)
                    | _ -> ErrorValue(Parse)
            | "SUM" ->
                match list with
                    | [] -> ErrorValue(Parse)
                    | _ ->
                        let refs = 
                            list 
                                |> List.filter (fun e -> match e with | Ref(_) -> true | _ -> false)
                                |> List.map (fun e -> eval cell e workbook)
                                |> List.collect (fun e -> match e with | ValueList(l) -> l | _ -> [e])
                                |> List.filter (fun e -> match e with | BooleanValue(_) | FloatValue(_) | ErrorValue(_) -> true | _ -> false)
                        let vals = 
                            list 
                                |> List.filter (fun e -> match e with | Ref(_) -> false | _ -> true)
                                |> List.map (fun e -> eval cell e workbook)
                        let all = List.append refs vals
                        let error = all |> List.tryFind (fun v -> match v with | ErrorValue(_) -> true | _ -> false)
                        match error with
                            | Some(e) -> e
                            | None ->
                                let str = all |> List.tryFind (fun v -> match v with | StringValue(_) -> true | _ -> false)
                                match str with
                                    | Some(s) -> ErrorValue(Value)
                                    | None ->
                                        all 
                                            |> List.map (fun v -> match v with | BooleanValue(b) -> toFloat b | FloatValue(f) -> f | _ -> 0.0)
                                            |> List.sum
                                            |> FloatValue
            | "COUNT" ->
                match list with
                    | [] -> ErrorValue(Parse)
                    | _ ->
                        list 
                            |> List.map (fun e -> eval cell e workbook)
                            |> List.collect (fun e -> match e with | ValueList(l) -> l | _ -> [e])
                            |> List.filter (fun e -> match e with | FloatValue(_) -> true | _ -> false)
                            |> List.length
                            |> float
                            |> FloatValue
            | _ -> ErrorValue(Parse)