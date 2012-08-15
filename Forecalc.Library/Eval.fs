namespace Forecalc.Library

open System
open System.Collections.Generic
open Ast

type CellValue =
    | StringValue of string
    | BooleanValue of bool
    | FloatValue of float
    | ErrorValue of Error 
    | NullValue
    | ValueList of CellValue list

type CellContent = { Expr : Expr ; Value : CellValue ; Volatile : bool }

module Eval =

    let random = new Random()

    let rec eval cell expr (workbook : Map<string, QT4.qt4<CellContent>>) (dirty : HashSet<AbsCell>) (computing : HashSet<AbsCell>) =
        dirty.Remove cell |> ignore
        computing.Add cell |> ignore
        let value =
            match expr with
                | Float v -> FloatValue v
                | Boolean v -> BooleanValue v
                | String v -> StringValue v
                | EscapedString v -> StringValue v
                | Error v -> ErrorValue v
                | Negate(e) -> 
                    match eval cell e workbook dirty computing with
                        | FloatValue v -> FloatValue(v * -1.0)
                        | BooleanValue true -> FloatValue -1.0
                        | BooleanValue false -> FloatValue 0.0
                        | NullValue -> FloatValue 0.0
                        | StringValue _ -> ErrorValue Value
                        | ErrorValue v -> ErrorValue v
                        | ValueList _ -> ErrorValue Value
                | Eq(e1, e2) -> 
                    match eval cell e1 workbook dirty computing, eval cell e2 workbook dirty computing with
                        | ErrorValue v, _ -> ErrorValue v
                        | ValueList _, _ -> ErrorValue Value
                        | _ , ErrorValue v -> ErrorValue v
                        | _, ValueList _ -> ErrorValue Value
                        | StringValue v1, StringValue v2 -> BooleanValue(String.Compare(v1, v2, StringComparison.CurrentCultureIgnoreCase) = 0)
                        | StringValue "", NullValue -> BooleanValue true
                        | NullValue, StringValue "" -> BooleanValue true                    
                        | FloatValue 0.0, NullValue -> BooleanValue true
                        | NullValue, FloatValue 0.0 -> BooleanValue true                    
                        | BooleanValue v, NullValue -> BooleanValue(v = false)
                        | NullValue, BooleanValue v -> BooleanValue(false = v)
                        | v1, v2 -> BooleanValue(v1 = v2)
                | NotEq(e1, e2) -> 
                    match eval cell (Eq(e1, e2)) workbook dirty computing with
                        | BooleanValue v -> BooleanValue(not v)
                        | v -> v
                | Lt(e1, e2) ->
                    match eval cell e1 workbook dirty computing, eval cell e2 workbook dirty computing with
                        | ErrorValue v, _ -> ErrorValue v
                        | ValueList _, _ -> ErrorValue Value
                        | _, ErrorValue v -> ErrorValue v
                        | _, ValueList _ -> ErrorValue Value                 
                        | StringValue _, BooleanValue _ -> BooleanValue(true)  // string < bool
                        | BooleanValue _, StringValue _ -> BooleanValue(false)
                        | FloatValue _, StringValue _ -> BooleanValue(true)    // float < string
                        | StringValue _, FloatValue _ -> BooleanValue(false)
                        | FloatValue _, BooleanValue _ -> BooleanValue(true)   // float < bool
                        | BooleanValue _, FloatValue _ -> BooleanValue(false)
                        | BooleanValue v1, BooleanValue v2 -> BooleanValue(v1 < v2)
                        | FloatValue v1, FloatValue v2 -> BooleanValue(v1 < v2)
                        | StringValue v1, StringValue v2 -> BooleanValue(String.Compare(v1, v2, StringComparison.CurrentCultureIgnoreCase) < 0)
                        | NullValue, NullValue -> BooleanValue(false)
                        | NullValue, BooleanValue v -> BooleanValue(false < v)
                        | BooleanValue v, NullValue -> BooleanValue(v < false)
                        | NullValue, FloatValue v -> BooleanValue(0.0 < v)
                        | FloatValue v, NullValue -> BooleanValue(v < 0.0)
                        | NullValue, StringValue v -> BooleanValue(String.Compare(String.Empty, v, StringComparison.CurrentCultureIgnoreCase) < 0)
                        | StringValue v, NullValue -> BooleanValue(String.Compare(v, String.Empty, StringComparison.CurrentCultureIgnoreCase) < 0)
                | Lte(e1, e2) -> // swap + lt + not
                    match eval cell (Lt(e2, e1)) workbook dirty computing with
                        | BooleanValue v -> BooleanValue(not v)
                        | v -> v
                | Gt(e1, e2) -> eval cell (Lt(e2, e1)) workbook dirty computing // swap + lt
                | Gte(e1, e2) -> // lt + not
                    match eval cell (Lt(e1, e2)) workbook dirty computing with
                        | BooleanValue v -> BooleanValue(not v)
                        | v -> v       
                | Concat(e1, e2) ->
                    match eval cell e1 workbook dirty computing, eval cell e2 workbook dirty computing with
                        | ErrorValue v, _ -> ErrorValue v
                        | ValueList _, _ -> ErrorValue Value
                        | _, ErrorValue v -> ErrorValue v
                        | _, ValueList _ -> ErrorValue Value
                        | v1, v2 -> StringValue(String.Concat(toString v1, toString v2))
                | Add(e1, e2) ->
                    match eval cell e1 workbook dirty computing, eval cell e2 workbook dirty computing with
                        | ErrorValue v, _ -> ErrorValue v
                        | StringValue _, _ -> ErrorValue Value
                        | ValueList _, _ -> ErrorValue Value
                        | _, ErrorValue v -> ErrorValue v
                        | _, StringValue _ -> ErrorValue Value
                        | _, ValueList _ -> ErrorValue(Value)
                        | NullValue, NullValue -> FloatValue(0.0)
                        | NullValue, FloatValue v -> FloatValue v
                        | FloatValue v, NullValue -> FloatValue v
                        | NullValue, BooleanValue v -> FloatValue(toFloat v)
                        | BooleanValue v, NullValue -> FloatValue(toFloat v)
                        | FloatValue v1, FloatValue v2 -> FloatValue(v1 + v2)
                        | FloatValue v1, BooleanValue v2 -> FloatValue(v1 + toFloat v2)
                        | BooleanValue v1, FloatValue v2 -> FloatValue(toFloat v1 + v2)
                        | BooleanValue v1, BooleanValue v2 -> FloatValue(toFloat v1 + toFloat v2)
                | Sub(e1, e2) ->
                    match eval cell e1 workbook dirty computing, eval cell e2 workbook dirty computing with
                        | ErrorValue v, _ -> ErrorValue v
                        | StringValue _, _ -> ErrorValue Value
                        | ValueList _, _ -> ErrorValue Value
                        | _, ErrorValue v -> ErrorValue v
                        | _, StringValue _ -> ErrorValue Value
                        | _, ValueList _ -> ErrorValue Value
                        | NullValue, NullValue -> FloatValue 0.0
                        | NullValue, FloatValue v -> FloatValue -v
                        | FloatValue v, NullValue -> FloatValue v
                        | NullValue, BooleanValue v -> FloatValue(-toFloat v)
                        | BooleanValue v, NullValue -> FloatValue(toFloat v)
                        | FloatValue v1, FloatValue v2 -> FloatValue(v1 - v2)
                        | FloatValue v1, BooleanValue v2 -> FloatValue(v1 - toFloat v2)
                        | BooleanValue v1, FloatValue v2 -> FloatValue(toFloat v1 - v2)
                        | BooleanValue v1, BooleanValue v2 -> FloatValue(toFloat v1 - toFloat v2)
                | Mul(e1, e2) ->
                    match eval cell e1 workbook dirty computing, eval cell e2 workbook dirty computing with
                        | ErrorValue v, _ -> ErrorValue v
                        | StringValue _, _ -> ErrorValue Value
                        | ValueList _, _ -> ErrorValue Value
                        | _, ErrorValue v -> ErrorValue v
                        | _, StringValue _ -> ErrorValue Value
                        | _, ValueList _ -> ErrorValue Value
                        | NullValue, _ -> FloatValue 0.0
                        | _, NullValue -> FloatValue 0.0
                        | FloatValue v1, FloatValue v2 -> FloatValue(v1 * v2)
                        | FloatValue v1, BooleanValue v2 -> FloatValue(v1 * toFloat v2)
                        | BooleanValue v1, FloatValue v2 -> FloatValue(toFloat v1 * v2)
                        | BooleanValue v1, BooleanValue v2 -> FloatValue(toFloat v1 * toFloat v2)
                | Div(e1, e2) ->
                    match eval cell e1 workbook dirty computing, eval cell e2 workbook dirty computing with
                        | ErrorValue v, _ -> ErrorValue v
                        | StringValue _, _ -> ErrorValue Value
                        | ValueList _, _ -> ErrorValue Value
                        | _, ErrorValue v -> ErrorValue v
                        | _, StringValue _ -> ErrorValue Value
                        | _, ValueList _ -> ErrorValue Value
                        | _, FloatValue 0.0 -> ErrorValue DivZero
                        | _, BooleanValue false -> ErrorValue DivZero
                        | _, NullValue -> ErrorValue DivZero
                        | NullValue, FloatValue _ -> FloatValue 0.0
                        | NullValue, BooleanValue true -> FloatValue 0.0
                        | FloatValue v1, FloatValue v2 -> FloatValue(v1 / v2)
                        | FloatValue v1, BooleanValue true -> FloatValue(v1)
                        | BooleanValue v1, FloatValue v2 -> FloatValue(toFloat v1 / v2)
                        | BooleanValue v1, BooleanValue true -> FloatValue(toFloat v1)
                | Pow(e1, e2) ->
                    match eval cell e1 workbook dirty computing, eval cell e2 workbook dirty computing with
                        | ErrorValue v, _ -> ErrorValue v
                        | StringValue _, _ -> ErrorValue Value
                        | ValueList _, _ -> ErrorValue Value
                        | _, ErrorValue v -> ErrorValue v
                        | _, StringValue _ -> ErrorValue Value
                        | _, ValueList _ -> ErrorValue Value
                        | NullValue, NullValue -> ErrorValue Number
                        | NullValue, FloatValue v when v < 0.0 -> ErrorValue DivZero
                        | NullValue, FloatValue 0.0 -> ErrorValue Number
                        | NullValue, FloatValue _ -> FloatValue 0.0
                        | NullValue, BooleanValue false -> ErrorValue Number
                        | NullValue, BooleanValue true -> FloatValue 0.0
                        | FloatValue 0.0, NullValue -> ErrorValue Number
                        | FloatValue _, NullValue -> FloatValue 1.0
                        | BooleanValue false, NullValue -> ErrorValue Number
                        | BooleanValue true, NullValue -> FloatValue 1.0
                        | FloatValue 0.0, FloatValue v when v < 0.0 -> ErrorValue DivZero
                        | FloatValue 0.0, FloatValue 0.0 -> ErrorValue Number
                        | FloatValue v1, FloatValue v2 -> FloatValue(v1 ** v2)
                        | FloatValue 0.0, BooleanValue false -> ErrorValue Number
                        | FloatValue _, BooleanValue false -> FloatValue 1.0
                        | FloatValue v, BooleanValue true -> FloatValue v
                        | BooleanValue false, FloatValue v when v < 0.0 -> ErrorValue DivZero
                        | BooleanValue false, FloatValue(0.0) -> ErrorValue Number
                        | BooleanValue false, FloatValue _ -> FloatValue 0.0
                        | BooleanValue true, FloatValue _ -> FloatValue 1.0
                        | BooleanValue false, BooleanValue false -> ErrorValue Number
                        | BooleanValue false, BooleanValue true -> FloatValue 0.0
                        | BooleanValue true, BooleanValue false -> FloatValue 1.0
                        | BooleanValue true, BooleanValue true -> FloatValue 1.0
                | Ref(e) ->
                    match e with
                        | Cell ref -> cellValue cell ref workbook dirty computing
                        | Range ref -> cellRange cell ref workbook dirty computing
                | UnresolvedRef _ -> failwith "References must be resolved before calling eval"
                | Blank -> NullValue
                | Fun(name, list) -> evalFun name list cell workbook dirty computing
        computing.Remove cell |> ignore
        workbook.[cell.Sheet].[cell.Col - 1, cell.Row - 1] <- Some { Expr = expr ; Value = value ; Volatile = isVolatile expr }
        value

    and toString = function
        | StringValue v -> v
        | BooleanValue v -> v.ToString().ToUpper()
        | FloatValue v -> v.ToString()
        | NullValue -> String.Empty
        | ErrorValue v ->  
            match v with
                | DivZero -> "#DIV/0!"
                | Name -> "#NAME?"
                | NotAvailable -> "#N/A!"
                | Null -> "#NULL!"
                | Number -> "#NUM!"
                | Parse -> "#PARSE!"
                | Reference -> "#REF!"
                | Value -> "#VALUE!"
        | ValueList _ -> "#VALUE!"

    and toFloat = function
        | true -> 1.0
        | false -> 0.0

    and cellValue cell ref workbook dirty computing =
        let sheetName = 
            match ref.Sheet with
                | None -> cell.Sheet
                | Some sheet -> sheet
        match workbook |> Map.containsKey sheetName with
            | false -> ErrorValue Reference
            | true ->
                let worksheet = workbook |> Map.find sheetName
                let c = 
                    match ref.ColAbs with
                        | true -> ref.Col
                        | false -> cell.Col + ref.Col
                let r = 
                    match ref.RowAbs with
                        | true -> ref.Row
                        | false -> cell.Row + ref.Row
                let refCell = { Sheet = sheetName ; Col = c ; Row = r }
                if computing.Contains refCell  then
                    failwithf "Circular reference found in %s!%s%i" sheetName (ReferenceResolver.alphaFromColumn c) r
                try
                    match worksheet.[c - 1, r - 1] with
                        | None -> NullValue
                        | Some v -> 
                            if not v.Volatile && not (dirty.Contains refCell) then
                                v.Value
                            else
                                eval refCell v.Expr workbook dirty computing
                with
                    | ex -> ErrorValue(Reference)

    and cellRange cell ref workbook dirty computing =
        let sheetName = 
            match ref.Sheet with
                | None -> cell.Sheet
                | Some sheet -> sheet
        match workbook |> Map.containsKey sheetName with
            | false -> ErrorValue Reference
            | true ->
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
                seq {
                    for c in [ c1 .. c2 ] do
                        for r in [ r1 .. r2 ] do
                            let ref = { Sheet = Some sheetName ; Col = c ; ColAbs = true ; Row = r ; RowAbs = true }
                            yield cellValue cell ref workbook dirty computing
                } |> Seq.toList |> ValueList

    and evalFun name list cell workbook dirty computing =
        match name with
            | "IF" -> 
                match list with
                    | [ e1 ; e2 ; e3 ] ->
                        match eval cell e1 workbook dirty computing with
                            | NullValue
                            | FloatValue 0.0
                            | BooleanValue false -> eval cell e3 workbook dirty computing
                            | FloatValue _
                            | BooleanValue true -> eval cell e2 workbook dirty computing
                            | ValueList _
                            | StringValue _ -> ErrorValue Value
                            | ErrorValue v -> ErrorValue v
                    | _ -> ErrorValue Parse
            | "SUM" ->
                match list with
                    | [] -> ErrorValue Parse
                    | _ ->
                        let values = 
                            list 
                                |> List.map (fun e -> eval cell e workbook dirty computing)
                                |> List.collect (fun e -> match e with | ValueList l -> l | _ -> [e])
                                |> List.filter (fun e -> match e with | BooleanValue _ | FloatValue _ | ErrorValue _ -> true | _ -> false)
                        let error = values |> List.tryFind (fun v -> match v with | ErrorValue _ -> true | _ -> false)
                        match error with
                            | Some e -> e
                            | None ->
                                values
                                    |> List.map (fun v -> match v with | BooleanValue b -> toFloat b | FloatValue(f) -> f | _ -> 0.0)
                                    |> List.sum
                                    |> FloatValue
            | "COUNT" ->
                match list with
                    | [] -> ErrorValue Parse
                    | _ ->
                        list 
                            |> List.map (fun e -> eval cell e workbook dirty computing)
                            |> List.collect (fun e -> match e with | ValueList l -> l | _ -> [e])
                            |> List.filter (fun e -> match e with | FloatValue _ -> true | _ -> false)
                            |> List.length
                            |> float
                            |> FloatValue
            | "RAND" ->
                match list with
                    | [] -> 
                        FloatValue(random.NextDouble())
                    | _ -> ErrorValue Parse
            | _ -> ErrorValue Parse

    and isVolatile expr =
        let isVolatileFun = function
            | "NOW"
            | "RAND" -> true
            | "RANDBETWEEN" -> true
            | _ -> false
        match expr with
            | Float _ 
            | Boolean _ 
            | String _ 
            | EscapedString _ 
            | Error _ 
            | Blank -> false
            | Negate e -> isVolatile e
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
            | UnresolvedRef(ref) -> failwith "References must be resolved before calling isVolatile"
            | Fun(name, list) -> isVolatileFun name || List.exists isVolatile list
            | Ref _ -> true