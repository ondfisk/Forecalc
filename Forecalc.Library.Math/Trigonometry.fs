namespace Forecalc.Library

open System
open System.Collections.Generic
open System.ComponentModel.Composition
open System.ComponentModel.Composition.Hosting
open Ast
open Eval


[<Export(typeof<ISheetFunction>)>]
type Pi() = 
    interface ISheetFunction with
        member this.Name = "PI"
        member this.Apply list cell workbook dirty computing =
            match list with
                | [] -> FloatValue(Math.PI)
                | _ -> ErrorValue Parse

[<Export(typeof<ISheetFunction>)>]
type Cos() = 
    interface ISheetFunction with
        member this.Name = "COS"
        member this.Apply list cell workbook dirty computing =
            match list with
                | [ e ] -> 
                    match eval cell e workbook dirty computing with
                        | FloatValue v -> FloatValue (Math.Cos v)
                        | BooleanValue v -> FloatValue (Math.Cos (toFloat v))
                        | NullValue -> FloatValue (Math.Cos 0.0)
                        | ValueList _
                        | StringValue _ -> ErrorValue Value
                        | ErrorValue v -> ErrorValue v 
                | _ -> ErrorValue Parse
