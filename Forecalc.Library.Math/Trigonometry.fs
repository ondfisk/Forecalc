namespace Forecalc.Library

open System
open System.Collections.Generic
open System.ComponentModel.Composition
open System.ComponentModel.Composition.Hosting
open Ast
open Eval

module Trigonometry =
    let apply func list cell workbook dirty computing =
        match list with
            | [ e ] -> 
                match eval cell e workbook dirty computing with
                    | FloatValue v -> FloatValue (func v)
                    | BooleanValue v -> FloatValue (func (toFloat v))
                    | NullValue -> FloatValue (func 0.0)
                    | ValueList _
                    | StringValue _ -> ErrorValue Value
                    | ErrorValue v -> ErrorValue v   
            | _ -> ErrorValue Parse

[<Export(typeof<ISheetFunction>)>]
type Pi() = 
    interface ISheetFunction with
        member __.Name = "PI"
        member __.Apply list cell workbook dirty computing =
            match list with
                | [] -> FloatValue(Math.PI)
                | _ -> ErrorValue Parse

[<Export(typeof<ISheetFunction>)>]
type Degrees() = 
    let degrees r = r * 180.0 / Math.PI
    interface ISheetFunction with
        member __.Name = "DEGREES"
        member __.Apply list cell workbook dirty computing =
            Trigonometry.apply degrees list cell workbook dirty computing

[<Export(typeof<ISheetFunction>)>]
type Radians() = 
    let radians d = d * Math.PI / 180.0
    interface ISheetFunction with
        member __.Name = "RADIANS"
        member __.Apply list cell workbook dirty computing =
            Trigonometry.apply radians list cell workbook dirty computing

[<Export(typeof<ISheetFunction>)>]
type Cosine() = 
    interface ISheetFunction with
        member __.Name = "COS"
        member __.Apply list cell workbook dirty computing =
            Trigonometry.apply Math.Cos list cell workbook dirty computing

[<Export(typeof<ISheetFunction>)>]
type Sine() = 
    interface ISheetFunction with
        member __.Name = "SIN"
        member __.Apply list cell workbook dirty computing =
            Trigonometry.apply Math.Sin list cell workbook dirty computing

[<Export(typeof<ISheetFunction>)>]
type Tangent() = 
    interface ISheetFunction with
        member __.Name = "TAN"
        member __.Apply list cell workbook dirty computing =
            Trigonometry.apply Math.Tan list cell workbook dirty computing