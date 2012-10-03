module TrigonometryTests

open System.Collections.Generic
open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``PI() -> FloatValue(3.141593)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("PI", [])
    match eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) with
        | FloatValue(v) -> v |> should (equalWithin 0.001) 3.141593
        | _ -> failwith "Fail"

[<Test>]
let ``PI("42") -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("PI", [ String "42" ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)