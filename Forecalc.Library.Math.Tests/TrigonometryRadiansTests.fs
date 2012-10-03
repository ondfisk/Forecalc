module TrigonometryRadiansTests

open System.Collections.Generic
open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``RADIANS(0.0) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("RADIANS", [ Float 0.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

[<Test>]
let ``RADIANS(180.0) -> FloatValue(3.141593)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("RADIANS", [ Float 180.0 ])
    match eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) with
        | FloatValue(v) -> v |> should (equalWithin 0.001) 3.141593
        | _ -> failwith "Fail"
    
[<Test>]
let ``RADIANS(true) -> FloatValue(0.017453293)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("RADIANS", [ Boolean true ])
    match eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) with
        | FloatValue(v) -> v |> should (equalWithin 0.001) 0.017453293
        | _ -> failwith "Fail"
    
let ``RADIANS(false) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("RADIANS", [ Float 0.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

let ``RADIANS(blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("RADIANS", [ Blank ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

[<Test>]
let ``RADIANS("42") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("RADIANS", [ String "42" ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Value)
    
[<Test>]
let ``RADIANS([]) -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("RADIANS", [])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)
    
[<Test>]
let ``RADIANS([ 0 ; 0 ]) -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("RADIANS", [ Float 0.0 ; Float 0.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)
 
[<Test>]
let ``RADIANS(DivZero) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("RADIANS", [ Error DivZero ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue DivZero)
 
