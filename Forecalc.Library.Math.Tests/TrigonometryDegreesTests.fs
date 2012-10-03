module TrigonometryDegreesTests

open System.Collections.Generic
open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``DEGREES(0.0) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("DEGREES", [ Float 0.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

[<Test>]
let ``DEGREES(3.141593) -> FloatValue(180.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("DEGREES", [ Float 3.141593 ])
    match eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) with
        | FloatValue(v) -> v |> should (equalWithin 0.001) 180.0
        | _ -> failwith "Fail"
    
[<Test>]
let ``DEGREES(true) -> FloatValue(57.29577951)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("DEGREES", [ Boolean true ])
    match eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) with
        | FloatValue(v) -> v |> should (equalWithin 0.001) 57.29577951
        | _ -> failwith "Fail"
    
let ``DEGREES(false) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("DEGREES", [ Float 0.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

let ``DEGREES(blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("DEGREES", [ Blank ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

[<Test>]
let ``DEGREES("42") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("DEGREES", [ String "42" ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Value)
    
[<Test>]
let ``DEGREES([]) -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("DEGREES", [])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)
    
[<Test>]
let ``DEGREES([ 0 ; 0 ]) -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("DEGREES", [ Float 0.0 ; Float 0.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)
 
[<Test>]
let ``DEGREES(DivZero) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("DEGREES", [ Error DivZero ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue DivZero)
 
