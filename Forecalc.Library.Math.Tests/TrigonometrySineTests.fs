module TrigonometrySineTests

open System.Collections.Generic
open NUnit.Framework
open FsUnit
open Forecalc.Library
open Forecalc.Library.Ast
open Forecalc.Library.Eval

[<Test>]
let ``SIN(0.0) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SIN", [ Float 0.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

[<Test>]
let ``SIN(3.141593) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SIN", [ Float 3.141593 ])
    match eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) with
        | FloatValue(v) -> v |> should (equalWithin 0.001) 0.0
        | _ -> failwith "Fail"

[<Test>]
let ``SIN(1.570796327) -> FloatValue(1.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SIN", [ Float 1.570796327 ])
    match eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) with
        | FloatValue(v) -> v |> should (equalWithin 0.001) 1.0
        | _ -> failwith "Fail"
    
[<Test>]
let ``SIN(true) -> FloatValue(0.841470985)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SIN", [ Boolean true ])
    match eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) with
        | FloatValue(v) -> v |> should (equalWithin 0.001) 0.841470985
        | _ -> failwith "Fail"
    
[<Test>]
let ``SIN(false) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SIN", [ Boolean false ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

[<Test>]
let ``SIN(Blank) -> FloatValue(0.0)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SIN", [ Blank ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (FloatValue 0.0)

[<Test>]
let ``SIN("42") -> ErrorValue(Value)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SIN", [ String "42" ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Value)
    
[<Test>]
let ``SIN([]) -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SIN", [])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)
    
[<Test>]
let ``SIN([ 0 ; 0 ]) -> ErrorValue(Parse)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SIN", [ Float 0.0 ; Float 0.0 ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue Parse)
 
[<Test>]
let ``SIN(DivZero) -> ErrorValue(DivZero)``() =
    let workbook = Map.ofList [ "Sheet1", (QT4.create<CellContent>()) ]
    let cell = { Sheet = "Sheet1" ; Row = 1 ; Col = 1 }
    let expr = Fun("SIN", [ Error DivZero ])
    eval cell expr workbook (HashSet<AbsCell>()) (HashSet<AbsCell>()) |> should equal (ErrorValue DivZero)
 
