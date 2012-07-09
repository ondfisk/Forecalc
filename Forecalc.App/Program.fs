module Program 

open System
open Ast
open Forecalc.Library.Parser
open System.Text.RegularExpressions

let cell = { Sheet = "Sheet1" ; Cell = { Row = 3 ; RowAbs = false ; Col = 3 ; ColAbs = false } }

let resolveRC ref =
    let getInt s = 
        let (_, i) = Int32.TryParse s
        i
    let p = @"^R(\[?)([\+|\-]?\d+)?(\]?)C(\[?)([\+|\-]?\d+)?(\])?$"
    match (|Groups|_|) ref p with
        | Some ("[" :: row :: "]" :: "[" :: col :: "]" :: []) -> { Row = getInt row ; RowAbs = false ; Col = getInt col ; ColAbs = false }
        | Some ("" :: row :: "" :: "[" :: col :: "]" :: []) -> { Row = getInt row ; RowAbs = row <> "" ; Col = getInt col ; ColAbs = false }
        | Some ("[" :: row :: "]" :: "" :: col :: "" :: []) -> { Row = getInt row ; RowAbs = false ; Col = getInt col ; ColAbs = col <> "" }
        | Some ("" :: row :: "" :: "" :: col :: "" :: []) -> { Row = getInt row ; RowAbs = row <> "" ; Col = getInt col ; ColAbs = col <> "" }
        | Some(list) -> failwithf "Invalid ref format %A" list
        | None -> failwith "No match, invalid ref format"

let res = resolveRC "R[+1]C"


printfn "%A" res


