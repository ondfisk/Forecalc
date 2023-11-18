﻿namespace Forecalc.Library

open System.Collections.Generic
open System.Linq

module Workbook =
    let makeDirtySet (workbook : Map<string, QT4<CellContent>>) =
        seq {
            for pair in workbook do
                let name = pair.Key
                let worksheet = pair.Value
                let l = QT4.w * QT4.h - 1
                for i0 in [ 0 .. l ] do
                    let v1 = worksheet.Tile0.[i0]
                    let c0 = (i0 >>> QT4.logh) <<< (3 * QT4.logw)
                    let r0 = (i0 &&& QT4.mh) <<< (3 * QT4.logh)
                    match v1 with
                        | None -> ()
                        | Some(t1) ->
                            for i1 in [ 0 .. l ] do
                                let v2 = t1.[i1]
                                let c1 = (i1 >>> QT4.logh) <<< (2 * QT4.logw)
                                let r1 = (i1 &&& QT4.mh) <<< (2 * QT4.logh)
                                match v2 with
                                    | None -> ()
                                    | Some(t2) ->
                                        for i2 in [ 0 .. l ] do
                                            let v3 = t2.[i2]
                                            let c2 = (i2 >>> QT4.logh) <<< QT4.logw
                                            let r2 = (i2 &&& QT4.mh) <<< QT4.logh
                                            match v3 with
                                                | None -> ()
                                                | Some(t3) ->
                                                    for i3 in [ 0 .. l ] do
                                                        let v = t3.[i3]
                                                        match v with
                                                            | None -> ()
                                                            | Some(t) ->
                                                                let c = c0 ||| c1 ||| c2 ||| (i3 >>> QT4.logh)
                                                                let r = r0 ||| r1 ||| r2 ||| (i3 &&& QT4.mh)
                                                                if t.Volatile then
                                                                    yield { Sheet = name ; Col = c + 1 ; Row = r + 1 }
        } |> Set.ofSeq

    let recalculate cell expr workbook =
        let expr = expr |> Expression.parse |> ReferenceResolver.resolveRefs cell
        let dirty = HashSet<AbsCell>(makeDirtySet workbook)
        let computing = HashSet<AbsCell>()
        Eval.eval cell expr workbook dirty computing |> ignore
        while dirty.Count > 0 do
            let d = dirty.First()
            match workbook.[d.Sheet].[d.Col - 1, d.Row - 1] with
                | None -> ()
                | Some e -> Eval.eval d e.Expr workbook dirty computing |> ignore

    let toArray sheet (c1, r1) (c2, r2) (workbook : Map<string, QT4<CellContent>>) =
        if not (Map.containsKey sheet workbook) then failwithf "The sheet: %s does not exist" sheet
        let worksheet = workbook |> Map.find sheet
        let array = Array2D.create (c2 - c1 + 1) (r2 - r1 + 1) NullValue
        for c in [ c1 .. c2 ] do
            for r in [ r1 .. r2 ] do
                match worksheet.[c - 1, r - 1] with
                    | None -> ()
                    | Some(content) -> array.[c - 1, r - 1] <- content.Value
        array