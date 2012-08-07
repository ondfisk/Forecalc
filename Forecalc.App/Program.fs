module Program 

open System
open Forecalc.Library


let qt4 = QT4.create<int>()
qt4.[1512, 6423] <- Some(42)

qt4 |> QT4.iteri (printfn "%i %i %A")
