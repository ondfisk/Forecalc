open System
open System.Diagnostics

let rows = 65536
let columns = 256
let proc = Process.GetCurrentProcess()
let counter = new PerformanceCounter("Process", "Private Bytes", proc.ProcessName)
let min = counter.NextValue()
let smallsheet = Array2D.zeroCreate<IntPtr> rows columns
let mem = counter.NextValue() - min
printfn "Space usage in megabytes for a %i rows by %i columns sheet: %f" 
    rows columns (mem/(1024.0f**2.0f))