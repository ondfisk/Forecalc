module Program 

open System
open System.Diagnostics

[<EntryPoint>]
let main argv = 
    let proc = Process.GetCurrentProcess()
    let counter = new PerformanceCounter("Process", "Private Bytes", proc.ProcessName)
    let min = counter.NextValue()
    let smallsheet = Array2D.zeroCreate<IntPtr> 65536 256
    let mem = counter.NextValue() - min
    printfn "Space usage in megabytes for a 65,536 rows by 256 columns sheet: %f" (mem/(1024.0f**2.0f))
    0