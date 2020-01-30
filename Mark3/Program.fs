// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Game
open System.Runtime

[<EntryPoint>]
let main argv = 
    do GCSettings.LatencyMode <- GCLatencyMode.SustainedLowLatency
    use g = new Game1()
    g.Run()
    0