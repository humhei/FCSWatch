// Learn more about F# at http://fsharp.org

open System
open System.Collections.Concurrent
open System.Threading
open TestLib2
[<EntryPoint>]
let main _ =
    printfn "ssss  %s" Say.fromLib2
    0 // return an integer exit code
