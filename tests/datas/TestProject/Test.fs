// Learn more about F# at http://fsharp.org

open System
open System.Collections.Concurrent
open System.Threading
open TestLib2
[<EntryPoint>]
let main _ =
    printfn "211212255551226%s" (Say.fromLib2 )
    /// simulate server
    Console.ReadLine()
    |> ignore

    0 // return an integer exit code
