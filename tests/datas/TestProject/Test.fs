// Learn more about F# at http://fsharp.org

open System
open System.Collections.Concurrent
open System.Threading
open TestLib2
[<EntryPoint>]
let main _ =
    printfn "HELLO21999108859911000999  %s" (Say.fromLib2 )
    /// simulate server
    Console.ReadLine()
    |> ignore

    0 // return an integer exit code
