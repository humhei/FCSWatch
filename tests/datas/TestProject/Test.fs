// Learn more about F# at http://fsharp.org

open System
open System.Collections.Concurrent
open System.Threading
open TestLib2
open FcsWatch.Porta.Client

type Program() =
    member x.Run() =
        printfn "HEssssss222  910885966911000999                        %s" (Say.fromLib2)

[<EntryPoint>]
let main _ =

    let fsliveClient = FsLiveClient()
    fsliveClient.Start()


    /// simulate server
    Console.ReadLine()
    |> ignore

    0 // return an integer exit code
