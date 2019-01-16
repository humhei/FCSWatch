// Learn more about F# at http://fsharp.org

open System
open System.Collections.Concurrent
open System.Threading
type Hello = {Name: string}
with 
    member x.Say = 
        printfn "Hello"
        "Hello"  

[<EntryPoint>]
let main _ =
    printfn "sdasadsda"
    let a = ""
    Console.Read()
    0 // return an integer exit code
 