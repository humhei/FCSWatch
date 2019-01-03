// Learn more about F# at http://fsharp.org

open System
open System.Collections.Concurrent
open System.Threading
let cache = new ConcurrentDictionary<string,string>()
[<EntryPoint>]
let main _ =
    printfn "asasdasdasddsaasdasd"
    Console.ReadLine() |> ignore  
    0 // return an integer exit code
 