// Learn more about F# at http://fsharp.org

open System
open System.Collections.Concurrent
open System.Threading
let cache = new ConcurrentDictionary<string,string>()

[<EntryPoint>]
let main _ =
    printfn "Hello World from F#!    "
    cache.AddOrUpdate("Hello","Yes",fun key value -> 
        "HelloWorld")

    cache.AddOrUpdate("Hello","Yes",fun key value -> 
        Thread.Sleep(1000)
        "HelloWorld2")
    cache.AddOrUpdate("Hello","Yes",fun key value -> 
        "HelloWorld3")
    Console.ReadLine() |> ignore  
    0 // return an integer exit code
 



 