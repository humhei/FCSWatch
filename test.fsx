// Learn more about F# at http://fsharp.org

open System
open System.Collections.Concurrent
open System.Threading
let cache = new ConcurrentDictionary<string,string>()

printfn "Hello World from F#!    "
cache.AddOrUpdate("Hello","Yes",fun key value -> 
    "HelloWorld")
async {
cache.AddOrUpdate("Hello","Yes",fun key value -> 
    Thread.Sleep(2000)
    "HelloWorld2")
} |> Async.Start
async {
    cache.AddOrUpdate("Hello","Yes",fun key value -> 
        "HelloWorld3")
} |> Async.Start
Thread.Sleep(5000)
printfn "%A" (cache.GetOrAdd("Hello",""))
    
Console.ReadLine() |> ignore
0 // return an integer exit code
 



 