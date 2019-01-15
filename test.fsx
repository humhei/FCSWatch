// Learn more about F# at http://fsharp.org

open System
open System.Collections.Concurrent
open System.Threading
open System.IO
open System.Net
open System.Collections.Generic
open System.Collections

let rec loop (searchCache:Map<string,string>) = async {
    if searchCache.Count > 0 then
        printfn "HelloWorld" 
    else 
        return! loop (searchCache.Add("6","5"))      
        return! loop (searchCache.Add("6","5"))  
        printfn "Yes"    
}
let r = loop Map.empty |> Async.RunSynchronously