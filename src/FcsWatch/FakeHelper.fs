module FcsWatch.FakeHelper
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.FcsWatcher
open FcsWatch.Types
open System

let runFcsWatcher (checker: FSharpChecker) projectFile = 
    let watcher = fcsWatcher Config.DefaultValue checker projectFile 
    logger.Important "Waiting for changes... press any key to exit"
    Console.ReadLine() |> ignore

let runFcsWatcherWith config (checker: FSharpChecker) projectFile = 
    let watcher = fcsWatcher config checker projectFile 
    logger.Important "Waiting for changes... press any key to exit"
    Console.ReadLine() |> ignore