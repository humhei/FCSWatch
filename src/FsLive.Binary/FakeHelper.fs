module FsLive.Binary.FakeHelper
open FSharp.Compiler.SourceCodeServices
open FsLive.Binary.Main
open System

let runFsLive (checker: FSharpChecker) projectFile = 
    let fsLive = fsLive id checker projectFile 
    printfn "Waiting for changes... press any key to exit" 
    Console.ReadLine() |> ignore
let runFsLiveWith buidingConfig (checker: FSharpChecker) projectFile = 
    let fsLive = fsLive buidingConfig checker projectFile 
    printfn "Waiting for changes... press any key to exit" 
    Console.ReadLine() |> ignore
