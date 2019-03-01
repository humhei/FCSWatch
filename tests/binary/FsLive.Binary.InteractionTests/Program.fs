// Learn more about F# at http://fsharp.org

open Expecto.Logging
open Expecto.Tests
open FsLive.Binary.InteractionTests.InteractionTests

let testConfig =  
    { Expecto.Tests.defaultConfig with 
         parallelWorkers = 1
         verbosity = LogLevel.Debug }

let tests = 
    testList "All tests" [ 
        interactionTests 
    ] 
// Create an interactive checker instance 
[<EntryPoint>]
let main argv = runTests testConfig tests
