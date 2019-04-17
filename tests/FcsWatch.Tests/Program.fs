// Learn more about F# at http://fsharp.org

open Expecto.Logging
open Expecto.Tests
open FcsWatch.Tests.Tests

let testConfig =  
    { Expecto.Tests.defaultConfig with 
         parallelWorkers = 1
         verbosity = LogLevel.Debug }

let tests = 
    testList "All tests" [ 
        programTests             
        pluginTests 
        webhookTests
        functionTests           
    ] 
// Create an interactive checker instance 
[<EntryPoint>]
let main argv = 
    runTests testConfig tests
