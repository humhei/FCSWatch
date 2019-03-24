// Learn more about F# at http://fsharp.org
module FcsWatch.Cli.Main
open FcsWatch.FcsWatcher
open FcsWatch.Cli.Share


[<EntryPoint>]
let main argv =

    let results = parser.Parse argv

    let processResult = processParseResults parser.PrintUsage results

    runFcsWatcher processResult.Config processResult.ProjectFile

    0 // return an integer exit code
