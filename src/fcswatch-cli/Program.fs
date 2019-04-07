// Learn more about F# at http://fsharp.org
module FcsWatch.Cli.Main
open FcsWatch.Binary
open FcsWatch.Cli.Share


[<EntryPoint>]
let main argv =

    let results = Binary.parser.Parse argv

    let processResult = Binary.processParseResults Binary.parser.PrintUsage results

    runFcsWatcher processResult.Config processResult.ProjectFile

    0 // return an integer exit code
