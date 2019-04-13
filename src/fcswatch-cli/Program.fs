// Learn more about F# at http://fsharp.org
module FcsWatch.Cli.Main
open FcsWatch.Binary
open FcsWatch.Cli.Share


let splitArgs args =
    let arguArgs =
        args
        |> Array.takeWhile(fun x -> x <> "--")
    let additionalArgs =
        args
        |> Array.skipWhile(fun x -> x <> "--")
        |> (fun a -> if Array.tryHead a = Some "--" then Array.tail a else a)
    (arguArgs, additionalArgs)

[<EntryPoint>]
let main argv =

    let arguArgs, additionalArgs = splitArgs argv
    let results = parser.Parse arguArgs

    let processResult = processParseResults additionalArgs parser.PrintUsage results

    runFcsWatcher processResult.Config processResult.ProjectFile

    0 // return an integer exit code
