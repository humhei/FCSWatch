// Learn more about F# at http://fsharp.org
module Program
open Argu
open FcsWatch
open FcsWatch.Core
open FcsWatch.Binary


type CoreArguments = FcsWatch.Cli.Share.Arguments

type Arguments =
    | Working_Dir of string
    | Project_File of string
    | Debuggable
    | Logger_Level of Logger.Level
    | No_Build
    | ExcelDna
with 
    member x.AsCore =
        match x with 
        | Working_Dir arg  -> CoreArguments.Working_Dir arg |> Some
        | Project_File arg -> CoreArguments.Project_File arg |> Some
        | Debuggable -> CoreArguments.Debuggable |> Some
        | Logger_Level arg -> CoreArguments.Logger_Level arg |> Some
        | No_Build -> Some CoreArguments.No_Build
        | ExcelDna -> None

    interface IArgParserTemplate with
        member x.Usage =
            match x with 
            | ExcelDna -> "develop excel dna plugin"
            | _ -> ((x.AsCore.Value) :> IArgParserTemplate).Usage


[<EntryPoint>]
let main argv =
    let coreParser = FcsWatch.Cli.Share.parser

    let parser = ArgumentParser.Create<Arguments>(programName = "fcswatch-win.exe")
    let results = parser.Parse argv

    let processResult = 
        results.GetAllResults()
        |> List.choose (fun (result: Arguments) -> result.AsCore)
        |> coreParser.ToParseResults
        |> FcsWatch.Cli.Share.processParseResults parser.PrintUsage

    let developmentTarget = 
        match results.TryGetResult ExcelDna with 
        | Some _ -> ExcelDna.plugin processResult.Config.DevelopmentTarget processResult.ProjectFile
        | None ->
            processResult.Config.DevelopmentTarget

    let config =
        { processResult.Config with 
            DevelopmentTarget = developmentTarget }

    runFcsWatcher config processResult.ProjectFile

    0 // return an integer exit code
