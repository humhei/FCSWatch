// Learn more about F# at http://fsharp.org

open Argu
open FcsWatch
open FcsWatch.Types
open System

type CoreArguments = FcsWatch.Cli.Arguments

type Arguments =
    | Working_Dir of string
    | Project_File of string
    | Auto_Reload
    | Logger_Level of Logger.Level
    | No_Build
    | ExcelDna
with 
    member x.AsCore =
        match x with 
        | Working_Dir arg  -> CoreArguments.Working_Dir arg |> Some
        | Project_File arg -> CoreArguments.Project_File arg |> Some
        | Auto_Reload -> CoreArguments.Auto_Reload |> Some
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
    let coreParser = FcsWatch.Cli.parser

    let parser = ArgumentParser.Create<Arguments>(programName = "fcswatch-win.exe")
    let results = parser.Parse argv

    let processResult = 
        results.GetAllResults()
        |> List.choose (fun (result: Arguments) -> result.AsCore)
        |> coreParser.ToParseResults
        |> FcsWatch.Cli.processParseResults parser.PrintUsage

    let developmentTarget = 
        match results.TryGetResult ExcelDna with 
        | Some _ ->
            ExcelDna.plugin processResult.ProjectFile
            |> DevelopmentTarget.Plugin
        | None ->
            DevelopmentTarget.Program

    let config =
        { processResult.Config with 
            DevelopmentTarget = developmentTarget }

    FcsWatch.FcsWatcher.runFcsWatcher config processResult.ProjectFile

    0 // return an integer exit code
