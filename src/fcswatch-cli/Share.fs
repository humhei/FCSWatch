module FcsWatch.Cli.Share

open System
open FcsWatch.Core
open FcsWatch.Binary
open Argu
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.Core
open FcsWatch.Core.Types

let private defaultUrl = "http://localhost:9867/update"

type Arguments =
    | Working_Dir of string
    | Project_File of string
    | Debuggable
    | Logger_Level of Logger.Level
    | No_Build
    | Webhook of string
    | Send
    | [<AltCommandLine("-f")>] Framework of string
    | [<AltCommandLine("-c")>] Configuration of Configuration
with
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Working_Dir _  -> "Specfic working directory, default is current directory"
            | Project_File _ -> "Entry project file, default is exact fsproj file in working dir"
            | Debuggable _ -> "Enable debuggable in vscode, This will disable auto Reload"
            | Logger_Level _ -> "Default is Minimal"
            | No_Build -> "--no-build"
            | Webhook _ -> "send a web hook when program (re)run"
            | Send _ -> sprintf "Equivalent to --webhook %s" defaultUrl
            | Framework _ -> "The target framework to build for. The default to prefer netcore."
            | Configuration _ -> "(experienment)The configuration to use for building the project. The default is Debug."

type ProcessResult =
    { Config: BinaryConfig
      ProjectFile: string }

let processParseResults additionalBinaryArgs (results: ParseResults<Arguments>) =
    let execContext = Fake.Core.Context.FakeExecutionContext.Create false "generate.fsx" []
    Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
    let defaultConfigValue = BinaryConfig.DefaultValue

    let workingDir = results.GetResult (Working_Dir,defaultConfigValue.WorkingDir)

    let projectFile =
        match results.TryGetResult Project_File with
        | Some projectFile -> projectFile
        | None ->
            (!! (workingDir </> "*.fsproj")
            |> Seq.filter (fun file -> file.EndsWith ".fsproj")
            |> List.ofSeq
            |> function
                | [ ] ->
                    failwithf "no project file found, no compilation arguments given and no project file found in \"%s\"" Environment.CurrentDirectory
                | [ file ] ->
                    printfn "using implicit project file '%s'" file
                    file
                | file1 :: file2 :: _ ->
                    failwithf "multiple project files found, e.g. %s and %s" file1 file2 )

    let noBuild =
        match results.TryGetResult No_Build with
        | Some _ -> true
        | None ->
            false

    let webhook = 
        match results.TryGetResult Send, results.TryGetResult Webhook with 
        | Some _, _ -> Some defaultUrl
        | _, Some webhook -> Some webhook
        | _ -> None

    { ProjectFile = projectFile
      Config =
        { BinaryConfig.DefaultValue with
            WorkingDir = workingDir
            DevelopmentTarget =
                match results.TryGetResult Debuggable with
                | Some _ -> DevelopmentTarget.debuggableProgram
                | None -> DevelopmentTarget.autoReloadProgram

            LoggerLevel = results.GetResult(Logger_Level, defaultConfigValue.LoggerLevel)
            NoBuild = noBuild
            Framework = results.TryGetResult Framework
            Configuration = results.GetResult(Configuration, BinaryConfig.DefaultValue.Configuration)
            Webhook = webhook
            AdditionalSwitchArgs = additionalBinaryArgs }
    }




let parser = ArgumentParser.Create<Arguments>(programName = "fcswatch.exe")
