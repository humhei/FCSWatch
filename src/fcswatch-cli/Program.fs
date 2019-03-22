// Learn more about F# at http://fsharp.org
module FcsWatch.Cli
open System
open FcsWatch
open Argu
open System.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fake.DotNet
open Fake.Core
open System.Threading

type Arguments =
    | Working_Dir of string
    | Project_File of string
    | Auto_Reload
    | Logger_Level of Logger.Level
    | No_Build
with 
    interface IArgParserTemplate with
        member x.Usage =
            match x with 
            | Working_Dir _  -> "Specfic working directory, default is current directory"
            | Project_File _ -> "Entry project file, default is exact fsproj file in working dir"
            | Auto_Reload _ -> "AutoReload Or Debuggable in vscode"
            | Logger_Level _ -> "Quiet; Minimal; Normal"
            | No_Build -> "--no-build"

type ProcessResult =
    { Config: Config 
      ProjectFile: string }


let processParseResults usage (results: ParseResults<Arguments>) =
    try 
        let execContext = Fake.Core.Context.FakeExecutionContext.Create false "generate.fsx" []
        Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
        let defaultConfigValue = Config.DefaultValue

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

        match results.TryGetResult No_Build with 
        | Some _ -> ()
        | None ->
            DotNet.build (fun ops ->
                { ops with Configuration = DotNet.BuildConfiguration.Debug }
            ) projectFile

        { ProjectFile = projectFile 
          Config =
            { Config.DefaultValue with 
                WorkingDir = workingDir
                AutoReload = 
                    match results.TryGetResult Auto_Reload with 
                    | Some _ -> true
                    | None -> false
                LoggerLevel = results.GetResult(Logger_Level, defaultConfigValue.LoggerLevel) } }
    with ex ->
        let usage = usage()
        failwithf "%A\n%s" ex.Message usage

let parser = ArgumentParser.Create<Arguments>(programName = "fcswatch.exe")

[<EntryPoint>]
let main argv =

    let results = parser.Parse argv

    let processResult = processParseResults parser.PrintUsage results

    let checker = FSharpChecker.Create()

    FakeHelper.runFcsWatcherWith processResult.Config checker processResult.ProjectFile

    0 // return an integer exit code
