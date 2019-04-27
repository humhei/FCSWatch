// Copyright 2018 Fabulous contributors. See LICENSE.md for license.

// F# PortaCode command processing (e.g. used by Fabulous.Cli)

[<AutoOpen>]
module FcsWatch.Porta.Main

open System
open FSharp.Compiler.SourceCodeServices
open FcsWatch.Core.Compiler
open FcsWatch.Core
open FcsWatch.Core.FcsWatcher
open FcsWatch.Core.Types
open Extensions
open Fake.IO
open System.IO

let runFcsWatcher (config: PortaConfig) =
    logger <- Logger.create config.LoggerLevel
    let compiler =
        { new ICompiler<CheckResult> with 
            member x.Compile(checker, crackedFsproj) = async {
                let! result = CrackedFsproj.check config.UseEditFiles config.LiveCheckOnly checker crackedFsproj
                return [|result|]
            }
            member x.WarmCompile = true
            member x.Summary (_, _) = ""
        }

    let compilerTmpEmitter = CompilerTmpEmitter.create config


    let coreConfig: FcsWatch.Core.Config = 
        { LoggerLevel = config.LoggerLevel
          WorkingDir = config.WorkingDir
          UseEditFiles = config.UseEditFiles
          OtherFlags = config.OtherFlags
          Configuration = Configuration.Debug }

    let checker = FSharpChecker.Create(keepAssemblyContents = true)

    let fcsWatcher, _ = fcsWatcherAndCompilerTmpAgent checker compilerTmpEmitter compiler coreConfig config.Fsproj

    if config.Watch then
        Console.ReadLine() |> ignore
    elif config.Eval then 
        let makeSourceFileChanges fullPaths =
            let makeFileChange fullPath : FileChange =
                let fullPath = Path.getFullName fullPath

                { FullPath = fullPath
                  Name = Path.GetFileName fullPath
                  Status = FileStatus.Changed }

            FcsWatcherMsg.DetectSourceFileChanges <!> (List.map makeFileChange fullPaths)

        let cache = fcsWatcher.PostAndReply FcsWatcherMsg.GetCache

        fcsWatcher.PostAndReply (makeSourceFileChanges [cache.EntryCrackedFsproj.SourceFiles.[0]])

        fcsWatcher.PostAndReply FcsWatcherMsg.WaitCompiled
        |> ignore
