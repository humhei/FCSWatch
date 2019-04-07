// Copyright 2018 Fabulous contributors. See LICENSE.md for license.

// F# PortaCode command processing (e.g. used by Fabulous.Cli)

[<AutoOpen>]
module FcsWatch.Porta.Main

open System
open FSharp.Compiler.SourceCodeServices
open FcsWatch.Core.CompilerTmpEmitter
open FcsWatch.Core.Compiler
open FcsWatch.Core
open FcsWatch.Core.FcsWatcher

open Extensions

let runFcsWatcher (config: PortaConfig) entryProjectFile =
    logger <- Logger.create config.LoggerLevel
    let compiler =
        { new ICompiler<CheckResult> with 
            member x.Compile(checker, crackedFsproj) = async {
                let! result = CrackedFsproj.check config.UseEditFiles config.LiveCheckOnly checker crackedFsproj
                return [|result|]
            }
            member x.WarmCompile = false
            member x.Summary (_, _) = ""
        }

    let compilerTmpEmitter = CompilerTmpEmitter.create config


    let config: FcsWatch.Core.Config = 
        { LoggerLevel = config.LoggerLevel
          WorkingDir = config.WorkingDir
          NoBuild = config.NoBuild
          UseEditFiles = config.UseEditFiles
          ActionAfterStoppingWatcher = ignore }

    let fcsWatcher = fcsWatcher compilerTmpEmitter compiler config entryProjectFile

    Console.ReadLine() |> ignore

