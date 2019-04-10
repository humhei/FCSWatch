﻿
namespace FcsWatch.Binary
open Fake.Core
open FcsWatch.Core
open Types
open FcsWatch.Core.CrackedFsproj
open System.Diagnostics
open System.Collections.Concurrent
open Fake.DotNet
open FcsWatch.Binary.Helpers
open Fake.IO
open Fake.IO.FileSystemOperators
open FcsWatch.Core.CompilerTmpEmitter
open Extensions

type PluginDebugInfo =
    { DebuggerAttachTimeDelay: int 
      Pid: int 
      VscodeLaunchConfigurationName: string }

[<RequireQualifiedAccess>]
module PluginDebugInfo =
    open VscodeHelper


    let writePidForPlugin workingDir (pluginDebugInfo: PluginDebugInfo) =
        match File.tryFindLaunchJsonUp workingDir with 
        | Some file ->

            let launch = Launch.read file
            Launch.writePid pluginDebugInfo.VscodeLaunchConfigurationName pluginDebugInfo.Pid launch
            |> Launch.write file
        | None -> logger.Important "Doesn't exists .vscode/launch.json, If you want write attachable pid automatically to launch.json, Please write it first"


[<RequireQualifiedAccess>]
module AutoReload =

    type TmpEmitterMsg = unit
    type TmpEmitterState = CompilerTmpEmitterState<int, CompilerResult>

    type Plugin =
        { Load: unit -> unit
          Unload: unit -> unit
          Calculate: unit -> unit
          /// sometimes i want work both in autoReload + debuggable
          PluginDebugInfo: PluginDebugInfo option }


    [<RequireQualifiedAccess>]
    type DevelopmentTarget =
        | Program
        | Plugin of Plugin

    let private runningProjects = new ConcurrentDictionary<string,Process>()

    [<RequireQualifiedAccess>]
    module internal CrackedFsproj =

        let private dotnetTool = 
            [DotNet.Options.Create().DotNetCliPath]
            |> Args.toWindowsCommandLine

        [<RequireQualifiedAccess>]
        module Process =
            let start tool args workingDir = 
                let startInfo = new ProcessStartInfo();
                let args = Args.toWindowsCommandLine args
                startInfo.WorkingDirectory <- workingDir
                startInfo.Arguments <- args
                startInfo.FileName <- tool
                logger.ImportantGreen "%s %s" tool args 
                Process.Start(startInfo)

        let tryRun developmentTarget workingDir (crackedFsproj: CrackedFsproj) =
            match (developmentTarget: DevelopmentTarget) with
            | DevelopmentTarget.Program ->
                match crackedFsproj.ProjectTarget with 
                | ProjectTarget.Exe -> 
                    runningProjects.GetOrAdd (crackedFsproj.ProjPath,fun _ ->
                        Process.start 
                            dotnetTool
                            ["run";"--project"; crackedFsproj.ProjPath; "--no-build"; "--no-restore"; "--framework"; crackedFsproj.PreferFramework] workingDir
                    )
                    |> ignore

                | ProjectTarget.Library ->
                    failwith "project is a library, autoReload is ture, development target is program;"
            | DevelopmentTarget.Plugin plugin ->
                plugin.Load()

        let tryReRun developmentTarget workingDir (crackedFsproj: CrackedFsproj) =
            tryRun developmentTarget workingDir (crackedFsproj: CrackedFsproj) 
            match developmentTarget with
            | DevelopmentTarget.Plugin plugin ->
                plugin.Calculate()
            | _ -> ()


        let tryKill developmentTarget (crackedFsproj: CrackedFsproj) =
            match developmentTarget with 
            | DevelopmentTarget.Plugin plugin ->
                plugin.Unload()
            | DevelopmentTarget.Program -> 
                match runningProjects.TryRemove (crackedFsproj.ProjPath) with
                | true, proc -> 
                    if not proc.HasExited 
                    then 
                        proc.KillTree()
                        logger.InfoGreen "Killed process %d" proc.Id
                | false, _ -> 
                    failwithf "Cannot remove %s from running projects" crackedFsproj.ProjPath


    [<RequireQualifiedAccess>]
    module internal TmpEmitterState =

        let tryEmit workingDir developmentTarget (state: TmpEmitterState) =
            let commonState = state.CommonState
            let cache = commonState.CrackerFsprojFileBundleCache

            match commonState.CompilingNumber with 
            | 0 ->

                logger.Info "Current cached compier task is %d" commonState.CachedCompilerTasks.Length

                match commonState.CachedCompilerTasks with 
                | [task] when task.Why = WhyCompile.WarmCompile -> state
                | _ ->
                    let lastTasks = 
                        commonState.CachedCompilerTasks 
                        |> List.groupBy (fun compilerTask ->
                            compilerTask.Task.Result.[0].ProjPath
                        )
                        |> List.map (fun (projPath, compilerTasks) ->
                            compilerTasks |> List.maxBy (fun compilerTask -> compilerTask.StartTime)
                        )


                    let allResults: CompilerResult list = lastTasks |> List.collect (fun task -> task.Task.Result)

                    match List.tryFind (fun (result: CompilerResult) -> result.ExitCode <> 0) allResults with 
                    | Some result ->  state

                    | None ->

                        let projRefersMap = cache.ProjRefersMap

                        let projLevelMap = cache.ProjLevelMap

                        CrackedFsproj.tryKill developmentTarget cache.EntryCrackedFsproj

                        commonState.CompilerTmp
                        |> Seq.sortByDescending (fun projPath ->
                            projLevelMap.[projPath]
                        )
                        |> Seq.iter (fun projPath ->
                            let currentCrackedFsproj = cache.ProjectMap.[projPath]

                            CrackedFsproj.copyObjToBin currentCrackedFsproj

                            let refCrackedFsprojs = projRefersMap.[projPath]

                            refCrackedFsprojs |> Seq.sortByDescending (fun refCrackedFsproj ->
                                projLevelMap.[refCrackedFsproj.ProjPath]
                            )
                            |> Seq.iter (CrackedFsproj.copyFileFromRefDllToBin projPath)
                        )

                        CrackedFsproj.tryReRun developmentTarget workingDir cache.EntryCrackedFsproj
                    
                        CompilerTmpEmitterState.createEmpty 0 cache
            | _ -> 
                state


    let create developmentTarget = 
        { new ICompilerTmpEmitter<unit, int, CompilerResult> with 
            member x.TryEmit(workingDir, state) = TmpEmitterState.tryEmit workingDir developmentTarget state
            member x.ProcessCustomMsg (_, state) = state
            member x.CustomInitialState = 0 }
        