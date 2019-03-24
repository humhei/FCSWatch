[<RequireQualifiedAccess>]
module FcsWatch.AutoReload
open Fake.Core
open Types
open FcsWatch.CrackedFsproj
open System.Diagnostics
open System.Collections.Concurrent
open FcsWatch.Helpers
open Fake.DotNet

type Plugin =
    { Load: unit -> unit
      Unload: unit -> unit
      Calculate: unit -> unit }

[<RequireQualifiedAccess>]
type DevelopmentTarget =
    | Program
    | Plugin of Plugin

let private runningProjects = new ConcurrentDictionary<string,Process>()

[<RequireQualifiedAccess>]
module private CrackedFsproj =

    let dotnetTool = 
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
            | false, _ -> 
                failwithf "Cannot remove %s from running projects" crackedFsproj.ProjPath

[<RequireQualifiedAccess>]
type TmpEmitterMsg =
    | IncrCompilingNum of int
    | DecrCompilingNum of int
    | AddTmp of string (*proj path*)
    | AddTask of CompilerTask
    | UpdateCache of CrackedFsprojBundleCache

[<RequireQualifiedAccess>]
module TmpEmitterMsg =
    let msgName = function 
        | TmpEmitterMsg.IncrCompilingNum _ -> "IncrCompilingNum"
        | TmpEmitterMsg.DecrCompilingNum _ -> "DecrCompilingNum"
        | TmpEmitterMsg.AddTmp _ -> "AddTmp"
        | TmpEmitterMsg.AddTask _ -> "AddTask"
        | TmpEmitterMsg.UpdateCache _ -> "UpdateCache"

type TmpEmitterState =
    { CompilingNumber: int
      /// proj paths
      CompilerTmp: Set<string>
      CachedCompilerTasks: CompilerTask list
      CrackerFsprojFileBundleCache: CrackedFsprojBundleCache }

[<RequireQualifiedAccess>]
module TmpEmitterState =

    let createEmpty cache = 
        { CompilingNumber = 0
          CompilerTmp = Set.empty
          CachedCompilerTasks = []
          CrackerFsprojFileBundleCache = cache }

    let setCompilingNumber number state =
        { state with CompilingNumber = number}

    let tryEmit workingDir developmentTarget state =
        let cache = state.CrackerFsprojFileBundleCache

        match state.CompilingNumber with 
        | 0 ->

            logger.Info "Current cached compier task is %d" state.CachedCompilerTasks.Length

            match state.CachedCompilerTasks with 
            | [task] when task.Why = "warm compile" -> state
            | _ ->
                let lastTasks = 
                    state.CachedCompilerTasks 
                    |> List.groupBy (fun compilerTask ->
                        compilerTask.Task.Result.[0].ProjPath
                    )
                    |> List.map (fun (projPath, compilerTasks) ->
                        compilerTasks |> List.maxBy (fun compilerTask -> compilerTask.StartTime)
                    )


                let allResults: CompilerResult list = lastTasks |> List.collect (fun task -> task.Task.Result)

                match List.tryFind (fun (result: CompilerResult) -> result.ExitCode <> 0) allResults with 
                | Some result ->
                    let errorText =  
                        result.Errors 
                        |> Seq.map (fun error -> error.ToString())
                        |> String.concat "\n"

                    state 

                | None ->

                    let projRefersMap = cache.ProjRefersMap

                    let projLevelMap = cache.ProjLevelMap

                    CrackedFsproj.tryKill developmentTarget cache.EntryCrackedFsproj

                    state.CompilerTmp
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
                    
                    createEmpty cache
        | _ -> 
            state




    let processMsg (msg: TmpEmitterMsg) (state: TmpEmitterState) =
        let newState = 
            match msg with 

            | TmpEmitterMsg.DecrCompilingNum number ->
                let compilingNumber = state.CompilingNumber - number

                assert (state.CompilingNumber > 0)
                {state with CompilingNumber = compilingNumber}

            | TmpEmitterMsg.IncrCompilingNum number -> 
                let compilingNumber = state.CompilingNumber + number

                {state with CompilingNumber = compilingNumber } 

            | TmpEmitterMsg.AddTmp projectFile -> 
                let newCompilerTmp = state.CompilerTmp.Add projectFile

                {state with CompilerTmp = newCompilerTmp }        

            | TmpEmitterMsg.AddTask task -> 
                {state with CachedCompilerTasks = task :: state.CachedCompilerTasks} 

            | TmpEmitterMsg.UpdateCache cache ->
                {state with CrackerFsprojFileBundleCache = cache} 


        let msgName = TmpEmitterMsg.msgName msg
        logger.Info "compilerTmpEmitter agent receive message %s,current compiling number is %d" msgName newState.CompilingNumber

        newState

let create workingDir developmentTarget (initialCache: CrackedFsprojBundleCache) = MailboxProcessor<TmpEmitterMsg>.Start(fun inbox ->
    logger.Important "FcsWatch is running in autoReload mode"
    CrackedFsproj.tryRun developmentTarget workingDir initialCache.EntryCrackedFsproj
    let rec loop state = async {

        let! msg = inbox.Receive()
        let newState = TmpEmitterState.processMsg msg state

        match msg with 
        | TmpEmitterMsg.DecrCompilingNum _ ->

            let newState = TmpEmitterState.tryEmit workingDir developmentTarget newState

            return! loop newState    

        | _ -> return! loop newState
    }
    loop (TmpEmitterState.createEmpty initialCache) 
)

