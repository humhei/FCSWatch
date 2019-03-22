module FcsWatch.AutoReload

open Types

[<RequireQualifiedAccess>]
type AutoReloadTmpEmitterMsg =
    | IncrCompilingNum of int
    | DecrCompilingNum of int
    | AddTmp of string (*proj path*)
    | AddTask of CompilerTask
    | UpdateCache of CrackedFsprojBundleCache

[<RequireQualifiedAccess>]
module AutoReloadTmpEmitterMsg =
    let msgName = function 
        | AutoReloadTmpEmitterMsg.IncrCompilingNum _ -> "IncrCompilingNum"
        | AutoReloadTmpEmitterMsg.DecrCompilingNum _ -> "DecrCompilingNum"
        | AutoReloadTmpEmitterMsg.AddTmp _ -> "AddTmp"
        | AutoReloadTmpEmitterMsg.AddTask _ -> "AddTask"
        | AutoReloadTmpEmitterMsg.UpdateCache _ -> "UpdateCache"

type AutoReloadTmpEmitterState =
    { CompilingNumber: int
      /// proj paths
      CompilerTmp: Set<string>
      CachedCompilerTasks: CompilerTask list
      CrackerFsprojFileBundleCache: CrackedFsprojBundleCache }

[<RequireQualifiedAccess>]
module AutoReloadTmpEmitterState =
    let createEmpty cache = 
        { CompilingNumber = 0
          CompilerTmp = Set.empty
          CachedCompilerTasks = []
          CrackerFsprojFileBundleCache = cache }

    let setCompilingNumber number state =
        { state with CompilingNumber = number}

    let tryEmit developmentTarget state =
        let cache = state.CrackerFsprojFileBundleCache

        match state.CompilingNumber with 
        | 0 ->

            logger.Info "Current cached compier task is %d" state.CachedCompilerTasks.Length

            let lastTasks = 
                state.CachedCompilerTasks 
                |> List.groupBy (fun compilerTask ->
                    compilerTask.Task.Result.[0].ProjPath
                )
                |> List.map (fun (projPath, compilerTasks) ->
                    compilerTasks |> List.maxBy (fun compilerTask -> compilerTask.StartTime)
                )

            let allResults = lastTasks |> List.collect (fun task -> task.Task.Result)

            match List.tryFind (fun result -> result.ExitCode <> 0) allResults with 
            | Some result ->
                let errorText =  
                    result.Errors 
                    |> Seq.map (fun error -> error.ToString())
                    |> String.concat "\n"

                state 

            | None ->

                let projRefersMap = cache.ProjRefersMap

                let projLevelMap = cache.ProjLevelMap

                match developmentTarget with 
                | DevelopmentTarget.Plugin plugin ->
                    plugin.Unload()
                | _ -> ()

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

                match developmentTarget with 
                | DevelopmentTarget.Plugin plugin ->
                    plugin.Load()
                    plugin.Calculate()
                | _ -> ()
                    
                createEmpty cache
        | _ -> 
            state




    let processMsg (msg: AutoReloadTmpEmitterMsg) (state: AutoReloadTmpEmitterState) =
        let newState = 
            match msg with 

            | AutoReloadTmpEmitterMsg.DecrCompilingNum number ->
                let compilingNumber = state.CompilingNumber - number

                assert (state.CompilingNumber > 0)
                {state with CompilingNumber = compilingNumber}

            | AutoReloadTmpEmitterMsg.IncrCompilingNum number -> 
                let compilingNumber = state.CompilingNumber + number

                {state with CompilingNumber = compilingNumber } 

            | AutoReloadTmpEmitterMsg.AddTmp projectFile -> 
                let newCompilerTmp = state.CompilerTmp.Add projectFile

                {state with CompilerTmp = newCompilerTmp }        

            | AutoReloadTmpEmitterMsg.AddTask task -> 
                {state with CachedCompilerTasks = task :: state.CachedCompilerTasks} 

            | AutoReloadTmpEmitterMsg.UpdateCache cache ->
                {state with CrackerFsprojFileBundleCache = cache} 


        let msgName = AutoReloadTmpEmitterMsg.msgName msg
        logger.Info "compilerTmpEmitter agent receive message %s,current compiling number is %d" msgName newState.CompilingNumber

        newState

let autoReloadTmpEmitter developmentTarget (initialCache: CrackedFsprojBundleCache)  = MailboxProcessor<AutoReloadTmpEmitterMsg>.Start(fun inbox ->
    logger.Important "FcsWatch is running in autoReload mode"
    let rec loop state = async {

        let! msg = inbox.Receive()
        let newState = AutoReloadTmpEmitterState.processMsg msg state

        match msg with 
        | AutoReloadTmpEmitterMsg.DecrCompilingNum _ ->

            let newState = AutoReloadTmpEmitterState.tryEmit developmentTarget newState

            return! loop newState    

        | _ -> return! loop newState
    }
    loop (AutoReloadTmpEmitterState.createEmpty initialCache) 
)

