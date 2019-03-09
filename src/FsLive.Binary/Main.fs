namespace FsLive.Binary
open FsLive.Core.FsLive
open FsLive.Core.CompilerTmpEmiiter
open FsLive.Core.CrackedFsproj
open FsLive.Core
open Suave
open System.Threading
open DebuggingServer
open FsLive.Core.FullCrackedFsproj

module Main =



    type Plugin =
        { Load: unit -> unit
          Unload: unit -> unit
          Calculate: unit -> unit
          DebuggerAttachTimeDelay: int }

    [<RequireQualifiedAccess>]
    type DevelopmentTarget =
        | Plugin of Plugin
        | Program

    type Config = 
        { DevelopmentTarget: DevelopmentTarget 
          LoggerLevel: Logger.Level
          OtherFlags: string list
          UseEditFiles: bool
          WorkingDir: string }

    with 
        static member DefaultValue = 
            let coreConfig = FsLive.Core.Config.DeafultValue
            { DevelopmentTarget = DevelopmentTarget.Program
              LoggerLevel = coreConfig.LoggerLevel
              OtherFlags = coreConfig.OtherFlags
              UseEditFiles = coreConfig.UseEditFiles
              WorkingDir = coreConfig.WorkingDir }

        member x.AsCore: FsLive.Core.Config = 
            { LoggerLevel = x.LoggerLevel
              OtherFlags = x.OtherFlags
              UseEditFiles = x.UseEditFiles
              WorkingDir = x.WorkingDir
              BuildingFSharpProjectOptions = id
              WarmCompile = true
              Eval = false }

    let private compile _ checker crackedFsProj = async {
        let! compileResults = CrackedFsproj.compile checker crackedFsProj
        return 
            compileResults |> Array.map CompileOrCheckResult.CompileResult
    } 
    
    let private tryEmit binaryDevelopmentTarget (logger: Logger.Logger) compilerTmpEmiiterState =
        let cache = compilerTmpEmiiterState.CrackerFsprojFileBundleCache

        logger.Info "tryEmitAction: current emitReplyChannels number is %d" compilerTmpEmiiterState.EmitReplyChannels.Length

        match compilerTmpEmiiterState.CompilingNumber,compilerTmpEmiiterState.EmitReplyChannels with 
        | 0, h::t ->
            let replySuccess() = h.Reply (Successful.OK "fcswatch: Ready to debug") 

            let replyFailure errorText = h.Reply (RequestErrors.BAD_REQUEST errorText)

            logger.Info "Current cached compier task is %d" compilerTmpEmiiterState.CompilerTasks.Length

            match compilerTmpEmiiterState.CompilerTasks with
            | [] ->
                replySuccess()

                match binaryDevelopmentTarget with 
                | DevelopmentTarget.Plugin plugin ->
                    Thread.Sleep(plugin.DebuggerAttachTimeDelay)

                    plugin.Calculate()
                | _ -> ()

                compilerTmpEmiiterState
            | _ ->
                let lastTasks = 
                    compilerTmpEmiiterState.CompilerTasks 
                    |> List.groupBy (fun compilerTask ->
                        compilerTask.Task.Result.[0].ProjPath
                    )
                    |> List.map (fun (projPath, compilerTasks) ->
                        compilerTasks |> List.maxBy (fun compilerTask -> compilerTask.StartTime)
                    )

                let allResults = lastTasks |> List.collect (fun task -> task.Task.Result)

                match List.tryFind CompileOrCheckResult.isFail allResults with 
                | Some result ->
                    let errorText =  
                        result.Errors 
                        |> Seq.map (fun error -> error.ToString())
                        |> String.concat "\n"

                    replyFailure errorText
                    { compilerTmpEmiiterState with EmitReplyChannels = [] } 

                | None ->

                    let projRefersMap = cache.ProjRefersMap

                    let projLevelMap = cache.ProjLevelMap

                    match binaryDevelopmentTarget with 
                    | DevelopmentTarget.Plugin plugin ->
                        plugin.Unload()
                    | _ -> ()

                    compilerTmpEmiiterState.CompilerTmp
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

                    replySuccess()

                    match binaryDevelopmentTarget with 
                    | DevelopmentTarget.Plugin plugin ->
                        plugin.Load()
                        plugin.Calculate()
                    | _ -> ()
                
                    CompilerTmpEmiiterState.createEmpty cache 
        | _ -> compilerTmpEmiiterState

    let private developmentTarget binaryDevelopmentTarget =
        { CompileOrCheck = compile 
          TryEmit = tryEmit binaryDevelopmentTarget
          StartDebuggingServer = 
            fun config compilerTmpEmitterAgent -> debuggingServer config compilerTmpEmitterAgent |> ignore }

    let fsLive (buildingConfig: Config -> Config) checker entryProjectFile =
        let config = buildingConfig Config.DefaultValue
        fsLive config.AsCore (developmentTarget config.DevelopmentTarget) checker entryProjectFile
