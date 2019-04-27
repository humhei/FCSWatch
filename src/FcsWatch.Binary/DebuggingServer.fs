[<RequireQualifiedAccess>]
module FcsWatch.Binary.DebuggingServer 
open Fake.IO
open System.Net
open Fake.IO.FileSystemOperators
open System.Text
open FcsWatch.Core
open Suave.Filters
open Suave.Operators
open Suave
open System.Net.Sockets
open FcsWatch.Core.CompilerTmpEmitter
open Extensions
open FcsWatch.Core.Types
open VscodeHelper

type Plugin =
    { Load: unit -> unit
      Unload: unit -> unit
      Calculate: unit -> unit
      TimeDelayAfterUninstallPlugin: int
      PluginDebugInfo: PluginDebugInfo }

[<RequireQualifiedAccess>]
module Plugin =
    let asAutoReloadPlugin (plugin: Plugin): AutoReload.Plugin =
        { Load = plugin.Load
          Unload = plugin.Unload
          Calculate = plugin.Calculate
          TimeDelayAfterUninstallPlugin = plugin.TimeDelayAfterUninstallPlugin
          PluginDebugInfo = Some plugin.PluginDebugInfo }

[<RequireQualifiedAccess>]
type DevelopmentTarget =
    | Program
    | Plugin of Plugin


let private freePort() =
    let listener = new TcpListener(IPAddress.Any, 0);
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

let private generateCurlCache freePort root =         
    let cacheDir = root </> ".fake" </> "fcswatch"

    let fileName = cacheDir </> "port.cache"

    Directory.ensure cacheDir

    let lines = 
        [ sprintf "url: http://localhost:%d/emitCompilerTmp" freePort
          "-f" ]

    File.writeWithEncoding Encoding.ASCII false fileName lines


type TmpEmitterMsg = TmpEmitterMsg of replyChannel: AsyncReplyChannel<WebPart>
type TmpEmitterState = CompilerTmpEmitterState<AsyncReplyChannel<WebPart> list, CompilerResult>


[<RequireQualifiedAccess>]
module internal TmpEmitterState =
    open System.Threading

    let tryEmit (developmentTarget: DevelopmentTarget) (tmpEmitterState: TmpEmitterState) =
        let emitReplyChannels = tmpEmitterState.CustomState
        let commonState = tmpEmitterState.CommonState
        let cache = commonState.CrackerFsprojFileBundleCache
        logger.Info "tryEmitAction: current emitReplyChannels number is %d" emitReplyChannels.Length

        match commonState.CompilingNumber, emitReplyChannels with 
        | 0, h::t ->
            let replySuccess() = h.Reply (Successful.OK "fcswatch: Ready to debug") 

            let replyFailure errorText = h.Reply (RequestErrors.BAD_REQUEST errorText)

            logger.Info "Current valid compier task is %d" commonState.CachedCompilerTasks.Length

            match commonState.CachedCompilerTasks with
            | [] ->
                replySuccess()

                match developmentTarget with 
                | DevelopmentTarget.Plugin plugin ->
                    Thread.Sleep(plugin.PluginDebugInfo.DebuggerAttachTimeDelay)
                    plugin.Calculate()
                | _ -> ()

                tmpEmitterState
            | _ ->
                let lastTasks = 
                    commonState.CachedCompilerTasks 
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

                    replyFailure errorText
                    { tmpEmitterState with CustomState = [] } 

                | None ->

                    let projRefersMap = cache.ProjRefersMap

                    let projLevelMap = cache.ProjLevelMap

                    match developmentTarget with 
                    | DevelopmentTarget.Plugin plugin ->
                        plugin.Unload()
                        Thread.Sleep plugin.TimeDelayAfterUninstallPlugin
                    | _ -> ()

                    commonState.CompilerTmp
                    |> Seq.sortByDescending (fun projPath ->
                        projLevelMap.[projPath]
                    )
                    |> Seq.iter (fun projPath ->
                        let currentCrackedFsproj = cache.ProjectMap.[projPath]

                        CrackedFsproj.copyObjToBin Configuration.Debug currentCrackedFsproj

                        let refCrackedFsprojs = projRefersMap.[projPath]

                        refCrackedFsprojs |> Seq.sortByDescending (fun refCrackedFsproj ->
                            projLevelMap.[refCrackedFsproj.ProjPath]
                        )
                        |> Seq.iter (CrackedFsproj.copyFileFromRefDllToBin Configuration.Debug projPath)
                    )

                    replySuccess()

                    match developmentTarget with 
                    | DevelopmentTarget.Plugin plugin ->
                        plugin.Load()
                        plugin.Calculate()
                    | _ -> ()
                    
                    CompilerTmpEmitterState.createEmpty [] cache

        | _ -> tmpEmitterState

let startServer workingDir (compileTmpEmitterAgent: IMailboxProcessor<TmpEmitterMsg>) =
    let root = 
        File.tryFindRootUpByLaunchJson workingDir 
        |> Option.defaultValue workingDir
    
    async {

        let webApp =
            let emitCompilerTmp: WebPart = 
                fun (ctx : HttpContext) ->
                    async {
                        let! handler = 
                            /// will finnaly invoke tryEmit
                            compileTmpEmitterAgent.PostAndAsyncReply TmpEmitterMsg
                        return! handler ctx  
                    }

            choose [
                path "/emitCompilerTmp" >=> emitCompilerTmp
            ]
        let config =
            let freePort = freePort()
            generateCurlCache freePort root 
            let local = Suave.Http.HttpBinding.createSimple HTTP "127.0.0.1" freePort

            { defaultConfig with bindings = [local] }

        startWebServer config webApp
    } |> Async.Start

let create developmentTarget = 
    { new ICompilerTmpEmitter<_, _, CompilerResult> with 
        member x.TryEmit(workingDir, state) = TmpEmitterState.tryEmit developmentTarget state
        member x.ProcessCustomMsg (customMsg, state) = 
            let (TmpEmitterMsg replyChannel) = customMsg
            { state with 
                CustomState = replyChannel :: state.CustomState }
            |> TmpEmitterState.tryEmit developmentTarget

        member x.CustomInitialState = [] }
        


