[<RequireQualifiedAccess>]
module FcsWatch.DebuggingServer 
open Fake.IO
open System.Net
open Fake.IO.FileSystemOperators
open System.Text
open Types
open Suave.Filters
open Suave.Operators
open Suave
open System.Net.Sockets

type PluginDebugInfo =
    { DebuggerAttachTimeDelay: int 
      Pid: int 
      VscodeLaunchConfigurationName: string }

type Plugin =
    { Load: unit -> unit
      Unload: unit -> unit
      Calculate: unit -> unit
      PluginDebugInfo: PluginDebugInfo }

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

let private generateCurlCache freePort workingDir =         
    let cacheDir = workingDir </> ".fake" </> "fcswatch"

    let fileName = cacheDir </> "port.cache"

    Directory.ensure cacheDir

    let lines = 
        [ sprintf "url: http://localhost:%d/emitCompilerTmp" freePort
          "-f" ]

    File.writeWithEncoding Encoding.ASCII false fileName lines

[<RequireQualifiedAccess>]
type TmpEmitterMsg =
    | EmitCompilerTmp of replyChannel: AsyncReplyChannel<WebPart>
    | AutoReload of AutoReload.TmpEmitterMsg

let upcastMsg msg = TmpEmitterMsg.AutoReload msg

type TmpEmitterState  =
    { AutoReloadTmpEmitterState: AutoReload.TmpEmitterState
      EmitReplyChannels: AsyncReplyChannel<WebPart> list }

with 
    member x.CompilingNumber = x.AutoReloadTmpEmitterState.CompilingNumber
    
    member x.CachedCompilerTasks = x.AutoReloadTmpEmitterState.CachedCompilerTasks

    member x.CompilerTmp = x.AutoReloadTmpEmitterState.CompilerTmp

    member x.CrackerFsprojFileBundleCache = x.AutoReloadTmpEmitterState.CrackerFsprojFileBundleCache


[<RequireQualifiedAccess>]
module TmpEmitterState =
    open System.Threading

    let createEmpty cache =
        { AutoReloadTmpEmitterState = AutoReload.TmpEmitterState.createEmpty cache 
          EmitReplyChannels = [] }

    let mapAutoReloadTmpEmitterState mapping (tmpEmitterState: TmpEmitterState) =
        { tmpEmitterState with 
            AutoReloadTmpEmitterState = mapping tmpEmitterState.AutoReloadTmpEmitterState }

    let setCompilingNumber number (tmpEmitterState: TmpEmitterState) =
        mapAutoReloadTmpEmitterState (AutoReload.TmpEmitterState.setCompilingNumber number) tmpEmitterState

    let tryEmit (developmentTarget: DevelopmentTarget) (tmpEmitterState: TmpEmitterState) =
        let cache = tmpEmitterState.CrackerFsprojFileBundleCache
        logger.Info "tryEmitAction: current emitReplyChannels number is %d" tmpEmitterState.EmitReplyChannels.Length

        match tmpEmitterState.CompilingNumber, tmpEmitterState.EmitReplyChannels with 
        | 0, h::t ->
            let replySuccess() = h.Reply (Successful.OK "fcswatch: Ready to debug") 

            let replyFailure errorText = h.Reply (RequestErrors.BAD_REQUEST errorText)

            logger.Info "Current valid compier task is %d" tmpEmitterState.CachedCompilerTasks.Length

            match tmpEmitterState.CachedCompilerTasks with
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
                    tmpEmitterState.CachedCompilerTasks 
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
                    { tmpEmitterState with EmitReplyChannels = [] } 

                | None ->

                    let projRefersMap = cache.ProjRefersMap

                    let projLevelMap = cache.ProjLevelMap

                    match developmentTarget with 
                    | DevelopmentTarget.Plugin plugin ->
                        plugin.Unload()
                    | _ -> ()

                    tmpEmitterState.CompilerTmp
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

                    match developmentTarget with 
                    | DevelopmentTarget.Plugin plugin ->
                        plugin.Load()
                        plugin.Calculate()
                    | _ -> ()
                    
                    createEmpty cache

        | _ -> tmpEmitterState


[<RequireQualifiedAccess>]
module DebuggingServer =
    [<AutoOpen>]
    module internal VscodeHelper =

        type Configuration =
            { name: string
              ``type``: string
              request: string
              preLaunchTask: obj
              program: obj
              args: obj
              processId: obj
              justMyCode: obj
              cwd: obj
              stopAtEntry: obj
              console: obj }

        type Launch =
            { version: string
              configurations: Configuration list }

        [<RequireQualifiedAccess>]
        module Launch =
            open Newtonsoft.Json

            let read file =

                let jsonTest = File.readAsString file
                JsonConvert.DeserializeObject<Launch> jsonTest

            let write file (launch: Launch) =
                let settings = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
                let jsonText = JsonConvert.SerializeObject(launch,Formatting.Indented,settings)
                File.writeString false file jsonText

            let writePid configurationName pid (launch: Launch) =
                { launch with
                    configurations =
                        launch.configurations
                        |> List.map (fun configuration ->
                            if configuration.request = "attach" && configuration.name = configurationName then
                                {configuration with processId = pid}
                            else configuration
                        ) }

    let writePidForPlugin developmentTarget root =
        match developmentTarget with 
        | DevelopmentTarget.Program -> ()
        | DevelopmentTarget.Plugin plugin ->
            let file = root </> ".vscode" </> "launch.json"
            if File.exists file then 
                let pluginDebugInfo = plugin.PluginDebugInfo
                let launch = Launch.read file
                Launch.writePid pluginDebugInfo.VscodeLaunchConfigurationName pluginDebugInfo.Pid launch
                |> ignore

let create developmentTarget cache workingDir = MailboxProcessor<TmpEmitterMsg>.Start(fun inbox ->
    DebuggingServer.writePidForPlugin developmentTarget workingDir

    logger.Important "FcsWatch is running in debuggable mode \n Added launch settings in vscode to debug it"
    async {
        let webApp =
            let emitCompilerTmp: WebPart = 
                fun (ctx : HttpContext) ->
                    async {
                        let! handler = 
                            inbox.PostAndAsyncReply(fun replyChannel ->
                                TmpEmitterMsg.EmitCompilerTmp replyChannel
                            )
                        return! handler ctx  
                    }

            choose [
                path "/emitCompilerTmp" >=> emitCompilerTmp
            ]
        let config =

            let freePort = freePort()
            generateCurlCache freePort workingDir
            let local = Suave.Http.HttpBinding.createSimple HTTP "127.0.0.1" freePort

            { defaultConfig with bindings = [local] }

        startWebServer config webApp
    } |> Async.Start

    let rec loop state = async {
        let! msg = inbox.Receive()
        match msg with 
        | TmpEmitterMsg.EmitCompilerTmp replyChannel ->
            let newState =
                { state with 
                    EmitReplyChannels = replyChannel :: state.EmitReplyChannels }
                |> TmpEmitterState.tryEmit developmentTarget

            return! loop newState

        | TmpEmitterMsg.AutoReload autoReloadMsg ->
            let newState = 
                { state with 
                    AutoReloadTmpEmitterState = AutoReload.TmpEmitterState.processMsg autoReloadMsg state.AutoReloadTmpEmitterState }
            match autoReloadMsg with 
            | AutoReload.TmpEmitterMsg.DecrCompilingNum _ ->
                return! loop (TmpEmitterState.tryEmit developmentTarget newState)
            | _ -> return! loop newState

    }
    loop (TmpEmitterState.createEmpty cache)
) 
