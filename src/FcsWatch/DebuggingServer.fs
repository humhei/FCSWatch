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
open AutoReload


let freePort() =
    let listener = new TcpListener(IPAddress.Any, 0);
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

let generateCurlCache freePort workingDir =         
    let cacheDir = workingDir </> ".fake" </> "fcswatch"

    let fileName = cacheDir </> "port.cache"

    Directory.ensure cacheDir

    let lines = 
        [ sprintf "url: http://localhost:%d/emitCompilerTmp" freePort
          "-f" ]

    File.writeWithEncoding Encoding.ASCII false fileName lines

[<RequireQualifiedAccess>]
type DebuggingServerMsg =
    | EmitCompilerTmp of replyChannel: AsyncReplyChannel<WebPart>
    | AutoReload of AutoReloadTmpEmitterMsg

let (!^) msg = DebuggingServerMsg.AutoReload msg

type DebuggingServerState =
    { AutoReloadTmpEmitterState: AutoReloadTmpEmitterState
      EmitReplyChannels: AsyncReplyChannel<WebPart> list }

with 
    member x.CompilingNumber = x.AutoReloadTmpEmitterState.CompilingNumber
    
    member x.CachedCompilerTasks = x.AutoReloadTmpEmitterState.CachedCompilerTasks

    member x.CompilerTmp = x.AutoReloadTmpEmitterState.CompilerTmp

    member x.CrackerFsprojFileBundleCache = x.AutoReloadTmpEmitterState.CrackerFsprojFileBundleCache


[<RequireQualifiedAccess>]
module DebuggingServerState =
    open System.Threading

    let createEmpty cache =
        { AutoReloadTmpEmitterState = AutoReloadTmpEmitterState.createEmpty cache 
          EmitReplyChannels = [] }

    let mapAutoReloadTmpEmitterState mapping (debuggingServerState: DebuggingServerState) =
        { debuggingServerState with 
            AutoReloadTmpEmitterState = mapping debuggingServerState.AutoReloadTmpEmitterState }

    let setCompilingNumber number (debuggingServerState: DebuggingServerState) =
        mapAutoReloadTmpEmitterState (AutoReloadTmpEmitterState.setCompilingNumber number) debuggingServerState

    let tryEmit developmentTarget (debuggingServerState: DebuggingServerState) =
        let cache = debuggingServerState.CrackerFsprojFileBundleCache
        logger.Info "tryEmitAction: current emitReplyChannels number is %d" debuggingServerState.EmitReplyChannels.Length

        match debuggingServerState.CompilingNumber, debuggingServerState.EmitReplyChannels with 
        | 0, h::t ->
            let replySuccess() = h.Reply (Successful.OK "fcswatch: Ready to debug") 

            let replyFailure errorText = h.Reply (RequestErrors.BAD_REQUEST errorText)

            logger.Info "Current valid compier task is %d" debuggingServerState.CachedCompilerTasks.Length

            match debuggingServerState.CachedCompilerTasks with
            | [] ->
                replySuccess()

                match developmentTarget with 
                | DevelopmentTarget.Plugin plugin ->
                    match plugin.PluginDebugInfo with 
                    | Some pluginDebugInfo ->
                        Thread.Sleep(pluginDebugInfo.DebuggerAttachTimeDelay)
                    | None -> ()

                    plugin.Calculate()
                | _ -> ()

                debuggingServerState
            | _ ->
                let lastTasks = 
                    debuggingServerState.CachedCompilerTasks 
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
                    { debuggingServerState with EmitReplyChannels = [] } 

                | None ->

                    let projRefersMap = cache.ProjRefersMap

                    let projLevelMap = cache.ProjLevelMap

                    match developmentTarget with 
                    | DevelopmentTarget.Plugin plugin ->
                        plugin.Unload()
                    | _ -> ()

                    debuggingServerState.CompilerTmp
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

        | _ -> debuggingServerState


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
                match plugin.PluginDebugInfo with 
                | Some pluginDebugInfo ->
                    let launch = Launch.read file
                    Launch.writePid pluginDebugInfo.VscodeLaunchConfigurationName pluginDebugInfo.Pid launch
                    |> ignore
                | None -> ()

let debuggingServer developmentTarget cache workingDir = MailboxProcessor<DebuggingServerMsg>.Start(fun inbox ->
    DebuggingServer.writePidForPlugin developmentTarget workingDir

    logger.Important "FcsWatch is running in debuggable mode \n Added launch settings in vscode to debug it"
    async {
        let webApp =
            let emitCompilerTmp: WebPart = 
                fun (ctx : HttpContext) ->
                    async {
                        let! handler = 
                            inbox.PostAndAsyncReply(fun replyChannel ->
                                DebuggingServerMsg.EmitCompilerTmp replyChannel
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
        | DebuggingServerMsg.EmitCompilerTmp replyChannel ->
            let newState =
                { state with 
                    EmitReplyChannels = replyChannel :: state.EmitReplyChannels }
                |> DebuggingServerState.tryEmit developmentTarget

            return! loop newState

        | DebuggingServerMsg.AutoReload autoReloadMsg ->
            let newState = 
                { state with 
                    AutoReloadTmpEmitterState = AutoReloadTmpEmitterState.processMsg autoReloadMsg state.AutoReloadTmpEmitterState }
            match autoReloadMsg with 
            | AutoReloadTmpEmitterMsg.DecrCompilingNum _ ->
                return! loop (DebuggingServerState.tryEmit developmentTarget newState)
            | _ -> return! loop newState

    }
    loop (DebuggingServerState.createEmpty cache)
) 
