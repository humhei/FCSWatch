module FcsWatch.FcsWatcher
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.Types
open Fake.IO.Globbing
open Fake.IO
open Atrous.Core
open FcsWatch.DebuggingServer
open FcsWatch.Compiler
open FcsWatch.CompilerTmpEmiiter
open System.Threading

type FcsWatcherMsg =
    | DetectFileChange of fileChange: FileChange
let fcsWatcher 
    (buildingConfig: Config -> Config) 
    (checker: FSharpChecker) 
    (projectFile: string) = 
        let config = buildingConfig {
                Logger = Logger.Normal
                WorkingDir = Path.getFullName "./"
            }

        let projectBundleAgent = crackedFsprojBundle projectFile
        let cache = projectBundleAgent.PostAndReply CrackedFsprojBundleMsg.Cache
        let projectMaps = cache.ProjectMaps
        let entry = projectMaps.[projectFile]
        let agent = MailboxProcessor<FcsWatcherMsg>.Start(fun inbox ->
            Logger.info (sprintf "fcs watcher is running in logger level %A" config.Logger) config.Logger
            Logger.info (sprintf "fcs watcher's working directory is %s" config.WorkingDir) config.Logger
            let compilerTmpEmitterAgent = compilerTmpEmitter config projectBundleAgent
            let debuggingServerAgent = debuggingServer compilerTmpEmitterAgent config
            debuggingServerAgent.Post DebuggingServerMsg.StartServer
            let compilerAgent = compiler compilerTmpEmitterAgent projectBundleAgent config checker
            compilerAgent.Post(CompilerMsg.WarmCompile entry)
            let rec loop state = async {
                let! msg = inbox.Receive()
                match msg with 
                | FcsWatcherMsg.DetectFileChange fileChange ->
                    Logger.important (sprintf "file %s is changed" fileChange.FullPath) config.Logger
                    compilerAgent.Post(CompilerMsg.DetectFileChange fileChange)
                    return! loop state
            }
            loop ()
        )

        let pattern = 
            let files = cache.SourceFileMaps |> Seq.map (fun pair -> pair.Key) |> List.ofSeq
            { BaseDirectory = config.WorkingDir
              Includes = files
              Excludes = [] }

        pattern |> ChangeWatcher.run (fun changes ->
            match List.ofSeq changes with 
            | [change] -> agent.Post(FcsWatcherMsg.DetectFileChange change)
            | _ ->
                failwith "multiple files changed at some time" 
        )
        
