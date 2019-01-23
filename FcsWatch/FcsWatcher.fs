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
open System
open System.Diagnostics

type ReplyData =
    { AllCompilerTaskNumber: int 
      CompilerTmp: Set<string> }

[<RequireQualifiedAccess>]
type FcsWatcherMsg =
    | DetectSourceFileChange of fileChange: FileChange * AsyncReplyChannel<int>
    | DetectSourceFileChanges of fileChange: FileChange list * AsyncReplyChannel<int>
    | DetectProjectFileChange of fileChange: FileChange
    | GetEmitterAgent of AsyncReplyChannel<MailboxProcessor<CompilerTmpEmitterMsg>>
    | GetDebuggingServerAgent of AsyncReplyChannel<MailboxProcessor<DebuggingServerMsg>>

let inline (<!>) msg content  = 
    fun replyChannel ->
        msg (content, replyChannel)


type FcsWatcherModel = 
    { SourceFileWatcher: IDisposable
      CrackerFsprojFileBundleCache: CrackerFsprojFileBundleCache }


let fcsWatcher 
    (buildingConfig: Config -> Config) 
    (checker: FSharpChecker) 
    (entryProjectFile: string) = 
        let config = buildingConfig {
                Logger = Logger.Minimal
                WorkingDir = Path.getFullName "./"
                DevelopmentTarget = DevelopmentTarget.Program
            }
        let config = { config with WorkingDir = Path.getFullName config.WorkingDir }        
 
        let agent = MailboxProcessor<FcsWatcherMsg>.Start(fun inbox ->
        
            let newSourceFileWatcher cache = 
                let pattern = 
                    let files = cache.SourceFileMap |> Seq.map (fun pair -> pair.Key) |> List.ofSeq
                    { BaseDirectory = config.WorkingDir
                      Includes = files
                      Excludes = [] }

                pattern |> ChangeWatcher.run (fun changes ->
                    inbox.PostAndAsyncReply (fun reply ->
                        FcsWatcherMsg.DetectSourceFileChanges (List.ofSeq changes,reply)
                    ) 
                    |> Async.StartAsTask
                    |> ignore
                )

            Logger.info (sprintf "fcs watcher is running in logger level %A" config.Logger) config.Logger
            Logger.info (sprintf "fcs watcher's working directory is %s" config.WorkingDir) config.Logger

            let projectBundleAgent = crackedFsprojBundle entryProjectFile
            let cache = projectBundleAgent.PostAndReply CrackedFsprojBundleMsg.Cache
            let projectMap = cache.ProjectMap
            let entry = projectMap.[entryProjectFile]

            let projectFilesWatcher =
                let pattern =
                    let files = cache.AllProjectFiles |> List.ofSeq
                    { BaseDirectory = config.WorkingDir
                      Includes = files
                      Excludes = [] }
                  
                pattern |> ChangeWatcher.run (fun changes ->
                    match List.ofSeq changes with 
                    | [change] -> inbox.Post(FcsWatcherMsg.DetectProjectFileChange change)
                    | _ ->
                        failwith "multiple project files changed at some time" 
                )        
            
            let compilerTmpEmitterAgent = compilerTmpEmitter config cache
            let debuggingServerAgent = debuggingServer compilerTmpEmitterAgent config
            debuggingServerAgent.Post DebuggingServerMsg.StartServer
            let compilerAgent = compiler compilerTmpEmitterAgent cache config checker
            compilerAgent.Post(CompilerMsg.WarmCompile entry)
            
            let rec loop state = async {
                let cache = state.CrackerFsprojFileBundleCache
                let sourceFileMaps = cache.SourceFileMap
                let projectMaps = cache.ProjectMap
                let! msg = inbox.Receive()
                match msg with 
                | FcsWatcherMsg.DetectSourceFileChange (fileChange,replyChannel) ->
                    Logger.important (sprintf "file %s is changed" fileChange.FullPath) config.Logger
                    let projFile = sourceFileMaps.[fileChange.FullPath]
                    compilerAgent.Post(CompilerMsg.CompilerProject projectMaps.[projFile])
                    let allCompilerTaskNumber = 
                        compilerAgent.PostAndReply CompilerMsg.AllCompilerTaskNumber
                    replyChannel.Reply allCompilerTaskNumber                    
                    return! loop state
                | FcsWatcherMsg.DetectSourceFileChanges (fileChanges,replyChannel) ->
                    let projFiles = 
                        fileChanges |> List.map (fun fileChange ->
                            Logger.important (sprintf "file %s is changed" fileChange.FullPath) config.Logger
                            sourceFileMaps.[fileChange.FullPath]
                        )
                        |> List.distinct
                    match projFiles.Length with 
                    | 1 ->
                        let projFile = projFiles |> List.exactlyOne
                        compilerAgent.Post(CompilerMsg.CompilerProject projectMaps.[projFile])
                        let allCompilerTaskNumber = 
                            compilerAgent.PostAndReply CompilerMsg.AllCompilerTaskNumber
                        replyChannel.Reply allCompilerTaskNumber                    
                        return! loop state            
                    | i when i > 1 && i < 5 ->
                        let infos = projFiles |> List.map (fun projFile -> projectMaps.[projFile] ) 
                        compilerAgent.Post(CompilerMsg.CompilerProjects infos)
                        let allCompilerTaskNumber = 
                            compilerAgent.PostAndReply CompilerMsg.AllCompilerTaskNumber
                        replyChannel.Reply allCompilerTaskNumber   
                        return! loop state  

                    | i when i >= 5 ->
                        failwith "more than 5 projects have to be recompiled at some time"                     
                    | 0 -> failwith "Invalid token"   
                    | _ -> failwith "Invalid token"                    

                | FcsWatcherMsg.DetectProjectFileChange fileChange ->
                    Logger.important (sprintf "project file %s is changed" fileChange.FullPath) config.Logger
                    let newCache = projectBundleAgent.PostAndReply (CrackedFsprojBundleMsg.DetectProjectFileChange <!> fileChange)
                    let sourceFileWatcher = 
                        state.SourceFileWatcher.Dispose()
                        newSourceFileWatcher newCache
                    compilerAgent.Post(CompilerMsg.UpdateCache newCache)
                    compilerTmpEmitterAgent.Post(CompilerTmpEmitterMsg.UpdateCache newCache)
                    return! loop { state with SourceFileWatcher = sourceFileWatcher; CrackerFsprojFileBundleCache = newCache }
                | FcsWatcherMsg.GetEmitterAgent replyChannel ->
                    replyChannel.Reply(compilerTmpEmitterAgent)
                    return! loop state          
                | FcsWatcherMsg.GetDebuggingServerAgent replyChannel ->
                    replyChannel.Reply(debuggingServerAgent)
                    return! loop state          
            }
            let sourceFileWatcher = newSourceFileWatcher cache
            loop { SourceFileWatcher = sourceFileWatcher; CrackerFsprojFileBundleCache = cache }
        )

        agent
        
