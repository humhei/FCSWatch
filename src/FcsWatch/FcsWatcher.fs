module FcsWatch.FcsWatcher
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.Types
open Fake.IO.Globbing
open Fake.IO
open FcsWatch.DebuggingServer
open FcsWatch.Compiler
open FcsWatch.CompilerTmpEmiiter
open System

type ReplyData =
    { AllCompilerTaskNumber: int 
      CompilerTmp: Set<string> }

type CompilerNumber = CompilerNumber of int

[<RequireQualifiedAccess>]
type FcsWatcherMsg =
    | DetectSourceFileChanges of fileChanges: FileChange list * AsyncReplyChannel<CompilerNumber>
    | DetectProjectFileChanges of fileChanges: FileChange list * AsyncReplyChannel<unit>

let inline (<!>) msg content = 
    fun replyChannel ->
        msg (content, replyChannel)

type FcsWatcherModel = 
    { SourceFileWatcher: IDisposable
      CrackerFsprojBundleCache: CrackedFsprojBundleCache }


let fcsWatcher 
    (buildingConfig: Config -> Config) 
    (checker: FSharpChecker) 
    (entryProjectFile: string) = 
        let entryProjectFile = Path.getFullName entryProjectFile

        let config = buildingConfig {
                LoggerLevel = Logger.Level.Minimal
                WorkingDir = Path.getFullName "./"
                DevelopmentTarget = DevelopmentTarget.Program
            }

        let config = { config with WorkingDir = Path.getFullName config.WorkingDir }        
 
        logger <- Logger.create (config.LoggerLevel)

        let agent = MailboxProcessor<FcsWatcherMsg>.Start(fun inbox ->
        
            let newSourceFileWatcher cache = 
                let pattern = 
                    let files = cache.SourceFileMap |> Seq.map (fun pair -> pair.Key) |> List.ofSeq
                    { BaseDirectory = config.WorkingDir
                      Includes = files
                      Excludes = [] }

                pattern |> ChangeWatcher.run (fun changes ->
                    inbox.PostAndReply (FcsWatcherMsg.DetectSourceFileChanges <!> (List.ofSeq changes))
                    |> ignore
                    ()
                )

            logger.Info "fcs watcher is running in logger level %A" config.LoggerLevel
            logger.Info "fcs watcher's working directory is %s" config.WorkingDir

            let crackedProjectBundleAgent = crackedFsprojBundle entryProjectFile

            let initialCache = crackedProjectBundleAgent.PostAndReply CrackedFsprojBundleMsg.GetCache

            let initialProjectMap = initialCache.ProjectMap

            let entryCrackedFsproj = initialProjectMap.[entryProjectFile]

            let projectFilesWatcher =
                let pattern =

                    let files = initialCache.AllProjectFiles |> List.ofSeq

                    { BaseDirectory = config.WorkingDir
                      Includes = files
                      Excludes = [] }
                  
                pattern |> ChangeWatcher.run (fun changes ->
                    inbox.PostAndReply(FcsWatcherMsg.DetectProjectFileChanges <!> (List.ofSeq changes))
                )        
            
            let compilerTmpEmitterAgent = compilerTmpEmitter config initialCache

            let debuggingServerAgent = debuggingServer compilerTmpEmitterAgent config

            let compilerAgent = compiler entryProjectFile compilerTmpEmitterAgent initialCache config checker

            let rec loop (state: FcsWatcherModel) = async {
                let cache = state.CrackerFsprojBundleCache

                let sourceFileMap = cache.SourceFileMap

                let projectMap = cache.ProjectMap

                let! msg = inbox.Receive()

                match msg with 
                | FcsWatcherMsg.DetectSourceFileChanges (fileChanges, replyChannel) ->

                    let projFiles = 
                        fileChanges |> List.map (fun fileChange ->
                            logger.ImportantGreen "detect file %s changed" fileChange.FullPath
                            sourceFileMap.[fileChange.FullPath]
                        )
                        |> List.distinct

                    let crackedFsprojs = projFiles |> List.map (fun projPath -> projectMap.[projPath] ) 

                    replyChannel.Reply (CompilerNumber crackedFsprojs.Length)

                    compilerAgent.Post(CompilerMsg.CompilerProjects crackedFsprojs)

                    return! loop state  


                | FcsWatcherMsg.DetectProjectFileChanges (fileChanges,replyChannel) ->
                    fileChanges |> List.iter (fun fileChange ->
                        logger.ImportantGreen "detect project file %s changed" fileChange.FullPath
                    )

                    let newCache = crackedProjectBundleAgent.PostAndReply (CrackedFsprojBundleMsg.DetectProjectFileChanges <!> fileChanges)
                    
                    let newSourceFileWatcher = 
                        state.SourceFileWatcher.Dispose()
                        newSourceFileWatcher newCache

                    compilerAgent.Post(CompilerMsg.UpdateCache newCache)

                    compilerTmpEmitterAgent.Post(CompilerTmpEmitterMsg.UpdateCache newCache)

                    replyChannel.Reply()

                    return! loop { state with SourceFileWatcher = newSourceFileWatcher; CrackerFsprojBundleCache = newCache }
        
            }
            let sourceFileWatcher = newSourceFileWatcher initialCache
            loop { SourceFileWatcher = sourceFileWatcher; CrackerFsprojBundleCache = initialCache }
        )

        agent
        
