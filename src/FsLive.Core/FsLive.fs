module FsLive.Core.FsLive
open FSharp.Compiler.SourceCodeServices
open FsLive.Core.CrackedFsprojBundle
open Fake.IO.Globbing
open Fake.IO
open FsLive.Core.Compiler
open FsLive.Core.CompilerTmpEmiiter
open System

type CompilerNumber = CompilerNumber of int

[<RequireQualifiedAccess>]
type FsLiveMsg =
    | DetectSourceFileChanges of fileChanges: FileChange list * AsyncReplyChannel<CompilerNumber>
    | DetectProjectFileChanges of fileChanges: FileChange list * AsyncReplyChannel<unit>

let inline (<!>) msg content = 
    fun replyChannel ->
        msg (content, replyChannel)

type FsLiveModel = 
    { SourceFileWatcher: IDisposable
      CrackerFsprojBundleCache: CrackedFsprojBundleCache }


let fsLive 
    (buildingConfig: Config -> Config) 
    (developmentTarget: DevelopmentTarget<'EmitReply>)
    (checker: FSharpChecker) 
    (entryProjectFileOp: string option) =

        let entryProjectFileOp = entryProjectFileOp |> Option.map (Path.getFullName >> Path.nomarlizeToUnixCompatible)

        let config = 
            Config.DeafultValue
            |> buildingConfig

        let config = { config with WorkingDir = Path.getFullName config.WorkingDir }        
 
        logger <- Logger.create (config.LoggerLevel)

        let agent = MailboxProcessor<FsLiveMsg>.Start(fun inbox ->
        
            let newSourceFileWatcher cache = 
                let pattern = 
                    let files = cache.SourceFileMap |> Seq.map (fun pair -> pair.Key) |> List.ofSeq
                    { BaseDirectory = config.WorkingDir
                      Includes = files
                      Excludes = [] }

                pattern |> ChangeWatcher.run (fun changes ->
                    inbox.PostAndReply (FsLiveMsg.DetectSourceFileChanges <!> (List.ofSeq changes))
                    |> ignore
                    ()
                )

            logger.Info "fcs watcher is running in logger level %A" config.LoggerLevel
            logger.Info "fcs watcher's working directory is %s" config.WorkingDir

            let crackedProjectBundleAgent = crackedFsprojBundle checker config entryProjectFileOp

            let initialCache = crackedProjectBundleAgent.PostAndReply CrackedFsprojBundleMsg.GetCache

            let projectFilesWatcher =
                let pattern =

                    let files = initialCache.AllProjectFiles |> List.ofSeq

                    { BaseDirectory = config.WorkingDir
                      Includes = files
                      Excludes = [] }
                  
                pattern |> ChangeWatcher.run (fun changes ->
                    inbox.PostAndReply(FsLiveMsg.DetectProjectFileChanges <!> (List.ofSeq changes))
                )        
            
            let compilerTmpEmitterAgent = compilerTmpEmitter developmentTarget config initialCache

            let compilerAgent = compiler developmentTarget initialCache.EntryProjectFile compilerTmpEmitterAgent initialCache config checker

            let rec loop (state: FsLiveModel) = async {
                let cache = state.CrackerFsprojBundleCache

                let sourceFileMap = cache.SourceFileMap

                let projectMap = cache.ProjectMap

                let! msg = inbox.Receive()

                match msg with 
                | FsLiveMsg.DetectSourceFileChanges (fileChanges, replyChannel) ->

                    let projFiles = 
                        fileChanges |> List.map (fun fileChange ->
                            logger.ImportantGreen "detect file %s changed" fileChange.FullPath
                            sourceFileMap.[fileChange.FullPath]
                        )
                        |> List.distinct

                    let crackedFsprojs = projFiles |> List.map (fun projPath -> projectMap.[projPath] ) 

                    replyChannel.Reply (CompilerNumber crackedFsprojs.Length)

                    compilerAgent.Post(CompilerMsg.CompileProjects crackedFsprojs)

                    return! loop state  


                | FsLiveMsg.DetectProjectFileChanges (fileChanges,replyChannel) ->
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
        
