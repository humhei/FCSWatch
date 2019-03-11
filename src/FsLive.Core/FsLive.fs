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
    { SourceFileWatchers: IDisposable list
      CrackerFsprojBundleCache: CrackedFsprojBundleCache }



/// <summary>create a fslive agent</summary>
/// <param name="entryFileOp">file end with *.fs;*.fsproj;*.fsx;*.fsi; If None then config.Otherflags must be not empty</param>
let fsLive 
    (config: Config) 
    (developmentTarget: DevelopmentTarget<'EmitReply>)
    (checker: FSharpChecker) 
    (entryFileOp: string option) =

        let entryFileOp = entryFileOp |> Option.map (Path.getFullName >> Path.nomarlizeToUnixCompatible)

        let config = { config with WorkingDir = Path.getFullName config.WorkingDir }        
 
        logger <- Logger.create (config.LoggerLevel)

        let agent = MailboxProcessor<FsLiveMsg>.Start(fun inbox ->
        
            let newSourceFileWatchers cache = 
                [
                    let sourceFileWatcher files =
                        let pattern = 
                            { BaseDirectory = config.WorkingDir
                              Includes = files
                              Excludes = [] }

                        ChangeWatcher.run (fun changes ->
                            inbox.PostAndReply (FsLiveMsg.DetectSourceFileChanges <!> (List.ofSeq changes))
                            |> ignore) pattern

                    let sourceFiles = cache.SourceFileMap |> Seq.map (fun pair -> pair.Key) |> List.ofSeq

                    yield 
                        sourceFileWatcher sourceFiles

                    if config.UseEditFiles then 
                        yield
                            sourceFiles 
                            |> List.map (fun sourceFile ->
                                let infoDir, editFile = FileSystem.editDirAndFile sourceFile config
                                editFile
                            )
                            |> sourceFileWatcher
                ]

            logger.Info "fslive is running in logger level %A" config.LoggerLevel
            logger.Info "fslive's working directory is %s" config.WorkingDir

            let crackedProjectBundleAgent = crackedFsprojBundle checker config entryFileOp

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
                        fileChanges |> List.collect (fun fileChange ->
                            logger.ImportantGreen "fslive: detect file %s changed" fileChange.FullPath
                            sourceFileMap.[fileChange.FullPath]
                        )
                        |> List.distinct

                    let crackedFsprojs = projFiles |> List.map (fun projPath -> projectMap.[projPath] ) 

                    replyChannel.Reply (CompilerNumber crackedFsprojs.Length)

                    compilerAgent.Post(CompilerMsg.CompileProjects crackedFsprojs)

                    return! loop state  


                | FsLiveMsg.DetectProjectFileChanges (fileChanges, replyChannel) ->
                    fileChanges |> List.iter (fun fileChange ->
                        logger.ImportantGreen "fslive: detect project file %s changed" fileChange.FullPath
                    )

                    let newCache = crackedProjectBundleAgent.PostAndReply (CrackedFsprojBundleMsg.DetectProjectFileChanges <!> fileChanges)
                    
                    let newSourceFileWatchers = 
                        state.SourceFileWatchers |> List.iter (fun watcher -> watcher.Dispose())
                        newSourceFileWatchers newCache

                    compilerAgent.Post(CompilerMsg.UpdateCache newCache)

                    compilerTmpEmitterAgent.Post(CompilerTmpEmitterMsg.UpdateCache newCache)

                    replyChannel.Reply()

                    return! loop { state with SourceFileWatchers = newSourceFileWatchers; CrackerFsprojBundleCache = newCache }
        
            }
            let sourceFileWatchers = newSourceFileWatchers initialCache
            loop { SourceFileWatchers = sourceFileWatchers; CrackerFsprojBundleCache = initialCache }
        )

        agent
        
