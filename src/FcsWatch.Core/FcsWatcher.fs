namespace FcsWatch.Core
open FSharp.Compiler.CodeAnalysis
open FcsWatch.Core.FullCrackedFsproj
open Fake.IO.Globbing
open Fake.IO
open FcsWatch.Core.Compiler
open FcsWatch.Core.CompilerTmpEmitter
open System
open System.IO
open System.Threading
open System.Timers



[<RequireQualifiedAccess>]
module private IntervalAccumMailBoxProcessor =
    type State<'accum> =
        { Timer: Timer option
          Accums: 'accum list }

    type Msg<'accum> =
        | Accum of 'accum
        | IntervalUp

    type IntervalAccumMailBoxProcessor<'accum> =
        { Agent: MailboxProcessor<'accum>
          CancellationTokenSource: CancellationTokenSource }

    let create (onChange : 'accum seq -> unit) =

        let cancellationTokenSource = new CancellationTokenSource()
        let agent =
            MailboxProcessor<Msg<'accum>>.Start ((fun inbox ->

                let createTimer() =
                    let timer = new Timer(100.)
                    timer.Start()
                    timer.Elapsed.Add (fun _ ->
                        timer.Stop()
                        timer.Dispose()
                        inbox.Post IntervalUp
                    )
                    timer

                let rec loop state = async {
                    let! msg = inbox.Receive()
                    match msg with
                    | Accum accum ->
                        match state.Timer with
                        | Some timer ->
                            timer.Stop()
                            timer.Dispose()
                        | None -> ()

                        return!
                            loop
                                { state with
                                    Timer = Some (createTimer())
                                    Accums = accum :: state.Accums }

                    | IntervalUp ->
                        onChange state.Accums
                        return! loop { state with Timer = None; Accums = [] }
                }

                loop { Timer = None; Accums = [] }
            ),cancellationTokenSource.Token)

        { CancellationTokenSource = cancellationTokenSource
          Agent = agent }

    let accum value = Msg.Accum value

[<RequireQualifiedAccess>]
module private ChangeWatcher =

    type Options = ChangeWatcher.Options

    let private handleWatcherEvents (status : FileStatus) (onChange : FileChange -> unit) (e : FileSystemEventArgs) =
        onChange ({ FullPath = e.FullPath
                    Name = e.Name
                    Status = status })

    let runWithAgent (foptions:Options -> Options) (onChange : FileChange seq -> unit) (fileIncludes : IGlobbingPattern) =
        let options = foptions { IncludeSubdirectories = true }
        let dirsToWatch = fileIncludes |> GlobbingPattern.getBaseDirectoryIncludes


        let intervalAccumAgent = IntervalAccumMailBoxProcessor.create onChange

        let postFileChange (fileChange: FileChange) =
            if fileIncludes.IsMatch fileChange.FullPath
            then intervalAccumAgent.Agent.Post (IntervalAccumMailBoxProcessor.accum fileChange)

        let watchers =
            dirsToWatch |> List.map (fun dir ->
                               //tracefn "watching dir: %s" dir

                               let watcher = new FileSystemWatcher(Path.getFullName dir, "*.*")
                               watcher.EnableRaisingEvents <- true
                               watcher.IncludeSubdirectories <- options.IncludeSubdirectories
                               watcher.Changed.Add(handleWatcherEvents Changed postFileChange)
                               watcher.Created.Add(handleWatcherEvents Created postFileChange)
                               watcher.Deleted.Add(handleWatcherEvents Deleted postFileChange)
                               watcher.Renamed.Add(fun (e : RenamedEventArgs) ->
                                   postFileChange { FullPath = e.OldFullPath
                                                    Name = e.OldName
                                                    Status = Deleted }
                                   postFileChange { FullPath = e.FullPath
                                                    Name = e.Name
                                                    Status = Created })
                               watcher)

        { new System.IDisposable with
              member this.Dispose() =
                  for watcher in watchers do
                      watcher.EnableRaisingEvents <- false
                      watcher.Dispose()
                  // only dispose the timer if it has been constructed
                  intervalAccumAgent.CancellationTokenSource.Cancel()
                  intervalAccumAgent.CancellationTokenSource.Dispose() }

    let run (onChange : FileChange seq -> unit) (fileIncludes : IGlobbingPattern) = runWithAgent id onChange fileIncludes





module FcsWatcher =


    type CompilerNumber = CompilerNumber of int

    [<RequireQualifiedAccess>]
    type FcsWatcherMsg =
        | DetectSourceFileChanges of fileChanges: FileChange list * incrCompilingNumberReplyChannel: AsyncReplyChannel<unit>
        | DetectProjectFileChanges of fileChanges: FileChange list
        | GetCache of AsyncReplyChannel<CrackedFsprojBundleCache>
        | WaitCompiled of AsyncReplyChannel<CompilerNumber>
        | Dispose of AsyncReplyChannel<unit>
        

    let inline (<!>) msg content =
        fun replyChannel ->
            msg (content, replyChannel)

    type FcsWatcherModel =
        { SourceFileWatcher: IDisposable
          CrackerFsprojBundleCache: CrackedFsprojBundleCache
          ProjectFilesWatcher: IDisposable }

    type IFcsWatcherAndCompilerTmpAgent =
        inherit System.IDisposable
        abstract member Checker: RemotableFSharpChecker
        abstract member GetCache: unit -> CrackedFsprojBundleCache
        abstract member WaitCompiled: unit -> CompilerNumber
        abstract member Agent: MailboxProcessor<FcsWatcherMsg>
        abstract member IsClosed: bool

    /// <param name="entryFileOp">file end with *.fs;*.fsproj;*.fsx;*.fsi; If None then otherflags will be applied</param>
    type FcsWatcherAndCompilerTmpAgent<'CompilerTmpEmitterCustomMsg, 'CompilerTmpEmitterCustomState,'CompilerOrCheckResult when 'CompilerOrCheckResult :> ICompilerOrCheckResult>(
        checker,
        compilerTmpEmitter: ICompilerTmpEmitter<'CompilerTmpEmitterCustomMsg, 'CompilerTmpEmitterCustomState,'CompilerOrCheckResult>,
        compiler: ICompiler<'CompilerOrCheckResult>,
        config: Config,
        entryFileOp: EntryFile option,
        projectFileChangedListener
        ) as this =
            let config = { config with WorkingDir = Path.nomalizeToUnixCompatiable config.WorkingDir }
            let mutable isClosed = false

            do logger <- Logger.create (config.LoggerLevel)

            let fullCrackedFsprojBuilder = FullCrackedFsprojBuilder.create config checker entryFileOp

            do logger.Info "fcs watcher is running in logger level %A" config.LoggerLevel
            do logger.Info "fcs watcher's working directory is %s" config.WorkingDir

            let crackedProjectBundleAgent = crackedFsprojBundle config.UseEditFiles fullCrackedFsprojBuilder

            let initialCache = crackedProjectBundleAgent.PostAndReply CrackedFsprojBundleMsg.GetCache

            let compilerTmpEmitterAgent = compilerTmpEmitterAgent config.WorkingDir compilerTmpEmitter initialCache

            let iCompilerTmpEmitterAgent = MailboxProcessor.toChildInterface CompilerTmpEmitterMsg.customMsg compilerTmpEmitterAgent

            let compilerAgent = compilerAgent compiler compilerTmpEmitterAgent initialCache checker

            let agent = MailboxProcessor<FcsWatcherMsg>.Start(fun inbox ->

                let newSourceFileWatcher cache =
                    let pattern =
                        let files = cache.EditSourceFileMap |> Seq.map (fun pair -> pair.Key) |> List.ofSeq
                        { BaseDirectory = config.WorkingDir
                          Includes = files
                          Excludes = [] }

                    pattern |> ChangeWatcher.run (fun changes ->
                        inbox.PostAndReply (FcsWatcherMsg.DetectSourceFileChanges <!> (List.ofSeq changes))
                    )



                let createProjectFilesWatcher (projectFiles: string seq) =
                    let pattern =

                        let files = projectFiles |> List.ofSeq

                        { BaseDirectory = config.WorkingDir
                          Includes = files
                          Excludes = [] }

                    pattern |> ChangeWatcher.run (fun changes ->
                        inbox.Post(FcsWatcherMsg.DetectProjectFileChanges (List.ofSeq changes))
                    )


                let rec loop (state: FcsWatcherModel) = async {
                    let cache = state.CrackerFsprojBundleCache

                    let editSourceFileMap = cache.EditSourceFileMap

                    let projectMap = cache.ProjectMap

                    let! msg = inbox.Receive()

                    match msg with
                    | FcsWatcherMsg.DetectSourceFileChanges (fileChanges, replyChannel) ->

                        let projFiles =
                            fileChanges
                            |> List.distinctBy (fun fileChange -> fileChange.FullPath)
                            |> List.collect (fun fileChange ->
                                logger.ImportantGreen "detect file %s changed" fileChange.FullPath
                                editSourceFileMap.[fileChange.FullPath]
                            )
                            |> List.distinct

                        let crackedFsprojs = projFiles |> List.map (fun projPath -> projectMap.[projPath] )

                        compilerAgent.PostAndReply(fun replyChannel ->
                            CompilerMsg.CompileProjects (WhyCompile.DetectFileChange, crackedFsprojs, replyChannel)
                        )

                        replyChannel.Reply()

                        return! loop state


                    | FcsWatcherMsg.DetectProjectFileChanges (fileChanges) ->
                        fileChanges |> List.iter (fun fileChange ->
                            logger.ImportantGreen "detect project file %s changed" fileChange.FullPath
                        )

                        let newCache = crackedProjectBundleAgent.PostAndReply (CrackedFsprojBundleMsg.DetectProjectFileChanges <!> fileChanges)

                        let newSourceFileWatcher =
                            state.SourceFileWatcher.Dispose()
                            newSourceFileWatcher newCache

                        let newProjectFilesWatcher =
                            state.ProjectFilesWatcher.Dispose()
                            createProjectFilesWatcher newCache.AllProjectFiles

                        compilerAgent.Post(CompilerMsg.UpdateCache newCache)

                        compilerTmpEmitterAgent.Post(CompilerTmpEmitterMsg.updateCache newCache)

                        let newState =
                            { state with 
                                SourceFileWatcher = newSourceFileWatcher;
                                CrackerFsprojBundleCache = newCache
                                ProjectFilesWatcher = newProjectFilesWatcher }
                        projectFileChangedListener this
                        return! loop newState

                    | FcsWatcherMsg.GetCache replyChannel ->
                        replyChannel.Reply(cache)
                        return! loop state

                    | FcsWatcherMsg.WaitCompiled replyChannel ->
                        let compilerNumber = compilerTmpEmitterAgent.PostAndReply CompilerTmpEmitterMsg.waitCompiled
                        replyChannel.Reply (CompilerNumber compilerNumber)
                        return! loop state

                        
                    | FcsWatcherMsg.Dispose replyChannel ->
                        match checker with 
                        | RemotableFSharpChecker.FSharpChecker checker -> 
                            checker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients()
                        | RemotableFSharpChecker.Remote  _ -> ()

                        state.SourceFileWatcher.Dispose()
                        state.ProjectFilesWatcher.Dispose()
                        replyChannel.Reply()
                        return! loop state

                }
                let sourceFileWatcher = newSourceFileWatcher initialCache
                let projectFilesWatcher = createProjectFilesWatcher (initialCache.AllProjectFiles)

                logger.Important "Waiting for changes... press CTRL+C to exit"

                loop { SourceFileWatcher = sourceFileWatcher; CrackerFsprojBundleCache = initialCache; ProjectFilesWatcher = projectFilesWatcher }

            )

            member x.Agent = agent

            member x.CompilerTmpEmitterAgent = iCompilerTmpEmitterAgent

            member x.GetCache() =
                agent.PostAndReply(FcsWatcherMsg.GetCache)

            member x.WaitCompiled() =
                agent.PostAndReply(FcsWatcherMsg.WaitCompiled)

            member x.Dispose() =
                agent.PostAndReply(FcsWatcherMsg.Dispose)
                agent.Dispose()
                iCompilerTmpEmitterAgent.Dispose()
                compilerAgent.Dispose()
                isClosed <- true

            member x.IsClosed = isClosed

            member x.Checker = checker

            interface System.IDisposable with 
                member x.Dispose() = x.Dispose()

            interface IFcsWatcherAndCompilerTmpAgent with 
                member x.GetCache() = x.GetCache()
                member x.WaitCompiled() = x.WaitCompiled()
                member x.Agent = x.Agent
                member x.Checker = checker
                member x.IsClosed = x.IsClosed

    let fcsWatcherAndCompilerTmpAgent
        checker
        compilerTmpEmitter
        compiler
        (config: Config)
        (entryFileOp: EntryFile option)
        (projectFileChangedEventLister: IFcsWatcherAndCompilerTmpAgent -> unit)
        = new FcsWatcherAndCompilerTmpAgent<_, _, _>(checker, compilerTmpEmitter, compiler, config, entryFileOp, projectFileChangedEventLister)