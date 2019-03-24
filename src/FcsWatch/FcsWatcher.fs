namespace FcsWatch
open FSharp.Compiler.SourceCodeServices
open FcsWatch.Types
open Fake.IO.Globbing
open Fake.IO
open FcsWatch.Compiler
open FcsWatch.CompilerTmpEmitter
open System
open System.IO
open System.Threading
open System.Timers


[<RequireQualifiedAccess>]
module IntervalAccumMailBoxProcessor =
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
module ChangeWatcher =

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




type Config =
    { LoggerLevel: Logger.Level
      WorkingDir: string
      DevelopmentTarget: DevelopmentTarget
      NoBuild: bool }

with 
    static member DefaultValue =
        { LoggerLevel = Logger.Level.Minimal
          WorkingDir = Path.getFullName "./"
          DevelopmentTarget = DevelopmentTarget.autoReloadProgram
          NoBuild = false }

[<RequireQualifiedAccess>]
module Config =

    let internal tryBuildProject entryProjectFile (config: Config) =
        let workingDir = config.WorkingDir
        if not config.NoBuild then dotnet "build" [entryProjectFile] workingDir


module FcsWatcher =



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
        (config: Config)
        (entryProjectFile: string) = 

            let checker = FSharpChecker.Create()
            let entryProjectFile = Path.getFullName entryProjectFile

            let config = { config with WorkingDir = Path.getFullName config.WorkingDir }        

            logger <- Logger.create (config.LoggerLevel)

            Config.tryBuildProject entryProjectFile config

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
            
                let compilerTmpEmitterAgent = CompilerTmpEmitter.create config.DevelopmentTarget initialCache config.WorkingDir

                let compilerAgent = compiler entryProjectFile compilerTmpEmitterAgent initialCache config checker

                let rec loop (state: FcsWatcherModel) = async {
                    let cache = state.CrackerFsprojBundleCache

                    let sourceFileMap = cache.SourceFileMap

                    let projectMap = cache.ProjectMap

                    let! msg = inbox.Receive()

                    match msg with 
                    | FcsWatcherMsg.DetectSourceFileChanges (fileChanges, replyChannel) ->

                        let projFiles = 
                            fileChanges 
                            |> List.distinctBy (fun fileChange -> fileChange.FullPath) 
                            |> List.map (fun fileChange ->
                                logger.ImportantGreen "detect file %s changed" fileChange.FullPath
                                sourceFileMap.[fileChange.FullPath]
                            )
                            |> List.distinct

                        let crackedFsprojs = projFiles |> List.map (fun projPath -> projectMap.[projPath] ) 

                        replyChannel.Reply (CompilerNumber crackedFsprojs.Length)

                        compilerAgent.Post(CompilerMsg.CompilerProjects ("detect source file changes",crackedFsprojs))

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

                        compilerTmpEmitterAgent.Post(CommonTmpEmitterMsg.UpdateCache newCache)

                        replyChannel.Reply()

                        return! loop { state with SourceFileWatcher = newSourceFileWatcher; CrackerFsprojBundleCache = newCache }
        
                }
                let sourceFileWatcher = newSourceFileWatcher initialCache

                logger.Important "Waiting for changes... press any key to exit"
                
                loop { SourceFileWatcher = sourceFileWatcher; CrackerFsprojBundleCache = initialCache }

            )

            agent
        


    let runFcsWatcher 
        (config: Config)
        (entryProjectFile: string) = 
            fcsWatcher config entryProjectFile |> ignore
            Console.ReadLine() |> ignore
            
        
