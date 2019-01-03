namespace FcsWatch
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.Types
open Fake.IO.Globbing
open Fake.IO
open System
open Atrous.Core
open Atrous.Core.Utils
open System.Collections.Concurrent
open Giraffe
open Saturn
open System.Threading
open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.Http
open System.Diagnostics



type DebuggingServer(config: Config,checker,bundle: CrackedFsprojBundle) =

    // do (CrackedFsprojInfo.warmupCompile config.Logger checker bundle.Entry.Info |> Async.Start)
    let logger = config.Logger
    let lockerFactory = new LockerFactory<string>()
    let mutable compilingNumber = 0
    let emitSet = new ManualResetEventSlim(true)
    let refreshEmitSet() = 
        if compilingNumber = 0 then emitSet.Set()
        else emitSet.Reset() 

    let compilingSet = new ManualResetEventSlim(true)

    let compilerTmp = new ConcurrentDictionary<string, option<CrackerFsprojFileTree * CrackerFsprojFileTree list>>()
    

    let emitCompilerTmpHandle : HttpHandler =

        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                do! Async.Sleep(config.FileSavingTimeBeforeDebugging)
                refreshEmitSet()     
                emitSet.Wait()
                compilingSet.Reset()

                compilerTmp.Values |> Seq.iter (fun valueOp ->
                    match valueOp with 
                    | Some (originFileTree,fsprojFileTrees) ->
                        fsprojFileTrees |> Seq.iter (fun fsprojFileTree ->
                            CrackerFsprojFileTree.copyFileInLocker lockerFactory logger originFileTree fsprojFileTree
                        )
                    | None -> ()
                )
                compilerTmp.Clear()
                compilingSet.Set()
                return! text "Ready to debug" next ctx
            }
    
    let webApp =
        choose [
            route "/emitCompilerTmp"   >=>  emitCompilerTmpHandle
        ]
    

    let app = application {
        url (sprintf "http://0.0.0.0:%d" config.DebuggingServerPort) 
        use_router webApp
    }

    member __.Run() = async {run app}


    member __.CompileProject projectFile = async {
        let! newValue = async {
            compilingNumber <- compilingNumber + 1            
            let stopWatch = Stopwatch.StartNew()            
            let (fsproj,stack) = bundle.ScanProject projectFile
            let fsprojInfo = fsproj.Info
            compilingSet.Wait()
            let! compilerResult = CrackedFsprojInfo.compile checker fsprojInfo
            CompilerResult.processCompileResult logger compilerResult
            compilingNumber <- compilingNumber - 1  
            refreshEmitSet()          
            if compilerResult.ExitCode = 0 then 
                Logger.important 
                    (sprintf 
                        "Summary:
-- origin: %s
-- dest： %s 
-- elapsed: %d milliseconds" 
                        projectFile 
                        compilerResult.Dll 
                        stopWatch.ElapsedMilliseconds) logger
                let fsprojFileTree = CrackerFsprojFileTree.ofCrackedFsproj fsprojInfo
                return Some (CrackerFsprojFileTree.ofCrackedFsproj fsprojInfo,fsprojFileTree :: stack) 
            else return None
        }
        
        compilerTmp.AddOrUpdate (projectFile,newValue,fun _ value ->
            match newValue with 
            | None -> value
            | Some _ -> newValue
        ) |> ignore
    }     


type FcsWatcher(buildingConfig: Config -> Config, checker: FSharpChecker, projectFile: string) =
    let config = buildingConfig {
            Logger = Logger.Normal
            DebuggingServerPort = 8050
            WorkingDir = Path.getFullName "./"
            FileSavingTimeBeforeDebugging = 1000
        }
    do (Logger.info (sprintf "fcs watcher is running in logger level %A" config.Logger) config.Logger)
    do (Logger.info (sprintf "fcs watcher's working directory is %s" config.WorkingDir) config.Logger)
    let bundle = CrackedFsprojBundle(projectFile,config.Logger,checker)
    let debuggingServer = DebuggingServer(config,checker,bundle)
    do (debuggingServer.Run() |> Async.Start)
    let fileMaps = bundle.FileMaps()

    let watcher = 
        let pattern = 
            let files = 
                fileMaps 
                |> Seq.collect (fun pair -> pair.Value) 
                |> List.ofSeq
            { BaseDirectory = config.WorkingDir
              Includes = files
              Excludes = [] }
        pattern |> ChangeWatcher.run (fun changes ->
            match List.ofSeq changes with 
            | [change] -> 
                Logger.important (sprintf "file %s is changed" change.FullPath) config.Logger
                async {
                    let projFilePair = 
                        fileMaps 
                        |> Seq.filter (fun fileMap -> fileMap.Value |> Array.contains change.FullPath)
                        |> Seq.exactlyOne
                    let projFile = projFilePair.Key
                    do! debuggingServer.CompileProject projFile
                } |> Async.Start
            | _ ->
                failwith "multiple files changed at some time" 
        )

    interface IDisposable with 
        member x.Dispose() = watcher.Dispose()