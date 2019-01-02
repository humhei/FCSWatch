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


type DebuggingServer(config: Config,checker,bundle: CrackedFsprojBundle) =


    do (CrackedFsprojInfo.warmupCompile config.Logger checker bundle.Entry.Info |> Async.Start)
    let logger = config.Logger
    let lockerFactory = new LockerFactory<string>()

    
    let compilerTmp = new ConcurrentDictionary<string, option<CrackerFsprojFileTree * CrackerFsprojFileTree list>>()


    let emitCompilerTmpHandle : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            compilerTmp.Values |> Seq.iter (fun valueOp ->
                match valueOp with 
                | Some (originFileTree,fsprojFileTrees) ->
                    fsprojFileTrees |> Seq.iter (fun fsprojFileTree ->
                        CrackerFsprojFileTree.copyFileInLocker lockerFactory logger originFileTree fsprojFileTree
                    )
                | None -> ()
            )
            compilerTmp.Clear()
            text "Ready to debug" next ctx
    
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
            Logger.info (sprintf "Compiling %s" projectFile) logger
            let (fsproj,stack) = bundle.ScanProject projectFile
            let! compilerResult = CrackedFsprojInfo.compile checker fsproj.Info
            CompilerResult.processCompileResult logger compilerResult
            if compilerResult.ExitCode = 0 then 
                let fsprojFileTree = CrackerFsprojFileTree.ofCrackedFsproj fsproj.Info
                return Some (CrackerFsprojFileTree.ofCrackedFsproj fsproj.Info,fsprojFileTree :: stack) 
            else return None
        }
        compilerTmp.AddOrUpdate (projectFile,newValue,fun _ value ->
            match newValue with 
            | None -> value
            | Some _ -> newValue
        ) |> ignore
    }     


type FcsWatcher(buildingConfig: Config -> Config, checker: FSharpChecker, projectFile: string) =
    let config = buildingConfig {Logger = Logger.Normal;DebuggingServerPort = 8050}
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
            { BaseDirectory = Path.getFullName "./"
              Includes = files
              Excludes = [] }
        pattern |> ChangeWatcher.run (fun changes ->
            match List.ofSeq changes with 
            | [change] -> 
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