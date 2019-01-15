module FcsWatch.Compiler
open Fake.IO
open Types
open System.Diagnostics
open Atrous.Core
open FcsWatch.CompilerTmpEmiiter
open System

[<RequireQualifiedAccess>]
type CompilerMsg =
    | CompilerProject of project: CrackedFsprojInfo
    | DetectFileChange of fileChange: FileChange
    | WarmCompile of project: CrackedFsprojInfo
    
let private summary projectPath dll elapsed =
    sprintf "Summary:
-- origin: %s
-- dest： %s 
-- elapsed: %d milliseconds" 
        projectPath dll elapsed

let compiler (compilerTmpEmitterAgent: MailboxProcessor<CompilerTmpEmitterMsg>) (fsprojBundleAgent: MailboxProcessor<CrackedFsprojBundleMsg>) config checker =  MailboxProcessor<CompilerMsg>.Start(fun inbox ->
    let logger = config.Logger
    let bundleCache = fsprojBundleAgent.PostAndReply CrackedFsprojBundleMsg.Cache                                            
    let projectMaps = bundleCache.ProjectMaps   
    let sourceFileMaps = bundleCache.SourceFileMaps
    let rec loop state = async {
        let! msg = inbox.Receive()
        match msg with 
        | CompilerMsg.DetectFileChange (fileChange) ->
            compilerTmpEmitterAgent.Post CompilerTmpEmitterMsg.IncrCompilingNum
            let projFile = sourceFileMaps.[fileChange.FullPath]
            inbox.Post (CompilerMsg.CompilerProject projectMaps.[projFile])
            return! loop state

        | CompilerMsg.CompilerProject project -> 
            let startTime = DateTime.Now
            let task = 
                async {
                    let stopWatch = Stopwatch.StartNew()            
                    let! compilerResult = CrackedFsprojInfo.compile checker project
                    CompilerResult.processCompileResult logger compilerResult
                    
                    if compilerResult.ExitCode = 0 then 
                        Logger.important (summary project.Path compilerResult.Dll stopWatch.ElapsedMilliseconds) logger
                        compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.AddTmp project.Path)
                    compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.DecrCompilingNum)
                    return compilerResult
                } |> Async.StartAsTask
            compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.AddTask (CompilerTask (startTime,task)))
            return! loop state
        | CompilerMsg.WarmCompile project ->
            let startTime = DateTime.Now
            let task = 
                async {
                    compilerTmpEmitterAgent.Post CompilerTmpEmitterMsg.IncrCompilingNum
                    let! compilerResult = CrackedFsprojInfo.compile checker project
                    compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.DecrCompilingNum)
                    return compilerResult
                }
                |> Async.StartAsTask
            compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.AddTask (CompilerTask (startTime,task)))
            return! loop state
    }
    loop ()
)