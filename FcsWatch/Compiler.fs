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
    | WarmCompile of project: CrackedFsprojInfo
    | AllCompilerTaskNumber of replyChannel: AsyncReplyChannel<int>
    | UpdateCache of CrackerFsprojFileBundleCache

type CompilerModel = 
    { AllTaskNumber: int 
      CrackerFsprojFileBundleCache: CrackerFsprojFileBundleCache }

let private summary projectPath dll elapsed =
    sprintf "Summary:
-- origin: %s
-- dest： %s 
-- elapsed: %d milliseconds" 
        projectPath dll elapsed

let compiler (compilerTmpEmitterAgent: MailboxProcessor<CompilerTmpEmitterMsg>) (cache: CrackerFsprojFileBundleCache) config checker =  MailboxProcessor<CompilerMsg>.Start(fun inbox ->
    let logger = config.Logger
    let rec loop model = async {
        let! msg = inbox.Receive()
        match msg with 
        | CompilerMsg.CompilerProject project -> 
            compilerTmpEmitterAgent.Post CompilerTmpEmitterMsg.IncrCompilingNum

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
            
            return! loop { model with AllTaskNumber = model.AllTaskNumber + 1 }

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
            return! loop { model with AllTaskNumber = model.AllTaskNumber + 1 }
        | CompilerMsg.AllCompilerTaskNumber replyChannel ->
            replyChannel.Reply (model.AllTaskNumber)
            return! loop model
        | CompilerMsg.UpdateCache cache ->
            return! loop { model with CrackerFsprojFileBundleCache = cache }                 
    }
    loop { CrackerFsprojFileBundleCache = cache; AllTaskNumber = 0}
)