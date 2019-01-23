module FcsWatch.Compiler
open Fake.IO
open Types
open System.Diagnostics
open Atrous.Core
open FcsWatch.CompilerTmpEmiiter
open System
open System.Threading.Tasks
open FSharp.Control.Tasks.V2.ContextInsensitive

[<RequireQualifiedAccess>]
type CompilerMsg =
    | CompilerProject of project: CrackedFsprojInfo
    | WarmCompile of project: CrackedFsprojInfo
    | AllCompilerTaskNumber of replyChannel: AsyncReplyChannel<int>
    | UpdateCache of CrackerFsprojFileBundleCache
    | CompilerProjects of project: CrackedFsprojInfo list

type CompilerModel = 
    { AllTaskNumber: int 
      CrackerFsprojFileBundleCache: CrackerFsprojFileBundleCache }

let private summary projectPath dll elapsed =
    sprintf "Summary:
-- origin: %s
-- dest： %s 
-- elapsed: %d milliseconds" 
        projectPath dll elapsed



let compiler (compilerTmpEmitterAgent: MailboxProcessor<CompilerTmpEmitterMsg>) (initialCache: CrackerFsprojFileBundleCache) config checker =  MailboxProcessor<CompilerMsg>.Start(fun inbox ->
    
    let logger = config.Logger

    let createCompileTask projects =
        projects 
        |> List.map (fun project ->
            async {
                let stopWatch = Stopwatch.StartNew()            
                let! compilerResult = CrackedFsprojInfo.compile checker project
                CompilerResult.processCompileResult logger compilerResult
                
                if compilerResult.ExitCode = 0 then 
                    Logger.important (summary project.Path compilerResult.Dll stopWatch.ElapsedMilliseconds) logger
                    compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.AddTmp project.Path)
                return compilerResult
            }        
        )
        |> Async.Parallel


    let rec loop model = async {
        let projectMap = model.CrackerFsprojFileBundleCache.ProjectMap        
        let! msg = inbox.Receive()
        match msg with 
        | CompilerMsg.CompilerProject project -> 
            compilerTmpEmitterAgent.Post CompilerTmpEmitterMsg.IncrCompilingNum
            let startTime = DateTime.Now
            let task = 
                async {
                    let! results = createCompileTask [project]
                    compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.DecrCompilingNum)
                    return results |> List.ofSeq
                } 
                |> Async.StartAsTask
            
            compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.AddTask (CompilerTask (startTime,task)))
            return! loop { model with AllTaskNumber = model.AllTaskNumber + 1 }

        | CompilerMsg.CompilerProjects (projects) ->
            compilerTmpEmitterAgent.Post CompilerTmpEmitterMsg.IncrCompilingNum

            let rec compileForTopLevel accResults (descendedProjectLevelMap: Map<string,int>) = async {
                match descendedProjectLevelMap.IsEmpty with
                | false ->
                    let topLevel =
                        descendedProjectLevelMap
                        |> Seq.map (fun pair -> descendedProjectLevelMap.[pair.Key])
                        |> Seq.max        

                    let inProcess =                                 
                        descendedProjectLevelMap 
                        |> Map.filter (fun projFile level -> level = topLevel)        

                    let projects = inProcess |> Seq.map (fun proj -> projectMap.[proj.Key]) |> List.ofSeq
                    let projectFiles = projects |> List.map (fun project -> project.Path)

                    let! results = createCompileTask projects
                    let results = 
                        results 
                        |> Array.tryFind(fun result -> 
                            result.ExitCode <> 0
                        )
                        |> function
                            | Some errorResult ->
                                results |> Array.iter (CompilerResult.processCompileResult logger)
                                [errorResult]
                            | None ->
                                let left = descendedProjectLevelMap |> Map.filter (fun key value ->
                                    not (List.contains key projectFiles)
                                )         
                                compileForTopLevel (accResults @ (List.ofArray results)) left |> Async.RunSynchronously            
                    return results
                | true -> return accResults     
            }
            let projectFiles = projects |> List.map (fun project -> project.Path)

            let map = model.CrackerFsprojFileBundleCache.DescendedProjectLevelMap |> Map.filter (fun key value ->
                List.contains key projectFiles
            )
            let startTime = DateTime.Now
            let task = 
                async {
                    let! result = compileForTopLevel [] map 
                    compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.DecrCompilingNum)
                    return result
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
                    return [compilerResult]
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
    loop { CrackerFsprojFileBundleCache = initialCache; AllTaskNumber = 0}
)