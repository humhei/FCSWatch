module FcsWatch.Core.Compiler
open Types
open System.Diagnostics
open FcsWatch.Core.CompilerTmpEmitter
open System
open FcsWatch.Core.CrackedFsproj
open FSharp.Compiler.SourceCodeServices



[<RequireQualifiedAccess>]
type CompilerMsg =
    | UpdateCache of CrackedFsprojBundleCache
    | CompileProjects of why: WhyCompile * projects: CrackedFsproj list * incrCompilingNumberChannel: AsyncReplyChannel<unit>

type CompilerModel =
    { CrackerFsprojBundleCache: CrackedFsprojBundleCache }


type ICompiler<'Result when 'Result :> ICompilerOrCheckResult> =
    abstract member Compile : checker: FSharpChecker * proejct: CrackedFsproj -> Async<'Result []>
    abstract member WarmCompile: bool 
    abstract member Summary: result: 'Result * elapsed: int64 -> string

let compilerAgent (compiler: ICompiler<'Result>) (compilerTmpEmitterAgent: MailboxProcessor<CompilerTmpEmitterMsg<_, _>>) (initialCache: CrackedFsprojBundleCache) checker =  MailboxProcessor<CompilerMsg>.Start(fun inbox ->
    inbox.Error.Add(fun error -> logger.Error "%A" error)

    let createCompileTask (crackedFsprojs: CrackedFsproj list) =
        crackedFsprojs
        |> List.map (fun crackedFsproj ->
            async {
                let stopWatch = Stopwatch.StartNew()

                let! compilerResults = compiler.Compile (checker, crackedFsproj)

                match Array.tryFind (fun (compilerResult: 'Result) -> compilerResult.ExitCode <> 0) compilerResults with
                | None ->
                    let compilerResult = compilerResults.[0]

                    ICompilerOrCheckResult.processCompileOrCheckResult compilerResult

                    logger.Important "%s" (compiler.Summary (compilerResult, stopWatch.ElapsedMilliseconds))

                    compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.addTmp crackedFsproj.ProjPath)

                    return compilerResult

                | Some erroCompilerResult ->
                    ICompilerOrCheckResult.processCompileOrCheckResult erroCompilerResult

                    return erroCompilerResult
            }
        )
        |> Async.Parallel


    let rec loop model = async {
        let projectMap = model.CrackerFsprojBundleCache.ProjectMap

        let! msg = inbox.Receive()
        match msg with
        | CompilerMsg.CompileProjects (why, crackedFsprojs, replyChannel) ->
            compilerTmpEmitterAgent.PostAndReply (fun replyChannel -> CompilerTmpEmitterMsg.incrCompilingNum (crackedFsprojs.Length,replyChannel))
            replyChannel.Reply()

            let projPaths = crackedFsprojs |> List.map (fun crackedFsproj -> crackedFsproj.ProjPath)

            /// from top to bottom
            let rec compileByLevel accResults (projLevelMap: Map<string,int>) = async {
                match projLevelMap.IsEmpty with
                | false ->
                    let inProcess =
                        let maxLevel =
                            projLevelMap
                            |> Seq.map (fun pair -> projLevelMap.[pair.Key])
                            |> Seq.max

                        projLevelMap
                        |> Map.filter (fun projPath level -> level = maxLevel)
                        |> Seq.map (fun projLevelPair -> projectMap.[projLevelPair.Key]) |> List.ofSeq

                    let inProcessProjPaths =
                        inProcess |> Seq.map (fun crackedProject ->
                            crackedProject.ProjPath
                        ) |> List.ofSeq

                    let! results = createCompileTask inProcess

                    let results =
                        results
                        |> Array.tryFind(fun result ->
                            result.ExitCode <> 0
                        )
                        |> function
                            | Some errorResult ->
                                [errorResult]

                            | None ->
                                let left = projLevelMap |> Map.filter (fun projPath level ->
                                    not (List.contains projPath inProcessProjPaths)
                                )

                                compileByLevel (accResults @ (List.ofArray results)) left |> Async.RunSynchronously

                    return results
                | true -> return accResults
            }


            let startTime = DateTime.Now

            let task =
                let correnspondingProjLevelMap = model.CrackerFsprojBundleCache.ProjLevelMap |> Map.filter (fun projPath level ->
                    List.contains projPath projPaths
                )

                async {
                    let! result = compileByLevel [] correnspondingProjLevelMap
                    compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.decrCompilingNum crackedFsprojs.Length)
                    return result
                } |> Async.StartAsTask

            compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.addTask (CompilerTask (why, startTime, task)))
            return! loop model

        | CompilerMsg.UpdateCache cache ->
            return! loop { model with CrackerFsprojBundleCache = cache }

    }

    let entryCrackedFsproj = initialCache.EntryCrackedFsproj

    if compiler.WarmCompile then 
        inbox.PostAndAsyncReply (fun replyChannel ->
            CompilerMsg.CompileProjects (WhyCompile.WarmCompile, [entryCrackedFsproj], replyChannel)
        ) |> Async.Start

    loop { CrackerFsprojBundleCache = initialCache }
)