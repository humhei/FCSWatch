module FsLive.Core.Compiler
open CrackedFsprojBundle
open System.Diagnostics
open FsLive.Core.CompilerTmpEmiiter
open System
open FsLive.Core.CrackedFsproj

[<RequireQualifiedAccess>]
type CompilerMsg =
    | UpdateCache of CrackedFsprojBundleCache
    | CompileProjects of projects: CrackedFsproj list

type CompilerModel =
    { CrackerFsprojBundleCache: CrackedFsprojBundleCache }





let compiler (developmentTarget: DevelopmentTarget) (entryProjectFile) (compilerTmpEmitterAgent: MailboxProcessor<CompilerTmpEmitterMsg>) (initialCache: CrackedFsprojBundleCache) config checker =  MailboxProcessor<CompilerMsg>.Start(fun inbox ->

    let createCompileTask (crackedFsprojInfoTargets: CrackedFsproj list) =
        crackedFsprojInfoTargets
        |> List.map (fun crackedFsprojInfoTarget ->
            async {
                let stopWatch = Stopwatch.StartNew()

                let! results = developmentTarget.CompileOrCheck checker crackedFsprojInfoTarget

                match Array.tryFind CompileOrCheckResult.isFail results with
                | None ->
                    let result = results.[0]

                    CompileOrCheckResult.processResult result

                    CompileOrCheckResult.summary stopWatch.ElapsedMilliseconds result

                    compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.AddTmp crackedFsprojInfoTarget.ProjPath)

                    return result

                | Some erroResult ->
                    CompileOrCheckResult.processResult erroResult

                    return erroResult
            }
        )
        |> Async.Parallel


    let rec loop model = async {
        let projectMap = model.CrackerFsprojBundleCache.ProjectMap

        let! msg = inbox.Receive()

        match msg with
        | CompilerMsg.CompileProjects (crackedFsprojs) ->
            compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.IncrCompilingNum crackedFsprojs.Length)

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
                        |> Array.tryFind CompileOrCheckResult.isFail
                        |> function
                            | Some errorResult ->
                                results |> Array.iter (CompileOrCheckResult.processResult)

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
                    compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.DecrCompilingNum crackedFsprojs.Length)
                    return result
                } |> Async.StartAsTask

            compilerTmpEmitterAgent.Post (CompilerTmpEmitterMsg.AddTask (CompilerTask (startTime,task)))
            return! loop model

        | CompilerMsg.UpdateCache cache ->
            return! loop { model with CrackerFsprojBundleCache = cache }



    }
    let entryCrackedFsproj = initialCache.ProjectMap.[entryProjectFile]

    inbox.Post (CompilerMsg.CompileProjects [entryCrackedFsproj])

    loop { CrackerFsprojBundleCache = initialCache }
)