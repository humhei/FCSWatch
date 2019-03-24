module FcsWatch.Compiler
open Types
open System.Diagnostics
open FcsWatch.CompilerTmpEmitter
open System
open FcsWatch.CrackedFsproj
open FcsWatch.AutoReload

[<RequireQualifiedAccess>]
type CompilerMsg =
    | UpdateCache of CrackedFsprojBundleCache
    | CompilerProjects of why: string * projects: CrackedFsproj list

type CompilerModel =
    { CrackerFsprojBundleCache: CrackedFsprojBundleCache }

let private summary projectPath dll elapsed =
    sprintf "Summary:
-- origin: %s
-- dest： %s
-- elapsed: %d milliseconds"
        projectPath dll elapsed



let compiler (entryProjectFile) (compilerTmpEmitterAgent: CompilerTmpEmitter) (initialCache: CrackedFsprojBundleCache) config checker =  MailboxProcessor<CompilerMsg>.Start(fun inbox ->

    let createCompileTask (crackedFsprojInfoTargets: CrackedFsproj list) =
        crackedFsprojInfoTargets
        |> List.map (fun crackedFsprojInfoTarget ->
            async {
                let stopWatch = Stopwatch.StartNew()

                let! compilerResults = CrackedFsproj.compile checker crackedFsprojInfoTarget

                match Array.tryFind (fun compilerResult -> compilerResult.ExitCode <> 0) compilerResults with
                | None ->
                    let compilerResult = compilerResults.[0]

                    CompilerResult.processCompileResult compilerResult

                    logger.Important "%s" (summary crackedFsprojInfoTarget.ProjPath compilerResult.Dll stopWatch.ElapsedMilliseconds)

                    compilerTmpEmitterAgent.Post (AutoReloadTmpEmitterMsg.AddTmp crackedFsprojInfoTarget.ProjPath)

                    return compilerResult

                | Some erroCompilerResult ->
                    CompilerResult.processCompileResult erroCompilerResult

                    return erroCompilerResult
            }
        )
        |> Async.Parallel


    let rec loop model = async {
        let projectMap = model.CrackerFsprojBundleCache.ProjectMap

        let! msg = inbox.Receive()

        match msg with
        | CompilerMsg.CompilerProjects (why,crackedFsprojs) ->
            compilerTmpEmitterAgent.Post (AutoReloadTmpEmitterMsg.IncrCompilingNum crackedFsprojs.Length)

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
                                results |> Array.iter (CompilerResult.processCompileResult)

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
                    compilerTmpEmitterAgent.Post (AutoReloadTmpEmitterMsg.DecrCompilingNum crackedFsprojs.Length)
                    return result
                } |> Async.StartAsTask

            compilerTmpEmitterAgent.Post (AutoReloadTmpEmitterMsg.AddTask (CompilerTask (why, startTime, task)))
            return! loop model

        | CompilerMsg.UpdateCache cache ->
            return! loop { model with CrackerFsprojBundleCache = cache }

    }
    let entryCrackedFsproj = initialCache.ProjectMap.[entryProjectFile]

    inbox.Post (CompilerMsg.CompilerProjects ("warm compile",[entryCrackedFsproj]))

    loop { CrackerFsprojBundleCache = initialCache }
)