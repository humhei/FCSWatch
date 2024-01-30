namespace FcsWatch.Porta
open Fake.Core
open FcsWatch.Core
open FullCrackedFsproj
open FcsWatch.Core.CrackedFsproj
open FcsWatch.Core.CompilerTmpEmitter
open FSharp.Compiler.CodeAnalysis
open FcsWatch.Porta.FromCompilerService
open Extensions

type PortaConfig =
    { Eval: bool
      LiveCheckOnly: bool 
      LoggerLevel: Logger.Level
      Fsproj: string option
      UseEditFiles: bool
      WriteInfo: bool
      NoBuild: bool
      WorkingDir: string
      Webhook: string option
      Watch: bool
      OtherFlags: string [] }





[<RequireQualifiedAccess>]
module CompilerTmpEmitter =
    type TmpEmitterState = CompilerTmpEmitterState<int, CheckResult>

    let mutable private count = 0

    [<RequireQualifiedAccess>]
    module CompilerTmpEmitterState =

        let private processResult config projOptions (result: CheckResult) =
            if result.ExitCode <> 0 then ()
            else
                match config.Webhook with 
                | Some hook -> 
                    sendToWebHook config.Eval hook result.Contents
                | None -> 

                    if config.Eval then 
                        printfn "fscd: CHANGE DETECTED, RE-EVALUATING ALL INPUTS...." 
                        evaluateDecls config.Eval config.WriteInfo config.LiveCheckOnly result.Contents projOptions

                // The default is to dump
                if not config.Eval && config.Webhook.IsNone then 
                    let fileConvContents = jsonFiles config.Eval (Array.ofList result.Contents)
                    count <- count + 1
                    logger.Info "%A\nCount:%d" fileConvContents count 


        let tryEmit (config: PortaConfig) (state: TmpEmitterState) =
            let commonState = state.CommonState

            let cache = commonState.CrackerFsprojFileBundleCache

            match commonState.CompilingNumber with 
            | 0 ->

                logger.Info "Current cached compier task is %d" commonState.CachedCompilerTasks.Length

                match commonState.CachedCompilerTasks with 
                | _ ->
                    let lastTasks = 
                        commonState.CachedCompilerTasks 
                        |> List.groupBy (fun compilerTask ->
                            compilerTask.Task.Result.[0].Fsproj.ProjPath
                        )
                        |> List.map (fun (projPath, compilerTasks) ->
                            compilerTasks |> List.maxBy (fun compilerTask -> compilerTask.StartTime)
                        )


                    let allResults: CheckResult list = lastTasks |> List.collect (fun task -> task.Task.Result)

                    match List.tryFind (fun (result: CheckResult) -> result.ExitCode <> 0) allResults with 
                    | Some result ->
                        let errorText =  
                            result.Errors 
                            |> Seq.map (fun error -> error.ToString())
                            |> String.concat "\n"

                        state

                    | None ->

                        let projRefersMap = cache.ProjRefersMap

                        let projLevelMap = cache.ProjLevelMap

                        commonState.CompilerTmp
                        |> Seq.sortByDescending (fun projPath ->
                            projLevelMap.[projPath]
                        )
                        |> Seq.iter (fun projPath ->
                            let correspondingResult = 
                                allResults |> List.find (fun result -> result.Fsproj.ProjPath = projPath)

                            processResult config correspondingResult.Fsproj.FSharpProjectOptions correspondingResult 
                        )
                        CompilerTmpEmitterState.createEmpty 0 cache 
            | _ -> state


    let create config = 
        { new ICompilerTmpEmitter<unit, int, CheckResult> with 
            member x.TryEmit(workingDir, state) = CompilerTmpEmitterState.tryEmit config state
            member x.ProcessCustomMsg (_, state) = state
            member x.CustomInitialState = 0 }
        


