
namespace FcsWatch.Binary
open Fake.Core
open FcsWatch.Core
open Types
open FcsWatch.Core.CrackedFsproj
open System.Diagnostics
open System.Collections.Concurrent
open Fake.DotNet
open FcsWatch.Binary.Helpers
open Fake.IO
open FcsWatch.Core.CompilerTmpEmitter
open Extensions
open System.Threading
open System.Net
open System.Text

[<RequireQualifiedAccess>]
type WhyRun =
    | Rerun
    | Run

type PluginDebugInfo =
    { DebuggerAttachTimeDelay: int
      Pid: int
      VscodeLaunchConfigurationName: string }

[<RequireQualifiedAccess>]
module PluginDebugInfo =
    open VscodeHelper


    let writePidForPlugin workingDir (pluginDebugInfo: PluginDebugInfo) =
        match File.tryFindLaunchJsonUp workingDir with
        | Some file ->

            let launch = Launch.read file
            Launch.writePid pluginDebugInfo.VscodeLaunchConfigurationName pluginDebugInfo.Pid launch
            |> Launch.write file
        | None -> logger.Important "Doesn't exists .vscode/launch.json, If you want write attachable pid automatically to launch.json, Please write it first"



[<RequireQualifiedAccess>]
module AutoReload =

    type TmpEmitterMsg = unit
    type TmpEmitterState = CompilerTmpEmitterState<int, CompilerResult>



    type Plugin =
        { Load: unit -> unit
          Unload: unit -> unit
          Calculate: unit -> unit
          TimeDelayAfterUninstallPlugin: int
          /// sometimes i want work both in autoReload + debuggable
          PluginDebugInfo: PluginDebugInfo option }


    [<RequireQualifiedAccess>]
    type DevelopmentTarget =
        | Program
        | Plugin of Plugin

    type ProgramRunningArgs =
        { Webhook: string option
          AdditionalArgs: string list 
          DevelopmentTarget: DevelopmentTarget
          WorkingDir: string
          WhyRun: WhyRun }

    let private runningProjects = new ConcurrentDictionary<string,Process>()

    [<RequireQualifiedAccess>]
    module internal CrackedFsproj =

        let private dotnetTool =
            [DotNet.Options.Create().DotNetCliPath]
            |> Args.toWindowsCommandLine

        [<RequireQualifiedAccess>]
        module Process =
            let start tool args workingDir =
                let startInfo = new ProcessStartInfo();
                let args = Args.toWindowsCommandLine args
                startInfo.WorkingDirectory <- workingDir
                startInfo.Arguments <- args
                startInfo.FileName <- tool
                logger.ImportantGreen "%s %s" tool args
                Process.Start(startInfo)

        let tryRun (args: ProgramRunningArgs) (crackedFsproj: CrackedFsproj) =

            match (args.DevelopmentTarget: DevelopmentTarget) with
            | DevelopmentTarget.Program ->
                match crackedFsproj.ProjectTarget with
                | ProjectTarget.Exe ->
                    let dotnetArgs =
                        [
                            "run";"--project"; crackedFsproj.ProjPath; "--no-build"; "--no-restore"; "--framework"; crackedFsproj.PreferFramework; "--"
                        ]

                    runningProjects.GetOrAdd (crackedFsproj.ProjPath,fun _ ->
                        let proc = 
                            Process.start
                                dotnetTool
                                (dotnetArgs @ args.AdditionalArgs)
                                args.WorkingDir

                        match args.Webhook with 
                        | Some webhook ->
                            let json = Newtonsoft.Json.JsonConvert.SerializeObject args.WhyRun
                            use webClient = new WebClient(Encoding = Encoding.UTF8)
                            logger.Important "SENDING TO WEBHOOK... " // : <<<%s>>>... --> %s" json.[0 .. min (json.Length - 1) 100] hook
                            let resp = webClient.UploadString (webhook,"Put",json)
                            logger.Important "RESP FROM WEBHOOK: %s" resp

                        | None -> ()

                        proc 


                    )
                    |> ignore

                | ProjectTarget.Library ->
                    failwith "project is a library, autoReload is ture, development target is program;"
            | DevelopmentTarget.Plugin plugin ->
                plugin.Load()

        let tryReRun args (crackedFsproj: CrackedFsproj) =
            let args = { args with WhyRun = WhyRun.Rerun }
            tryRun args (crackedFsproj: CrackedFsproj)


        let tryKill developmentTarget (crackedFsproj: CrackedFsproj) =
            match developmentTarget with
            | DevelopmentTarget.Plugin plugin ->
                plugin.Unload()
                Thread.Sleep plugin.TimeDelayAfterUninstallPlugin
            | DevelopmentTarget.Program ->
                match runningProjects.TryRemove (crackedFsproj.ProjPath) with
                | true, proc ->
                    if not proc.HasExited
                    then
                        proc.KillTree()
                        logger.InfoGreen "Killed process %d" proc.Id
                | false, _ ->
                    failwithf "Cannot remove %s from running projects" crackedFsproj.ProjPath


    [<RequireQualifiedAccess>]
    module internal TmpEmitterState =

        let tryEmit args (state: TmpEmitterState) =
            let commonState = state.CommonState
            let cache = commonState.CrackerFsprojFileBundleCache

            match commonState.CompilingNumber with
            | 0 ->

                logger.Info "Current cached compier task is %d" commonState.CachedCompilerTasks.Length

                match commonState.CachedCompilerTasks with
                | [task] when task.Why = WhyCompile.WarmCompile -> state
                | _ ->
                    let lastTasks =
                        commonState.CachedCompilerTasks
                        |> List.groupBy (fun compilerTask ->
                            compilerTask.Task.Result.[0].ProjPath
                        )
                        |> List.map (fun (projPath, compilerTasks) ->
                            compilerTasks |> List.maxBy (fun compilerTask -> compilerTask.StartTime)
                        )


                    let allResults: CompilerResult list = lastTasks |> List.collect (fun task -> task.Task.Result)

                    match List.tryFind (fun (result: CompilerResult) -> result.ExitCode <> 0) allResults with
                    | Some result ->  state

                    | None ->

                        let projRefersMap = cache.ProjRefersMap

                        let projLevelMap = cache.ProjLevelMap

                        CrackedFsproj.tryKill args.DevelopmentTarget cache.EntryCrackedFsproj

                        commonState.CompilerTmp
                        |> Seq.sortByDescending (fun projPath ->
                            projLevelMap.[projPath]
                        )
                        |> Seq.iter (fun projPath ->
                            let currentCrackedFsproj = cache.ProjectMap.[projPath]

                            CrackedFsproj.copyObjToBin currentCrackedFsproj

                            let refCrackedFsprojs = projRefersMap.[projPath]

                            refCrackedFsprojs |> Seq.sortByDescending (fun refCrackedFsproj ->
                                projLevelMap.[refCrackedFsproj.ProjPath]
                            )
                            |> Seq.iter (CrackedFsproj.copyFileFromRefDllToBin projPath)
                        )

                        CrackedFsproj.tryReRun args cache.EntryCrackedFsproj

                        CompilerTmpEmitterState.createEmpty 0 cache
            | _ ->
                state


    let create args =
        { new ICompilerTmpEmitter<unit, int, CompilerResult> with
            member x.TryEmit(_, state) = TmpEmitterState.tryEmit args state
            member x.ProcessCustomMsg (_, state) = state
            member x.CustomInitialState = 0 }
