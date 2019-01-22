module FcsWatch.CompilerTmpEmiiter
open FcsWatch.Types
open Atrous.Core
open System.Threading.Tasks
open System
open Giraffe.Core
open Giraffe.HttpStatusCodeHandlers
open System.Threading

type CompilerTask = CompilerTask of startTime: DateTime * task: Task<CompilerResult>
with 
    member x.StartTime = 
        match x with 
        | CompilerTask(startTime = m) -> m
    member x.Task = 
        match x with 
        | CompilerTask(task = m) -> m

[<RequireQualifiedAccess>]
type CompilerTmpEmitterMsg =
    | IncrCompilingNum 
    | DecrCompilingNum 
    | AddTmp of string
    | Emit of replyChannel: AsyncReplyChannel<HttpHandler>
    | AddTask of CompilerTask
    | UpdateCache of CrackerFsprojFileBundleCache
    | GetCompilerTmp of AsyncReplyChannel<Set<string>>

type CompilerTmpEmitterState =
    {
        CompilingNumber: int
        CompilerTmp: Set<string>
        EmitReplyChannels: AsyncReplyChannel<HttpHandler> list
        GetTmpReplyChannels: AsyncReplyChannel<Set<string>> list
        CompilerTasks: CompilerTask list
        CrackerFsprojFileBundleCache: CrackerFsprojFileBundleCache // global cache
    }
    
[<RequireQualifiedAccess>]
module CompilerTmpEmiiterState =
    let createEmpty cache = 
            {
                CompilingNumber = 0
                CompilerTmp = Set.empty
                EmitReplyChannels = []
                CompilerTasks = []
                CrackerFsprojFileBundleCache = cache
                GetTmpReplyChannels = []
            }

    let tryGetCompilerTmp compilerTmpEmiiterState =
        match compilerTmpEmiiterState.CompilingNumber with 
        | 0 -> 
            match compilerTmpEmiiterState.GetTmpReplyChannels.Length with 
            | i when i > 0 -> 
                compilerTmpEmiiterState.GetTmpReplyChannels |> List.iter (fun replyChannel ->
                    replyChannel.Reply compilerTmpEmiiterState.CompilerTmp
                )
                { compilerTmpEmiiterState with GetTmpReplyChannels = [] }
            | _ -> compilerTmpEmiiterState
        | _ -> compilerTmpEmiiterState

    let tryEmitAction (config: Config) logger cache compilerTmpEmiiterState =
        let emitCommon (timeDelay: int) calculate load unLoad replyFailure replySuccess =
            match compilerTmpEmiiterState.CompilerTasks with
            | [] ->
                replySuccess()
                calculate()
                Thread.Sleep(timeDelay)
                compilerTmpEmiiterState
            | _ ->            
                let lastTask = compilerTmpEmiiterState.CompilerTasks |> List.maxBy(fun task -> task.StartTime)
                let result = lastTask.Task.Result
                if result.ExitCode <> 0 then 
                    let errorText =  
                        result.Errors 
                        |> Seq.map (fun error -> error.ToString())
                        |> String.concat "\n"
                    replyFailure errorText
                    { compilerTmpEmiiterState with EmitReplyChannels = [] } 
                else            
                    let projectMaps = cache.ProjectMaps
                    let fileTreeMaps = cache.FileTreesMaps
                    unLoad()
                    compilerTmpEmiiterState.CompilerTmp |> Seq.iter (fun projectFile ->
                        let originFileTree = CrackerFsprojFileTree.ofCrackedFsproj projectMaps.[projectFile]
                        let fsprojFileTrees = fileTreeMaps.[projectFile]
                        fsprojFileTrees |> Seq.iter (fun fsprojFileTree ->
                            CrackerFsprojFileTree.copyFile logger originFileTree fsprojFileTree
                        )
                    )
                    replySuccess()
                    load()
                    calculate()
                    
                    { createEmpty cache with GetTmpReplyChannels = compilerTmpEmiiterState.GetTmpReplyChannels }

        match config.DevelopmentTarget with 
        | DevelopmentTarget.Program  ->
            match compilerTmpEmiiterState.CompilingNumber,compilerTmpEmiiterState.EmitReplyChannels with 
            | 0, h::t ->
                t |> List.iter (fun replyChannel -> replyChannel.Reply(RequestErrors.GONE "request is cancelled"))
                
                let replySuccess() = 
                    h.Reply (Successful.OK "fcswatch: Ready to debug")

                let replyFailure errorText = h.Reply (RequestErrors.BAD_REQUEST errorText)    
                emitCommon 0 ignore ignore ignore replyFailure replySuccess
            | _ -> compilerTmpEmiiterState
        | DevelopmentTarget.Plugin (plugin) ->
            match compilerTmpEmiiterState.CompilingNumber,compilerTmpEmiiterState.EmitReplyChannels with 
            | 0, h::t ->
                t |> List.iter (fun replyChannel -> replyChannel.Reply(RequestErrors.GONE "request is cancelled"))
                
                let replySuccess() = 
                    h.Reply (Successful.OK "fcswatch: Ready to debug")

                let replyFailure errorText = h.Reply (RequestErrors.BAD_REQUEST errorText)    
                emitCommon plugin.DebuggerAttachTimeDelay plugin.Calculate plugin.Load plugin.Unload replyFailure replySuccess
            | _ -> compilerTmpEmiiterState

        | DevelopmentTarget.AtOnce (load,unLoad) ->
            compilerTmpEmiiterState.EmitReplyChannels |> 
                List.iter (fun replyChannel -> replyChannel.Reply(RequestErrors.BAD_REQUEST "invalid request"))

            match compilerTmpEmiiterState.CompilingNumber with 
            | 0 -> emitCommon 0 ignore load unLoad ignore ignore
            | _ -> compilerTmpEmiiterState 


    let tryEmit (config: Config) logger cache compilerTmpEmiiterState =
        compilerTmpEmiiterState
        |> tryEmitAction config logger cache
        |> tryGetCompilerTmp

let compilerTmpEmitter config (cache: CrackerFsprojFileBundleCache) = MailboxProcessor<CompilerTmpEmitterMsg>.Start(fun inbox ->
    let logger = config.Logger
    let rec loop state = async {
        let cache = state.CrackerFsprojFileBundleCache        
        let traceMsg compilingNumber msg = 
            Logger.info
                (sprintf "compilerTmpEmitter agent receive message %s,current compiling number is %d" msg compilingNumber)
                logger

        let! msg = inbox.Receive()
        match msg with 
        | CompilerTmpEmitterMsg.DecrCompilingNum -> 
            let compilingNumber = state.CompilingNumber - 1
            traceMsg compilingNumber "Decr"
            assert (state.CompilingNumber > 0)
            let newState = 
                {state with CompilingNumber = compilingNumber}
                |> CompilerTmpEmiiterState.tryEmit config logger cache

            return! loop newState      
        | CompilerTmpEmitterMsg.IncrCompilingNum -> 
            let compilingNumber = state.CompilingNumber + 1
            traceMsg compilingNumber "Incr"
            
            return! loop {state with CompilingNumber = compilingNumber } 
        | CompilerTmpEmitterMsg.AddTmp projectFile -> return! loop {state with CompilerTmp = state.CompilerTmp.Add projectFile}        
        | CompilerTmpEmitterMsg.Emit replyChannel ->
            traceMsg state.CompilingNumber "Emit"
            let newState =
                {state with EmitReplyChannels = replyChannel :: state.EmitReplyChannels}
                |> CompilerTmpEmiiterState.tryEmit config logger cache
            return! loop newState
        | CompilerTmpEmitterMsg.AddTask task -> 
            traceMsg state.CompilingNumber "AddTask"
            return! loop {state with CompilerTasks = task:: state.CompilerTasks}   
        | CompilerTmpEmitterMsg.UpdateCache cache ->
            traceMsg state.CompilingNumber "Update cache"
            return! loop {state with CrackerFsprojFileBundleCache = cache} 
        | CompilerTmpEmitterMsg.GetCompilerTmp replyChannel ->
            traceMsg state.CompilingNumber "GetCompilerTmp"
            let newState = 
                {state with GetTmpReplyChannels = replyChannel :: state.GetTmpReplyChannels }
                |> CompilerTmpEmiiterState.tryEmit config logger cache
            return! loop newState

    }
    loop (CompilerTmpEmiiterState.createEmpty cache) 
)