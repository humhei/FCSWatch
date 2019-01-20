module FcsWatch.CompilerTmpEmiiter
open FcsWatch.Types
open Atrous.Core
open System.Threading.Tasks
open System
open Giraffe.Core
open Giraffe.HttpStatusCodeHandlers

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

type CompilerTmpEmitterState =
    {
        CompilingNumber: int
        CompilerTmp: Set<string>
        EmitReplyChannels: AsyncReplyChannel<HttpHandler> list
        CompilerTasks: CompilerTask list
        CrackerFsprojFileBundleCache: CrackerFsprojFileBundleCache
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
            }
    let tryEmit (config: Config) logger cache compilerTmpEmiiterState =
        match compilerTmpEmiiterState.CompilingNumber,compilerTmpEmiiterState.EmitReplyChannels with 
        | 0, h::t ->

                
            t |> List.iter (fun replyChannel -> replyChannel.Reply(RequestErrors.GONE "request is cancelled"))
            
            let replySuccess() = 
                config.AfterEmitTmp()
                h.Reply (Successful.OK "fcswatch: Ready to debug")

            match compilerTmpEmiiterState.CompilerTasks with
            | [] -> 
                replySuccess()
                compilerTmpEmiiterState
            | _ ->
                let lastTask = compilerTmpEmiiterState.CompilerTasks |> List.maxBy(fun task -> task.StartTime)
                let result = lastTask.Task.Result
                if result.ExitCode <> 0 then 
                    let errorText =  
                        result.Errors 
                        |> Seq.map (fun error -> error.ToString())
                        |> String.concat "\n"
                    h.Reply (RequestErrors.BAD_REQUEST errorText)
                    { compilerTmpEmiiterState with EmitReplyChannels = [] } 
                else            
                    let projectMaps = cache.ProjectMaps
                    let fileTreeMaps = cache.FileTreesMaps
                    compilerTmpEmiiterState.CompilerTmp |> Seq.iter (fun projectFile ->
                        let originFileTree = CrackerFsprojFileTree.ofCrackedFsproj projectMaps.[projectFile]
                        let fsprojFileTrees = fileTreeMaps.[projectFile]
                        fsprojFileTrees |> Seq.iter (fun fsprojFileTree ->
                            CrackerFsprojFileTree.copyFile logger originFileTree fsprojFileTree
                        )
                    )
                    replySuccess()
                    createEmpty cache
        | _ -> compilerTmpEmiiterState    


let compilerTmpEmitter config (cache: CrackerFsprojFileBundleCache) = MailboxProcessor<CompilerTmpEmitterMsg>.Start(fun inbox ->
    let logger = config.Logger
    let rec loop state = async {
        let cache = state.CrackerFsprojFileBundleCache        
        let diagnosticsMessage compilingNumber msg = 
            Logger.info
                (sprintf "compilerTmpEmitter agent receive message %s,current compiling number is %d" msg compilingNumber)
                logger

        let! msg = inbox.Receive()
        match msg with 
        | CompilerTmpEmitterMsg.DecrCompilingNum -> 
            let compilingNumber = state.CompilingNumber - 1
            diagnosticsMessage compilingNumber "Decr"
            assert (state.CompilingNumber > 0)
            let newState = 
                {state with CompilingNumber = compilingNumber}
                |> CompilerTmpEmiiterState.tryEmit config logger cache

            return! loop newState      
        | CompilerTmpEmitterMsg.IncrCompilingNum -> 
            let compilingNumber = state.CompilingNumber + 1

            diagnosticsMessage compilingNumber "Incr"
            
            return! loop {state with CompilingNumber = compilingNumber } 
        | CompilerTmpEmitterMsg.AddTmp projectFile -> return! loop {state with CompilerTmp = state.CompilerTmp.Add projectFile}        
        | CompilerTmpEmitterMsg.Emit replyChannel ->
            diagnosticsMessage state.CompilingNumber "Emit"
            config.BeforeEmitTmp()
            let newState =
                {state with EmitReplyChannels = replyChannel :: state.EmitReplyChannels}
                |> CompilerTmpEmiiterState.tryEmit config logger cache
            return! loop newState
        | CompilerTmpEmitterMsg.AddTask task -> 
            diagnosticsMessage state.CompilingNumber "AddTask"
            return! loop {state with CompilerTasks = task:: state.CompilerTasks}   
        | CompilerTmpEmitterMsg.UpdateCache cache ->
            diagnosticsMessage state.CompilingNumber "Update cache"
            return! loop {state with CrackerFsprojFileBundleCache = cache}   
    }
    loop {CompilingNumber = 0;CompilerTmp = Set.empty;EmitReplyChannels = []; CompilerTasks = []; CrackerFsprojFileBundleCache = cache}
)