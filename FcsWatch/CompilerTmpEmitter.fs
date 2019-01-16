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

type CompilerTmpEmitterState =
    {
        CompilingNumber: int
        CompilerTmp: Set<string>
        EmitReplyChannels: AsyncReplyChannel<HttpHandler> list
        CompilerTasks: CompilerTask list
    }
    
[<RequireQualifiedAccess>]
module CompilerTmpEmiiterState =
    let empty = 
            {
                CompilingNumber = 0
                CompilerTmp = Set.empty
                EmitReplyChannels = []
                CompilerTasks = []
            }
    let tryEmit logger cache compilerTmpEmiiterState =
        match compilerTmpEmiiterState.CompilingNumber,compilerTmpEmiiterState.EmitReplyChannels with 
        | 0, h::t ->
            t |> List.iter (fun replyChannel -> replyChannel.Reply(RequestErrors.GONE "request is cancelled"))
            match compilerTmpEmiiterState.CompilerTasks with
            | [] -> 
                h.Reply (Successful.OK "fcswatch: Ready to debug")
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
                    h.Reply (Successful.OK "fcswatch: Ready to debug")
                    empty
        | _ -> compilerTmpEmiiterState    


let compilerTmpEmitter config (bundleProjectAgent: MailboxProcessor<CrackedFsprojBundleMsg>) = MailboxProcessor<CompilerTmpEmitterMsg>.Start(fun inbox ->
    let logger = config.Logger
    let cache = bundleProjectAgent.PostAndReply CrackedFsprojBundleMsg.Cache
    let rec loop state = async {
        let diagnosticsMessage msg = 
            sprintf "receive message %s,current compiling number is %d" msg state.CompilingNumber
            |> Logger.diagnostics

        let! msg = inbox.Receive()
        match msg with 
        | CompilerTmpEmitterMsg.DecrCompilingNum -> 
            diagnosticsMessage "Decr"
            assert (state.CompilingNumber > 0)
            let newState = 
                {state with CompilingNumber = state.CompilingNumber - 1}
                |> CompilerTmpEmiiterState.tryEmit logger cache

            return! loop newState      
        | CompilerTmpEmitterMsg.IncrCompilingNum -> 
            diagnosticsMessage "Incr"
            
            return! loop {state with CompilingNumber = state.CompilingNumber + 1} 
        | CompilerTmpEmitterMsg.AddTmp projectFile -> return! loop {state with CompilerTmp = state.CompilerTmp.Add projectFile}        
        | CompilerTmpEmitterMsg.Emit replyChannel ->
            diagnosticsMessage "Emit"
            let newState =
                {state with EmitReplyChannels = replyChannel :: state.EmitReplyChannels}
                |> CompilerTmpEmiiterState.tryEmit logger cache
            return! loop newState
        | CompilerTmpEmitterMsg.AddTask task -> 
            diagnosticsMessage "AddTask"
            return! loop {state with CompilerTasks = task:: state.CompilerTasks}        
    }
    loop {CompilingNumber = 0;CompilerTmp = Set.empty;EmitReplyChannels = []; CompilerTasks = []}
)