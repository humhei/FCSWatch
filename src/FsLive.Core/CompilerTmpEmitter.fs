module FsLive.Core.CompilerTmpEmiiter
open FsLive.Core.CrackedFsprojBundle
open System.Threading.Tasks
open System
open FsLive.Core.CrackedFsproj
open Microsoft.FSharp.Compiler.SourceCodeServices



type CompilerTask = CompilerTask of startTime: DateTime * task: Task<CompileOrCheckResult list>
with 
    member x.StartTime = 
        match x with 
        | CompilerTask(startTime = m) -> m
    member x.Task = 
        match x with 
        | CompilerTask(task = m) -> m

[<RequireQualifiedAccess>]
type CompilerTmpEmitterMsg<'EmitReply> =
    | IncrCompilingNum of int
    | DecrCompilingNum of int
    | AddTmp of string (*proj path*)
    | Emit of replyChannel: AsyncReplyChannel<'EmitReply>
    | AddTask of CompilerTask
    | UpdateCache of CrackedFsprojBundleCache

type CompilerTmpEmitterState<'EmitReply> =
    { CompilingNumber: int
      /// proj paths
      CompilerTmp: Set<string>
      EmitReplyChannels: AsyncReplyChannel<'EmitReply> list
      CompilerTasks: CompilerTask list
      CrackerFsprojFileBundleCache: CrackedFsprojBundleCache }
    


[<RequireQualifiedAccess>]
module CompilerTmpEmiiterState =
    let createEmpty cache = 
        { CompilingNumber = 0
          CompilerTmp = Set.empty
          EmitReplyChannels = []
          CompilerTasks = []
          CrackerFsprojFileBundleCache = cache }

type DevelopmentTarget<'EmitReply> = 
    { CompileOrCheck: FSharpChecker -> CrackedFsproj -> Async<CompileOrCheckResult []>
      TryEmit: Logger.Logger -> CompilerTmpEmitterState<'EmitReply> -> CompilerTmpEmitterState<'EmitReply>
      StartDebuggingServer: Config -> MailboxProcessor<CompilerTmpEmitterMsg<'EmitReply>> -> unit }

let compilerTmpEmitter  (developmentTarget: DevelopmentTarget<'EmitReply>) config (initialCache: CrackedFsprojBundleCache) = MailboxProcessor<CompilerTmpEmitterMsg<'EmitReply>>.Start(fun inbox ->
    developmentTarget.StartDebuggingServer config inbox
    let rec loop state = async {
        let cache = state.CrackerFsprojFileBundleCache        
        let traceMsg compilingNumber msg = 
            logger.Info "compilerTmpEmitter agent receive message %s,current compiling number is %d" msg compilingNumber

        let! msg = inbox.Receive()
        match msg with 
        | CompilerTmpEmitterMsg.DecrCompilingNum number ->
            let compilingNumber = state.CompilingNumber - number

            traceMsg compilingNumber "DecrCompilingNum"

            assert (state.CompilingNumber > 0)
            let newState = 
                {state with CompilingNumber = compilingNumber}
                |> developmentTarget.TryEmit logger

            return! loop newState    

        | CompilerTmpEmitterMsg.IncrCompilingNum number -> 
            let compilingNumber = state.CompilingNumber + number

            traceMsg compilingNumber "IncrCompilingNum"

            return! loop {state with CompilingNumber = compilingNumber } 

        | CompilerTmpEmitterMsg.AddTmp projectFile -> 
            let newCompilerTmp = state.CompilerTmp.Add projectFile

            return! loop {state with CompilerTmp = newCompilerTmp }        
        | CompilerTmpEmitterMsg.Emit replyChannel ->
            traceMsg state.CompilingNumber "Emit"

            let newState =
                {state with EmitReplyChannels = replyChannel :: state.EmitReplyChannels}
                |> developmentTarget.TryEmit logger 

            return! loop newState
        | CompilerTmpEmitterMsg.AddTask task -> 
            traceMsg state.CompilingNumber "AddTask"
            return! loop {state with CompilerTasks = task :: state.CompilerTasks} 

        | CompilerTmpEmitterMsg.UpdateCache cache ->
            traceMsg state.CompilingNumber "Update cache"

            return! loop {state with CrackerFsprojFileBundleCache = cache} 
            
    }
    loop (CompilerTmpEmiiterState.createEmpty initialCache) 
)