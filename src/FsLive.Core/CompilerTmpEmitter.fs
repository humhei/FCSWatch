module FsLive.Core.CompilerTmpEmiiter
open FsLive.Core.CrackedFsprojBundle
open System.Threading.Tasks
open System
open FsLive.Core.CrackedFsproj
open Suave
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
type CompilerTmpEmitterMsg =
    | IncrCompilingNum of int
    | DecrCompilingNum of int
    | AddTmp of string (*proj path*)
    | Emit of replyChannel: AsyncReplyChannel<WebPart>
    | AddTask of CompilerTask
    | UpdateCache of CrackedFsprojBundleCache
    | GetCompilerTmp of AsyncReplyChannel<Set<string>>

type CompilerTmpEmitterState =
    { CompilingNumber: int
      /// proj paths
      CompilerTmp: Set<string>
      EmitReplyChannels: AsyncReplyChannel<WebPart> list
      GetTmpReplyChannels: AsyncReplyChannel<Set<string>> list
      CompilerTasks: CompilerTask list
      CrackerFsprojFileBundleCache: CrackedFsprojBundleCache }
    


[<RequireQualifiedAccess>]
module CompilerTmpEmiiterState =
    let createEmpty cache = 
        { CompilingNumber = 0
          CompilerTmp = Set.empty
          EmitReplyChannels = []
          CompilerTasks = []
          CrackerFsprojFileBundleCache = cache
          GetTmpReplyChannels = [] }

type DevelopmentTarget = 
    { CompileOrCheck: FSharpChecker -> CrackedFsproj -> Async<CompileOrCheckResult []>
      TryEmit: Logger.Logger -> Config -> CrackedFsprojBundleCache -> CompilerTmpEmitterState -> CompilerTmpEmitterState }

let compilerTmpEmitter (developmentTarget: DevelopmentTarget) config (initialCache: CrackedFsprojBundleCache) = MailboxProcessor<CompilerTmpEmitterMsg>.Start(fun inbox ->
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
                |> developmentTarget.TryEmit logger config cache 

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
                |> developmentTarget.TryEmit logger config cache 

            return! loop newState
        | CompilerTmpEmitterMsg.AddTask task -> 
            traceMsg state.CompilingNumber "AddTask"
            return! loop {state with CompilerTasks = task :: state.CompilerTasks} 

        | CompilerTmpEmitterMsg.UpdateCache cache ->
            traceMsg state.CompilingNumber "Update cache"

            return! loop {state with CrackerFsprojFileBundleCache = cache} 
            
        | CompilerTmpEmitterMsg.GetCompilerTmp replyChannel ->
            traceMsg state.CompilingNumber "GetCompilerTmp"
            let newState = 
                {state with GetTmpReplyChannels = replyChannel :: state.GetTmpReplyChannels }
                |> developmentTarget.TryEmit logger config cache 

            return! loop newState
    }
    loop (CompilerTmpEmiiterState.createEmpty initialCache) 
)