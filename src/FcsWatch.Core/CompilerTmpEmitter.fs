module FcsWatch.Core.CompilerTmpEmitter
open Types
open System.Threading


[<RequireQualifiedAccess>]
type CompilerTmpEmitterMsg<'Result when 'Result :> ICompilerOrCheckResult> =
    | IncrCompilingNum of int * replyChannel: AsyncReplyChannel<unit>
    | DecrCompilingNum of int
    | AddTmp of string (*proj path*)
    | AddTask of CompilerTask<'Result>
    | UpdateCache of CrackedFsprojBundleCache
    /// for blackbox test
    | WaitCompiled of AsyncReplyChannel<int>

[<RequireQualifiedAccess>]
type CompilerTmpEmitterMsg<'CustomMsg, 'Result when 'Result :> ICompilerOrCheckResult> = 
    | CustomMsg of 'CustomMsg
    | CommonMsg of  CompilerTmpEmitterMsg<'Result>

[<RequireQualifiedAccess>]
module CompilerTmpEmitterMsg =
    let addTmp projPath =
        (CompilerTmpEmitterMsg<_, _>.CommonMsg (CompilerTmpEmitterMsg<_>.AddTmp projPath))

    let incrCompilingNum (number, replyChannel) =
        (CompilerTmpEmitterMsg<_, _>.CommonMsg (CompilerTmpEmitterMsg<_>.IncrCompilingNum (number, replyChannel)))

    let decrCompilingNum number =
        (CompilerTmpEmitterMsg<_, _>.CommonMsg (CompilerTmpEmitterMsg<_>.DecrCompilingNum number))

    let addTask task =
        (CompilerTmpEmitterMsg<_, _>.CommonMsg (CompilerTmpEmitterMsg<_>.AddTask task))

    let updateCache cache =
        (CompilerTmpEmitterMsg<_, _>.CommonMsg (CompilerTmpEmitterMsg<_>.UpdateCache cache))

    let internal customMsg msg =
        (CompilerTmpEmitterMsg<_, _>.CustomMsg msg)


    /// for blackbox test
    let internal waitCompiled replyChannel =
        (CompilerTmpEmitterMsg<_, _>.CommonMsg (CompilerTmpEmitterMsg<_>.WaitCompiled replyChannel))

    let msgName = function 
        | CompilerTmpEmitterMsg.IncrCompilingNum _ -> "IncrCompilingNum"
        | CompilerTmpEmitterMsg.DecrCompilingNum _ -> "DecrCompilingNum"
        | CompilerTmpEmitterMsg.AddTmp _ -> "AddTmp"
        | CompilerTmpEmitterMsg.AddTask _ -> "AddTask"
        | CompilerTmpEmitterMsg.UpdateCache _ -> "UpdateCache"
        | CompilerTmpEmitterMsg.WaitCompiled _ -> "WaitCompiled"

type CompilerTmpEmitterState<'Result when 'Result :> ICompilerOrCheckResult> =
    { CompilingNumber: int
      /// proj paths
      CompilerTmp: Set<string>
      CachedCompilerTasks: CompilerTask<'Result> list
      /// used for test only
      LastestIncredNum: int
      CrackerFsprojFileBundleCache: CrackedFsprojBundleCache
      AccumWaitCompiledReplyChannels: AsyncReplyChannel<int> list}

type CompilerTmpEmitterState<'CustomState, 'Result when 'Result :> ICompilerOrCheckResult> = 
    { CustomState: 'CustomState
      CommonState: CompilerTmpEmitterState<'Result> }

[<RequireQualifiedAccess>]
module CompilerTmpEmitterState =

    let createCommonEmpty cache =
        { CompilingNumber = 0
          CompilerTmp = Set.empty
          CachedCompilerTasks = []
          LastestIncredNum = 0
          CrackerFsprojFileBundleCache = cache
          AccumWaitCompiledReplyChannels = [] }

    let createEmpty customState cache = 
        { CommonState = createCommonEmpty cache
          CustomState = customState }


    let setCompilingNumber number state =
        { state with 
            CommonState =
                {state.CommonState with CompilingNumber = number }}


    let internal tryReplyCompiled (state: CompilerTmpEmitterState<_>) =
        if state.CompilingNumber = 0 && state.AccumWaitCompiledReplyChannels.Length > 0
        then 
            state.AccumWaitCompiledReplyChannels |> List.iter (fun replyChannel -> replyChannel.Reply state.LastestIncredNum)
            createCommonEmpty state.CrackerFsprojFileBundleCache
        else 
            state

    let processMsg (msg: CompilerTmpEmitterMsg<_>) (state: CompilerTmpEmitterState<_>) =
        let newState = 
            match msg with 

            | CompilerTmpEmitterMsg.DecrCompilingNum number ->
                let compilingNumber = state.CompilingNumber - number
                let newState = {state with CompilingNumber = compilingNumber}

                assert (newState.CompilingNumber >= 0)
                newState

            | CompilerTmpEmitterMsg.IncrCompilingNum (number, replyChannel) -> 
                let compilingNumber = state.CompilingNumber + number
                let newState = 
                    {state with 
                        CompilingNumber = compilingNumber
                        LastestIncredNum = compilingNumber } 

                replyChannel.Reply()

                newState

            | CompilerTmpEmitterMsg.AddTmp projectFile -> 
                let newCompilerTmp = state.CompilerTmp.Add projectFile

                {state with CompilerTmp = newCompilerTmp }        

            | CompilerTmpEmitterMsg.AddTask task -> 
                {state with CachedCompilerTasks = task :: state.CachedCompilerTasks} 

            | CompilerTmpEmitterMsg.UpdateCache cache ->
                {state with CrackerFsprojFileBundleCache = cache} 

            | CompilerTmpEmitterMsg.WaitCompiled replyChannel ->
                let newState = { state with AccumWaitCompiledReplyChannels = replyChannel :: state.AccumWaitCompiledReplyChannels }
                tryReplyCompiled newState

        let msgName = CompilerTmpEmitterMsg.msgName msg
        logger.Info "compilerTmpEmitter agent receive message %s,current compiling number is %d" msgName newState.CompilingNumber

        newState

type ICompilerTmpEmitter<'CustomMsg, 'CustomState,'Result when 'Result :> ICompilerOrCheckResult> =
    abstract member ProcessCustomMsg: customMsg: 'CustomMsg * state: CompilerTmpEmitterState<'CustomState,'Result> -> CompilerTmpEmitterState<'CustomState,'Result>
    abstract member TryEmit: workingDir: string * state: CompilerTmpEmitterState<'CustomState,'Result> -> CompilerTmpEmitterState<'CustomState,'Result>
    abstract member CustomInitialState: 'CustomState

let compilerTmpEmitterAgent workingDir (compilerTmpEmitter:  ICompilerTmpEmitter<_, _, _>) (initialCache: CrackedFsprojBundleCache) = MailboxProcessor<CompilerTmpEmitterMsg<_, _>>.Start(fun inbox ->
    inbox.Error.Add(fun error -> logger.Error "%A" error)

    let rec loop state = async {

        let! msg = inbox.Receive()
        match msg with 
        | CompilerTmpEmitterMsg.CustomMsg customMsg -> 
            let newState = compilerTmpEmitter.ProcessCustomMsg (customMsg, state)
            return! loop newState
        | CompilerTmpEmitterMsg.CommonMsg commonMsg ->

            let commonProcessedState = 
                { state with 
                    CommonState = CompilerTmpEmitterState.processMsg commonMsg state.CommonState }

            match commonMsg with 
            | CompilerTmpEmitterMsg.DecrCompilingNum _ ->

                let resetWaitCompiled state =
                    { state with 
                        CommonState = 
                            { state.CommonState
                                with 
                                    LastestIncredNum = commonProcessedState.CommonState.LastestIncredNum
                                    AccumWaitCompiledReplyChannels = commonProcessedState.CommonState.AccumWaitCompiledReplyChannels } }

                let replyWaitCompiled state =
                    { state with 
                        CommonState = CompilerTmpEmitterState.tryReplyCompiled state.CommonState }

                let newState = 
                    compilerTmpEmitter.TryEmit(workingDir, commonProcessedState)
                    |> resetWaitCompiled
                    |> replyWaitCompiled


                return! loop newState    

            | _ -> return! loop commonProcessedState
    }
    loop (CompilerTmpEmitterState.createEmpty compilerTmpEmitter.CustomInitialState initialCache) 
)




