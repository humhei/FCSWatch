module FcsWatch.CompilerTmpEmitter
open AutoReload
open FcsWatch.DebuggingServer


[<RequireQualifiedAccess>]
type CompilerTmpEmitterMsg =
    | AutoReload of AutoReloadTmpEmitterMsg
    | DebuggingServer of DebuggingServerMsg



[<RequireQualifiedAccess>]
type CompilerTmpEmitterState =
    | AutoReload of AutoReloadTmpEmitterState
    | DebuggingServer of DebuggingServer.DebuggingServerState

with 
    member x.CompilingNumber = 
        match x with 
        | CompilerTmpEmitterState.AutoReload autoReload -> autoReload.CompilingNumber
        | CompilerTmpEmitterState.DebuggingServer debuggingServer -> debuggingServer.CompilingNumber


[<RequireQualifiedAccess>]
module CompilerTmpEmitterState =

    let tryEmit developmentTarget = function 
        | CompilerTmpEmitterState.AutoReload autoReload -> 
            AutoReloadTmpEmitterState.tryEmit developmentTarget autoReload
            |> CompilerTmpEmitterState.AutoReload

        | CompilerTmpEmitterState.DebuggingServer debuggingServer -> 
            DebuggingServerState.tryEmit developmentTarget debuggingServer
            |> CompilerTmpEmitterState.DebuggingServer

    let setCompilingNumber number = function
        | CompilerTmpEmitterState.AutoReload autoReload -> 
            AutoReloadTmpEmitterState.setCompilingNumber number autoReload
            |> CompilerTmpEmitterState.AutoReload

        | CompilerTmpEmitterState.DebuggingServer debuggingServer -> 
            DebuggingServerState.setCompilingNumber number debuggingServer
            |> CompilerTmpEmitterState.DebuggingServer


[<RequireQualifiedAccess>]
type CompilerTmpEmitter =
    | AutoReload of MailboxProcessor<AutoReloadTmpEmitterMsg>
    | DebuggingServer of MailboxProcessor<DebuggingServerMsg>

with 
    member x.Post msg = 
        match x with 
        | CompilerTmpEmitter.AutoReload autoReload ->
            autoReload.Post msg
        | CompilerTmpEmitter.DebuggingServer debuggingServer ->
            debuggingServer.Post !^ (msg)


[<RequireQualifiedAccess>]
module CompilerTmpEmitter =
    let create autoReload developmentTarget initialCache workingDir =
        if autoReload 
        then 
            autoReloadTmpEmitter developmentTarget initialCache
            |> CompilerTmpEmitter.AutoReload
        else 
            debuggingServer developmentTarget initialCache workingDir
            |> CompilerTmpEmitter.DebuggingServer