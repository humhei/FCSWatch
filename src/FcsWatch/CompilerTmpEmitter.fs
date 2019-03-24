namespace FcsWatch


[<RequireQualifiedAccess>]
type DevelopmentTarget =
    | Debuggable of DebuggingServer.DevelopmentTarget
    | AutoReload of AutoReload.DevelopmentTarget

[<RequireQualifiedAccess>]
module DevelopmentTarget =
    let debuggableProgram = DevelopmentTarget.Debuggable DebuggingServer.DevelopmentTarget.Program
    let autoReloadProgram = DevelopmentTarget.AutoReload AutoReload.DevelopmentTarget.Program
    let autoReloadPlugin plugin = DevelopmentTarget.AutoReload (AutoReload.DevelopmentTarget.Plugin plugin)
    let debuggablePlugin plugin = DevelopmentTarget.Debuggable (DebuggingServer.DevelopmentTarget.Plugin plugin)

module CompilerTmpEmitter =

    [<RequireQualifiedAccess>]
    type CompilerTmpEmitterMsg =
        | AutoReload of DebuggingServer.TmpEmitterMsg
        | DebuggingServer of AutoReload.TmpEmitterMsg



    [<RequireQualifiedAccess>]
    type CompilerTmpEmitterState =
        | AutoReload of AutoReload.TmpEmitterState
        | DebuggingServer of DebuggingServer.TmpEmitterState

    with 
        member x.CompilingNumber = 
            match x with 
            | CompilerTmpEmitterState.AutoReload autoReload -> autoReload.CompilingNumber
            | CompilerTmpEmitterState.DebuggingServer debuggingServer -> debuggingServer.CompilingNumber


    [<RequireQualifiedAccess>]
    type CompilerTmpEmitter =
        | AutoReload of MailboxProcessor<AutoReload.TmpEmitterMsg>
        | DebuggingServer of MailboxProcessor<DebuggingServer.TmpEmitterMsg>

    with 
        member x.Post msg = 
            match x with 
            | CompilerTmpEmitter.AutoReload autoReload ->
                autoReload.Post msg
            | CompilerTmpEmitter.DebuggingServer debuggingServer ->
                debuggingServer.Post (DebuggingServer.upcastMsg msg)


    [<RequireQualifiedAccess>]
    module CompilerTmpEmitter =
        let create developmentTarget initialCache workingDir =
            match developmentTarget with 
            | DevelopmentTarget.AutoReload autoReload ->
                AutoReload.create workingDir autoReload initialCache
                |> CompilerTmpEmitter.AutoReload

            | DevelopmentTarget.Debuggable debuggable ->
                DebuggingServer.create debuggable initialCache workingDir
                |> CompilerTmpEmitter.DebuggingServer