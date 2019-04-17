namespace FcsWatch.Core
open Fake.Core


[<RequireQualifiedAccess>]
module Logger =
    open System

    [<RequireQualifiedAccess>]
    type Level =
        | Minimal
        | Normal
        | Quiet
        | Debug

    type Logger internal (level) =

        let timeStamp (time:DateTime) = time.ToString("yyyy-MM-dd HH:mm:ss.fff")

        let withTimeStamp (f: string -> unit) =
            fun message ->
                let now = timeStamp DateTime.UtcNow
                sprintf "%s %s" now message
                |> f


        let _debug trace message =
            match level with
            | Level.Minimal -> ()
            | Level.Normal -> ()
            | Level.Quiet -> ()
            | Level.Debug -> trace message


        let _info trace message =
            match level with
            | Level.Minimal -> ()
            | Level.Normal -> ()
            | Level.Quiet -> ()
            | Level.Debug -> ()

        let _important trace message =
            match level with
            | Level.Quiet -> ()
            | _ -> trace message

        let _warn message = Trace.traceImportant message

        let _error message = Trace.traceError message

        member x.Info format =
            Printf.ksprintf (_info Trace.log) format

        member x.Debug format =
            Printf.ksprintf (_debug Trace.log) format

        member x.InfoGreen format =
            Printf.ksprintf (_info Trace.trace) format

        member x.Diagnostics text =
            System.Diagnostics.Debugger.Log(1,"",sprintf "%s %s\n" (timeStamp DateTime.UtcNow) text)

        /// with timeStamp
        member x.Infots format =
            Printf.ksprintf (withTimeStamp (_info Trace.log)) format

        member x.InfoGreents format =
            Printf.ksprintf (withTimeStamp (_info Trace.trace)) format

        /// LG = light gray
        member x.ImportantLG format =
            Printf.ksprintf (_important Trace.log) format

        member x.Important format =  Printf.ksprintf (_important (printfn "%s")) format

        member x.ImportantGreen format =
            Printf.ksprintf (_important Trace.trace) format

        member x.Importantts format =
            Printf.ksprintf (withTimeStamp (_important Trace.log)) format

        member x.ImportantGreents format =
            Printf.ksprintf (withTimeStamp (_important Trace.trace)) format

        member x.Warn format =
            Printf.ksprintf _warn format

        member x.Error format =
            Printf.ksprintf _error format

    let create level = Logger(level)
