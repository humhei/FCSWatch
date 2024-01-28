namespace FcsWatch.Core
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open FSharp.Compiler.CodeAnalysis

type SerializableFSharpDiagnostic =
    { Range: range 
      Message: string 
      Severity: FSharpDiagnosticSeverity
      Subcategory: string 
      ErrorNum: int 
      NumberPrefix: string 
}

[<RequireQualifiedAccess>]
module SerializableFSharpDiagnostic =
    let ofFSharpDiagnostic(info: FSharpDiagnostic) =
        { Range = info.Range 
          Message = info.Message
          Severity = info.Severity
          Subcategory = info.Subcategory
          ErrorNum = info.ErrorNumber
          NumberPrefix = info.ErrorNumberPrefix }


type SerializableFSharpCheckFileResults =
    { Diagnostics: SerializableFSharpDiagnostic [] }



[<RequireQualifiedAccess>]
module SerializableFSharpCheckFileResults =
    let ofFSharpCheckFileResults(info: FSharpCheckFileResults) =
        { Diagnostics = Array.map SerializableFSharpDiagnostic.ofFSharpDiagnostic info.Diagnostics  }

[<RequireQualifiedAccess>]
type SerializableFSharpCheckFileAnswer =
    | Succeeded of SerializableFSharpCheckFileResults
    | Aborted

module SerializableFSharpCheckFileAnswer =
    let ofFSharpCheckFileAnswer(answer: FSharpCheckFileAnswer) =
        match answer with 
        | FSharpCheckFileAnswer.Succeeded results -> 
            SerializableFSharpCheckFileResults.ofFSharpCheckFileResults results
            |> SerializableFSharpCheckFileAnswer.Succeeded

        | FSharpCheckFileAnswer.Aborted -> SerializableFSharpCheckFileAnswer.Aborted


type IMailboxProcessor<'Msg> =
    abstract member PostAndReply: buildMsg: (AsyncReplyChannel<'Reply> -> 'Msg) -> 'Reply
    abstract member PostAndAsyncReply: buildMsg: (AsyncReplyChannel<'Reply> -> 'Msg) -> Async<'Reply>

[<AutoOpen>]
module internal Extensions =


    [<RequireQualifiedAccess>]
    module internal Path =
        let nomalizeToUnixCompatiable path =
            let path = (Path.getFullName path).Replace('\\','/')

            let dir = Path.getDirectory path

            let segaments =
                let fileName = Path.GetFileName path
                fileName.Split([|'\\'; '/'|])

            let folder dir segament =
                dir </> segament
                |> Path.getFullName

            segaments
            |> Array.fold folder dir


    [<RequireQualifiedAccess>]
    module internal Dictionary =

        let toMap dictionary =
            (dictionary :> seq<_>)
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq


    [<RequireQualifiedAccess>]
    module internal MailboxProcessor =
        let toChildInterface (mapping: 'ChildMsg -> 'Msg) (agent: MailboxProcessor<'Msg>) =
            { new IMailboxProcessor<'ChildMsg> with 
                member x.PostAndReply buildMsg =
                    agent.PostAndReply (fun replyChannel -> mapping (buildMsg replyChannel))

                member x.PostAndAsyncReply buildMsg =
                    agent.PostAndAsyncReply(fun replyChannel -> mapping (buildMsg replyChannel))
            }


[<AutoOpen>]
module internal Global =
    open Fake.Core

    let mutable logger = Logger.create (Logger.Level.Minimal)

    type internal Logger.Logger with
        member x.ProcessCompileOrCheckResult (errors: FSharpDiagnostic [],exitCode) =
            if exitCode = 0 then
                if not <| Array.isEmpty errors then
                    logger.Warn "WARNINGS:\n%A" errors

            else logger.Error "ERRORS:\n%A" errors

[<RequireQualifiedAccess>]
module FileSystem =
    let internal editDirAndFile (fileName: string) (useEditFiles) =
        assert useEditFiles
        let infoDir = Path.Combine(Path.GetDirectoryName fileName,".fsharp")
        let editFile = Path.Combine(infoDir,Path.GetFileName fileName + ".edit")
        if not (Directory.Exists infoDir) then 
            Directory.CreateDirectory infoDir |> ignore
        infoDir, editFile

    let readFile (fileName: string) (useEditFiles) = 
        if useEditFiles then 
            let infoDir, editFile = editDirAndFile fileName useEditFiles
            let preferEditFile =
                try 
                    Directory.Exists infoDir && File.Exists editFile && File.Exists fileName && File.GetLastWriteTime(editFile) > File.GetLastWriteTime(fileName)
                with _ -> 
                    false
            if preferEditFile then 
                logger.Info "*** preferring %s to %s ***" editFile fileName
                File.ReadAllText editFile
            else
                File.ReadAllText fileName
        else
            File.ReadAllText fileName





