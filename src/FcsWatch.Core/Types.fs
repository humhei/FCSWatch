namespace FcsWatch.Core
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open FSharp.Compiler.CodeAnalysis

type SerializableRange =    
    { FileName:string  
      Start: Position
      End: Position
      Line: int }

[<RequireQualifiedAccess>]
module SerializableRange =
    let ofRange (range: range) =
        { FileName = range.FileName 
          Start = range.Start
          End = range.End
          Line = range.EndLine }

type SerializableFSharpDiagnostic =
    { Range: SerializableRange
      Message: string 
      Severity: FSharpDiagnosticSeverity
      Subcategory: string 
      ErrorNum: int 
      NumberPrefix: string 
}
with 
    member x.Start = x.Range.Start
    member x.End = x.Range.End
    member x.FileName = x.Range.FileName

    override m.ToString() =
        let fileName = m.FileName
        let s = m.Start
        let e = m.End
        let severity = 
            match m.Severity with
            | FSharpDiagnosticSeverity.Warning -> "warning"
            | FSharpDiagnosticSeverity.Error -> "error"
            | FSharpDiagnosticSeverity.Info -> "info"
            | FSharpDiagnosticSeverity.Hidden -> "hidden"
        sprintf "%s (%d,%d)-(%d,%d) %s %s %s" fileName s.Line (s.Column + 1) e.Line (e.Column + 1) m.Subcategory severity m.Message


[<RequireQualifiedAccess>]
module SerializableFSharpDiagnostic =
    let ofFSharpDiagnostic(info: FSharpDiagnostic) =
        { Range = SerializableRange.ofRange info.Range 
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

[<RequireQualifiedAccess>]
module SerializableFSharpCheckFileAnswer =
    let ofFSharpCheckFileAnswer(answer: FSharpCheckFileAnswer) =
        match answer with 
        | FSharpCheckFileAnswer.Succeeded results -> 
            SerializableFSharpCheckFileResults.ofFSharpCheckFileResults results
            |> SerializableFSharpCheckFileAnswer.Succeeded

        | FSharpCheckFileAnswer.Aborted -> SerializableFSharpCheckFileAnswer.Aborted

type IRemoteFSharpChecker =
    abstract member Compile_Serializable: argv: string [] * ?userOpName: string -> Async<SerializableFSharpDiagnostic [] * int>
    abstract member ParseAndCheckFileInProject_Serializable:     
        fileName: string 
        * fileVersion: int
        * sourceText: string
        * options: FSharpProjectOptions
        * ?userOpName: string -> Async<SerializableFSharpCheckFileAnswer>
            
    abstract member GetProjectOptionsFromScript_Serializable: fileName: string * source: string * ?previewEnabled: bool * ?loadedTimeStamp: System.DateTime * ?otherFlags: string array * ?useFsiAuxLib: bool * ?useSdkRefs: bool * ?assumeDotNetFramework: bool * ?sdkDirOverride: string * ?optionsStamp: int64 * ?userOpName: string -> Async<FSharpProjectOptions * SerializableFSharpDiagnostic list>

    abstract member GetProjectOptionsFromCommandLineArgs: projectFileName: string * argv: string array * ?loadedTimeStamp: System.DateTime * ?isInteractive: bool * ?isEditing: bool -> FSharpProjectOptions

[<RequireQualifiedAccess>]
type RemotableFSharpChecker =
    | FSharpChecker of FSharpChecker
    | Remote of IRemoteFSharpChecker
with 
    member x.GetProjectOptionsFromCommandLineArgs(projectFileName: string , argv: string array , ?loadedTimeStamp: System.DateTime , ?isInteractive: bool , ?isEditing: bool) =
        match x with 
        | FSharpChecker checker -> 
            checker.GetProjectOptionsFromCommandLineArgs
                (projectFileName,
                 argv,
                 ?loadedTimeStamp = loadedTimeStamp,
                 ?isInteractive = isInteractive,
                 ?isEditing = isEditing
            )

        | Remote checker -> 
            checker.GetProjectOptionsFromCommandLineArgs
                (projectFileName,
                 argv,
                 ?loadedTimeStamp = loadedTimeStamp,
                 ?isInteractive = isInteractive,
                 ?isEditing = isEditing
            )
    

    member x.GetProjectOptionsFromScript_Serializable(fileName: string , source: string , ?previewEnabled: bool , ?loadedTimeStamp: System.DateTime , ?otherFlags: string array , ?useFsiAuxLib: bool , ?useSdkRefs: bool , ?assumeDotNetFramework: bool , ?sdkDirOverride: string , ?optionsStamp: int64 , ?userOpName: string) =
        async {
            match x with 
            | FSharpChecker checker ->
                let! (fsharpOptions, fsharpErrors) =
                    checker.GetProjectOptionsFromScript(
                        fileName,
                        SourceText.ofString source,
                        ?previewEnabled = previewEnabled,
                        ?loadedTimeStamp =loadedTimeStamp,
                        ?otherFlags = otherFlags,
                        ?useFsiAuxLib = useFsiAuxLib,
                        ?useSdkRefs = useSdkRefs,
                        ?assumeDotNetFramework = assumeDotNetFramework,
                        ?sdkDirOverride = sdkDirOverride,
                        ?optionsStamp = optionsStamp,
                        ?userOpName = userOpName
                    )
                let fsharpErrors = List.map SerializableFSharpDiagnostic.ofFSharpDiagnostic fsharpErrors
                return (fsharpOptions, fsharpErrors)


            | Remote checker -> 
                return!
                    checker.GetProjectOptionsFromScript_Serializable(
                        fileName,
                        source,
                        ?previewEnabled = previewEnabled,
                        ?loadedTimeStamp =loadedTimeStamp,
                        ?otherFlags = otherFlags,
                        ?useFsiAuxLib = useFsiAuxLib,
                        ?useSdkRefs = useSdkRefs,
                        ?assumeDotNetFramework = assumeDotNetFramework,
                        ?sdkDirOverride = sdkDirOverride,
                        ?optionsStamp = optionsStamp,
                        ?userOpName = userOpName
                    )

        }


    member private x.GetProjectOptionsFromScript(fileName: string , source: FSharp.Compiler.Text.ISourceText , ?previewEnabled: bool , ?loadedTimeStamp: System.DateTime , ?otherFlags: string array , ?useFsiAuxLib: bool , ?useSdkRefs: bool , ?assumeDotNetFramework: bool , ?sdkDirOverride: string , ?optionsStamp: int64 , ?userOpName: string) =
        match x with 
        | FSharpChecker checker -> 
            checker.GetProjectOptionsFromScript(
                fileName,
                source,
                ?previewEnabled = previewEnabled,
                ?loadedTimeStamp =loadedTimeStamp,
                ?otherFlags = otherFlags,
                ?useFsiAuxLib = useFsiAuxLib,
                ?useSdkRefs = useSdkRefs,
                ?assumeDotNetFramework = assumeDotNetFramework,
                ?sdkDirOverride = sdkDirOverride,
                ?optionsStamp = optionsStamp,
                ?userOpName = userOpName)

        | Remote _ -> failwithf "Invalid token. RemoteFSharpChecker only support GetProjectOptionsFromScript_Serializable"


    member x.Compile_Serializable(argv, ?userOpName) = 
        async {
            match x with 
            | FSharpChecker checker -> 
                let! (errors, exitCode)  = checker.Compile(argv, ?userOpName = userOpName)
                let errors = Array.map SerializableFSharpDiagnostic.ofFSharpDiagnostic errors 
                return (errors, exitCode)

            | Remote checker -> return! checker.Compile_Serializable(argv, ?userOpName = userOpName)
        }
        
    member private x.Compile(argv, ?userOpName) = 
        match x with 
        | FSharpChecker checker -> checker.Compile(argv, ?userOpName = userOpName)
        | Remote _ -> failwithf "Invalid token. RemoteFSharpChecker only support Compile_Serializable"

    member private x.ParseAndCheckFileInProject(fileName, fileVersion, sourceText, options, ?userOpName) =
        match x with 
        | FSharpChecker checker -> 
            checker.ParseAndCheckFileInProject(fileName, fileVersion, sourceText, options, ?userOpName = userOpName)
        | Remote _ -> failwithf "Invalid token. RemoteFSharpChecker only support ParseAndCheckFileInProject_Serializable"

    member x.ParseAndCheckFileInProject_Serializable(fileName, fileVersion, sourceText: string, options, ?userOpName) =
        async {
            match x with 
            | FSharpChecker checker -> 
                let! (_, checkAnswer) = checker.ParseAndCheckFileInProject(fileName, fileVersion, SourceText.ofString sourceText, options, ?userOpName = userOpName)
                let checkAnswer = SerializableFSharpCheckFileAnswer.ofFSharpCheckFileAnswer checkAnswer
                return checkAnswer
            
            | Remote checker ->
                return! checker.ParseAndCheckFileInProject_Serializable(fileName, fileVersion, sourceText, options, ?userOpName = userOpName)

        }


type IMailboxProcessor<'Msg> =
    inherit System.IDisposable
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

                member x.Dispose() = agent.Dispose()
            }


[<AutoOpen>]
module internal Global =
    open Fake.Core

    let mutable logger = Logger.create (Logger.Level.Minimal)

    type internal Logger.Logger with
        member x.ProcessCompileOrCheckResult (errors: SerializableFSharpDiagnostic [],exitCode) =
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





