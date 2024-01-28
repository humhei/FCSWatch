namespace FcsWatch.Porta

open System.IO
open Fake.IO
open FcsWatch.Core.CrackedFsproj
open FcsWatch.Core
open FcsWatch.Core.Types
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text

type CheckResult =
    { ExitCode: int 
      Errors: FSharpDiagnostic [] 
      Fsproj: SingleTargetCrackedFsproj
      Contents: FSharpImplementationFileContents list }
with 
    interface ICompilerOrCheckResult with
        member x.ExitCode = x.ExitCode
        member x.Errors = Array.map SerializableFSharpDiagnostic.ofFSharpDiagnostic x.Errors
        member x.ProjPath = x.Fsproj.ProjPath

[<AutoOpen>]
module internal Global =
    let mutable logger = Logger.create Logger.Level.Minimal

module Extensions =

    [<RequireQualifiedAccess>]
    module SingleTargetCrackedFsproj =

        let private mapProjOptionsWithoutSourceFiles (projOptions: FSharpProjectOptions) =
            { projOptions with 
                OtherOptions = 
                    projOptions.OtherOptions
                    |> Array.filter (fun (op: string) -> not (op.EndsWith ".fs" || op.EndsWith ".fsi" || op.EndsWith ".fsx"))
            }
        let check useEditFiles liveCheckOnly (checker: FSharpChecker) (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) = async {
            let rec checkFile1 count sourceFile =         
                try 
                    let _, checkResults =
                        let sourceFileText = 
                            FileSystem.readFile sourceFile useEditFiles
                            |> SourceText.ofString
                            
                        checker.ParseAndCheckFileInProject(sourceFile, 0, sourceFileText , mapProjOptionsWithoutSourceFiles singleTargetCrackedFsproj.FSharpProjectOptions) |> Async.RunSynchronously  
                    match checkResults with 
                    | FSharpCheckFileAnswer.Aborted -> 
                        logger.Important "aborted"
                        Result.Error (None,[||])

                    | FSharpCheckFileAnswer.Succeeded res -> 
                        let mutable hasErrors = false
                        for error in res.Diagnostics do 
                            if error.Severity = FSharpDiagnosticSeverity.Error then 
                                hasErrors <- true

                        if hasErrors then 
                            Result.Error (res.ImplementationFile, res.Diagnostics)
                        else
                            Result.Ok res.ImplementationFile 
                with 
                | :? System.IO.IOException when count = 0 -> 
                    System.Threading.Thread.Sleep 500
                    checkFile1 1 sourceFile
                | exn -> 
                    logger.Error "%s" (exn.ToString())
                    Result.Error (None,[||])

            let checkFile2 file =
                match checkFile1 0 (Path.GetFullPath(file)) with 
                // Note, if livechecks are on, we continue on regardless of errors
                | Result.Error (iopt, errors) when not liveCheckOnly -> 
                    logger.Important "fscd: ERRORS for %s" file
                    Result.Error errors 
                | Result.Error (iopt, errors) ->
                    match iopt with 
                    | None -> Result.Error errors
                    | Some i -> 
                        logger.Important "fscd: GOT PortaCode for %s" file
                        Result.Ok (i,errors)
                | Result.Ok iopt -> 
                    match iopt with 
                    | None -> Result.Error [||]
                    | Some i -> 
                        logger.Important "fscd: GOT PortaCode for %s" file
                        Result.Ok (i, [||])

            let sourceFiles = singleTargetCrackedFsproj.SourceFiles
            let allResults =
                sourceFiles 
                |> Array.map checkFile2

            let isError result =
                match result with 
                | Result.Error _ -> true
                | Result.Ok _ -> false

            return 
                if Array.exists isError allResults
                then 
                    { ExitCode = -1 
                      Errors = 
                        allResults |> Array.choose (fun result ->
                            match result with 
                            | Result.Error errors -> Some (errors)
                            | Result.Ok (_,errors) -> Some (errors)
                        )
                        |> Array.concat
                      Fsproj = singleTargetCrackedFsproj
                      Contents = [] }
                else 
                    let contents, errors =
                        allResults 
                        |> Array.choose (fun result ->
                            match result with 
                            | Result.Error _ -> None
                            | Result.Ok (contents, errors) -> Some (contents, errors)
                        )
                        |> Array.unzip

                    { ExitCode = 0
                      Contents = List.ofArray contents 
                      Errors = Array.concat errors
                      Fsproj = singleTargetCrackedFsproj }
        }


    [<RequireQualifiedAccess>]
    module CrackedFsproj =

        let check useEditFiles liveCheckOnly (checker: FSharpChecker) (crackedFsproj: CrackedFsproj) =
            SingleTargetCrackedFsproj.check useEditFiles liveCheckOnly checker crackedFsproj.PreferSingleTargetCrackedFsproj

