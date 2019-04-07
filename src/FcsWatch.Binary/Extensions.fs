namespace FcsWatch.Binary
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open FcsWatch.Core.CrackedFsproj
open FSharp.Compiler.SourceCodeServices
open FcsWatch.Core

type CompilerResult =
    { Dll: string
      Errors: FSharpErrorInfo []
      ExitCode: int
      ProjPath: string }
with
    member x.Pdb = Path.changeExtension ".pdb" x.Dll

    interface ICompilerOrCheckResult with 
        member x.Errors = x.Errors
        member x.ExitCode = x.ExitCode
        member x.ProjPath = x.ProjPath

[<AutoOpen>]
module Global =
    let mutable logger = Logger.create Logger.Level.Minimal

module Extensions =


    type internal Logger.Logger with
        member x.CopyFile src dest =
            File.Copy(src,dest,true)
            logger.Important "%s ->\n%s" src dest

    [<RequireQualifiedAccess>]
    module SingleTargetCrackedFsproj =
        let copyFileFromRefDllToBin originProjectFile (destCrackedFsprojSingleTarget: SingleTargetCrackedFsproj) =

            let targetDir = destCrackedFsprojSingleTarget.TargetDir

            let originDll =
                let projName = Path.GetFileNameWithoutExtension originProjectFile

                destCrackedFsprojSingleTarget.RefDlls
                |> Array.find(fun refDll -> Path.GetFileNameWithoutExtension refDll = projName)

            let fileName = Path.GetFileName originDll

            let destDll = targetDir </> fileName

            logger.CopyFile originDll destDll

            let originPdb = originDll |> Path.changeExtension ".pdb"

            let destPdb = targetDir </> (Path.changeExtension ".pdb" fileName)

            logger.CopyFile originPdb destPdb

        let copyObjToBin (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            logger.CopyFile singleTargetCrackedFsproj.ObjTargetFile singleTargetCrackedFsproj.TargetPath
            logger.CopyFile singleTargetCrackedFsproj.ObjTargetPdb singleTargetCrackedFsproj.TargetPdbPath


        let compile (checker: FSharpChecker) (crackedProjectSingleTarget: SingleTargetCrackedFsproj) = async {
            let tmpDll = crackedProjectSingleTarget.ObjTargetFile

            let baseOptions =
                crackedProjectSingleTarget.FSharpProjectOptions.OtherOptions
                |> Array.map (fun op -> if op.StartsWith "-o:" then "-o:" + tmpDll else op)

            let fscArgs = Array.concat [[|"fsc.exe"|]; baseOptions;[|"--nowin32manifest"|]]
            let! errors,exitCode = checker.Compile(fscArgs)
            return
                { Errors = errors
                  ExitCode = exitCode
                  Dll = tmpDll
                  ProjPath = crackedProjectSingleTarget.ProjPath }
        }

    [<RequireQualifiedAccess>]
    module CrackedFsproj =

        let copyFileFromRefDllToBin projectFile (destCrackedFsproj: CrackedFsproj) =
            destCrackedFsproj.AsList
            |> List.iter (SingleTargetCrackedFsproj.copyFileFromRefDllToBin projectFile)

        let copyObjToBin (crackedFsproj: CrackedFsproj) =
            crackedFsproj.AsList |> List.iter SingleTargetCrackedFsproj.copyObjToBin


        let compile (checker: FSharpChecker) (crackedFsProj: CrackedFsproj) =
            crackedFsProj.AsList
            |> List.map (SingleTargetCrackedFsproj.compile checker)
            |> Async.Parallel