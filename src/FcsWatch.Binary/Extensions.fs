namespace FcsWatch.Binary
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open FcsWatch.Core.CrackedFsproj
open FcsWatch.Core
open Fake.Core
open FcsWatch.Core.FullCrackedFsproj
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open System.Diagnostics
open FSharp.Compiler.Text




type CompilerResult =
    { Dll: string
      Errors: SerializableFSharpDiagnostic []
      ExitCode: int
      ProjPath: string }
with
    member x.Pdb = Path.changeExtension ".pdb" x.Dll

    interface ICompilerOrCheckResult with 
        member x.Errors = x.Errors
        member x.ExitCode = x.ExitCode
        member x.ProjPath = x.ProjPath

[<AutoOpen>]
module internal Global =
    let mutable logger = Logger.create Logger.Level.Minimal



module PlatformTool =
    let platformTool tool =
        tool
        |> ProcessUtils.tryFindFileOnPath
        |> function Some t -> t | _ -> failwithf "%s not found" tool

    let dotnet command args (cwd: string) =
        let args = command :: args
        let info = ProcessStartInfo()
        info.WorkingDirectory <- cwd
        info.FileName <- platformTool "dotnet"

        for arg in args do
            info.ArgumentList.Add arg

        info.RedirectStandardOutput <- true
        use p = System.Diagnostics.Process.Start(info)

        let output =
            seq {
                while not p.StandardOutput.EndOfStream do
                    yield p.StandardOutput.ReadLine()
            }
            |> Seq.toArray

        p.WaitForExit()
        output
        |> Array.iter(fun m ->
            logger.InfoGreents "%s" m
        )



module Extensions =


    type internal Logger.Logger with
        member x.CopyFile src dest =
            File.Copy(src,dest,true)
            logger.Important "%s ->\n%s" src dest

        /// In release configuration, still copy pdb
        member x.CopyPdb _configuration src dest =
            x.CopyFile src dest

    [<RequireQualifiedAccess>]
    module SingleTargetCrackedFsproj =


        let copyFileFromRefDllToBin (configuration: Configuration) (originProjectFile: string) (destCrackedFsprojSingleTarget: SingleTargetCrackedFsproj) =

            let targetDir = destCrackedFsprojSingleTarget.TargetDir

            let originDll =
                let projName = Path.GetFileNameWithoutExtension originProjectFile

                destCrackedFsprojSingleTarget.RefDlls
                |> Array.find(fun refDll -> Path.GetFileNameWithoutExtension refDll = projName)

            let fileName = Path.GetFileName originDll

            let destDll = targetDir </> fileName

            logger.CopyFile originDll destDll

            logger.CopyPdb configuration (Path.changeExtension ".pdb" originDll) (Path.changeExtension ".pdb" destDll)

            destCrackedFsprojSingleTarget.AdditionalTargetDirs |> Seq.iter (fun targetDir ->
                let destDll = targetDir </> fileName
                logger.CopyFile originDll destDll
                logger.CopyPdb configuration (Path.changeExtension ".pdb" originDll) (Path.changeExtension ".pdb" destDll))


        let copyObjToBin configuration (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            logger.CopyFile singleTargetCrackedFsproj.ObjTargetFile singleTargetCrackedFsproj.TargetPath

            logger.CopyPdb configuration singleTargetCrackedFsproj.ObjTargetPdb singleTargetCrackedFsproj.TargetPdbPath

            singleTargetCrackedFsproj.AdditionalTargetDirs |> Seq.iter (fun targetDir ->
                logger.CopyFile singleTargetCrackedFsproj.ObjTargetFile targetDir
                logger.CopyPdb configuration singleTargetCrackedFsproj.ObjTargetPdb targetDir)


        let compile (checker: RemotableFSharpChecker) (crackedProjectSingleTarget: SingleTargetCrackedFsproj) = async {
            let tmpDll = crackedProjectSingleTarget.ObjTargetFile

            let baseOptions =
                crackedProjectSingleTarget.FSharpProjectOptions.OtherOptions
                |> Array.map (fun op -> if op.StartsWith "-o:" then "-o:" + tmpDll else op)

            let fscArgs = Array.concat [[|"fsc.exe"|]; baseOptions;[|"--nowin32manifest"|]]
            let! errors, exitCode = checker.Compile_Serializable(fscArgs)
            return
                { Errors = errors
                  ExitCode = exitCode
                  Dll = tmpDll
                  ProjPath = crackedProjectSingleTarget.ProjPath }
        }

    [<RequireQualifiedAccess>]
    module CrackedFsproj =

        let copyFileFromRefDllToBin configuration projectFile (destCrackedFsproj: CrackedFsproj) =
            destCrackedFsproj.AsList
            |> List.iter (SingleTargetCrackedFsproj.copyFileFromRefDllToBin configuration projectFile)

        let copyObjToBin configuration (crackedFsproj: CrackedFsproj) =
            crackedFsproj.AsList |> List.iter (SingleTargetCrackedFsproj.copyObjToBin configuration)


        let compile (checker) (crackedFsProj: CrackedFsproj) =
            crackedFsProj.AsList
            |> List.map (SingleTargetCrackedFsproj.compile checker)
            |> Async.Parallel

[<AutoOpen>]
module internal InternalExtensions =
    [<RequireQualifiedAccess>]
    module File =
        let rec tryFindUntilRoot makePath dir =
            let file = makePath dir
            match file with 
            | null -> None
            | _ ->
                if File.exists file 
                then Some file
                else tryFindUntilRoot makePath (Path.getDirectory dir)