namespace FsLive.Core
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open FSharp.Compiler.SourceCodeServices

[<RequireQualifiedAccess>]
module internal Path =
    let nomarlizeToUnixCompatible path =
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

module CrackedFsproj =

    [<RequireQualifiedAccess>]
    module internal Array =
        let keepOrAdd (keeper: string -> bool) added list =
            match Array.tryFind keeper list with 
            | Some _ -> list
            | None -> Array.append [|added|] list

    [<RequireQualifiedAccess>]
    module FSharpProjectOptions =
        let mapOtherOptions mapping (fsharpProjectOptions: FSharpProjectOptions) =
            { fsharpProjectOptions with
                OtherOptions = fsharpProjectOptions.OtherOptions |> Array.map mapping }



    [<RequireQualifiedAccess>]
    type ProjectTarget =
        | Exe
        | Library

    type CompileResult =
        { Dll: string
          Errors: FSharpErrorInfo []
          ExitCode: int
          ProjPath: string }
    with
        member x.Pdb = Path.changeExtension ".pdb" x.Dll

    type CheckResult =
        { Errors: FSharpErrorInfo [] 
          Result: Result<FSharpImplementationFileContents list ,unit>
          ProjPath: string 
          /// used for eval context
          ProjOptions: FSharpProjectOptions }

    [<RequireQualifiedAccess>]
    type CompileOrCheckResult =
        | CompileResult of CompileResult
        | CheckResult of CheckResult

    with 
        member x.ProjPath = 
            match x with 
            | CompileOrCheckResult.CheckResult checkResult -> checkResult.ProjPath
            | CompileOrCheckResult.CompileResult compileResult -> compileResult.ProjPath

        member x.Errors = 
            match x with 
            | CompileOrCheckResult.CheckResult checkResult -> checkResult.Errors
            | CompileOrCheckResult.CompileResult compileResult -> compileResult.Errors

    [<RequireQualifiedAccess>]
    module CompileOrCheckResult =

        let isSuccess = function
            | CompileOrCheckResult.CheckResult checkResult ->
                match checkResult.Result with 
                | Result.Ok _ -> true
                | Result.Error _ -> false
            | CompileOrCheckResult.CompileResult compileResult ->
                compileResult.ExitCode = 0

        let isFail = isSuccess >> not

        let errors (compileOrCheckResult: CompileOrCheckResult) = compileOrCheckResult.Errors

    type SingleTargetCrackedFsproj =
        { FSharpProjectOptions: FSharpProjectOptions
          ProjRefs: string list
          ProjPath: string 
          Props: Map<string,string> }

    with

        member x.ProjectTarget =
            x.FSharpProjectOptions.OtherOptions |> Array.find (fun op ->
                op.StartsWith "--target"
            )
            |> function
                | "--target:exe" -> ProjectTarget.Exe
                | "--target:library" -> ProjectTarget.Library
                | others -> failwithf "unknown target compile option %s" others

        member x.TargetPath = x.Props.["TargetPath"]

        member x.TargetDir = Path.getDirectory x.TargetPath

        member x.TargetFramework = x.Props.["TargetFramework"]

        member x.TargetPdbPath = Path.changeExtension ".pdb" x.TargetPath

        member x.TargetFileName = Path.GetFileName(x.TargetPath)

        member x.TargetPdbName = Path.changeExtension ".pdb" x.TargetFileName

        member x.ObjTargetFile =
            let projDir = Path.getDirectory x.ProjPath
            let targetPath = x.TargetPath
            let relative = targetPath.Replace(projDir,"").TrimStart([|'/';'\\'|])
            let objRelative =
                if relative.StartsWith "bin" || relative.StartsWith "bin" then  "obj" + relative.Substring 3
                else failwithf "is not a valid bin relativePath %s" relative
            projDir </> objRelative

        member x.ObjTargetPdb = Path.changeExtension ".pdb" x.ObjTargetFile

        member x.RefDlls =
            x.FSharpProjectOptions.OtherOptions
            |> Array.filter(fun op ->
                op.StartsWith "-r:" && x.ProjRefs |> List.exists (fun ref -> Path.GetFileName op = Path.GetFileNameWithoutExtension ref + ".dll")
            ) |> Array.map (fun op -> op.Remove(0,3))

        member x.SourceFiles = x.FSharpProjectOptions.SourceFiles



    [<RequireQualifiedAccess>]
    module SingleTargetCrackedFsproj =

        let create projPath (projOptions: FSharpProjectOptions, projRefs: string list, props: Map<string,string>): SingleTargetCrackedFsproj = 
            { FSharpProjectOptions = 
                { projOptions with 
                    SourceFiles = 
                        projOptions.OtherOptions
                        |> Array.filter(fun op -> (op.EndsWith ".fs" || op.EndsWith ".fsx" || op.EndsWith ".fsi") && not <| op.EndsWith "AssemblyInfo.fs" )
                    OtherOptions = 
                        projOptions.OtherOptions
                        |> Array.map (fun op -> 
                            if op.StartsWith "-o:" 
                            then 
                                let dir = Path.getDirectory projPath
                                "-o:" + Path.nomarlizeToUnixCompatible(dir </> op.Substring 3)
                            else op
                        )
                }
              Props = props 
              ProjPath = projPath
              ProjRefs = projRefs }

        let compile (checker: FSharpChecker) (crackedProjectSingleTarget: SingleTargetCrackedFsproj) = async {
            let tmpDll = crackedProjectSingleTarget.ObjTargetFile

            let fscArgs =
                crackedProjectSingleTarget.FSharpProjectOptions.OtherOptions
                |> Array.keepOrAdd (fun op -> op.StartsWith "-o:" || op.StartsWith "--out:") ("-o:" + tmpDll)
                |> Array.keepOrAdd 
                    (fun op -> 
                        op.StartsWith "--win32manifest:" 
                        || op.StartsWith "--win32res"
                        || op = "--nowin32manifest") "--nowin32manifest"
                |> function
                    | options when not (options.[0].EndsWith ".exe") ->
                        Array.append [|"fsc.exe"|] options
                    | options -> options

            let! errors,exitCode = checker.Compile(fscArgs)
            return
                { Errors = errors
                  ExitCode = exitCode
                  Dll = tmpDll
                  ProjPath = crackedProjectSingleTarget.ProjPath }
        }

        let mapProjOptions mapping (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            { singleTargetCrackedFsproj with FSharpProjectOptions = mapping singleTargetCrackedFsproj.FSharpProjectOptions }

        let mapProjOtherOptions mapping (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            mapProjOptions (fun projOptions ->
                { projOptions with 
                    OtherOptions = mapping projOptions.OtherOptions }
            ) singleTargetCrackedFsproj

        let mapProjOptionsDebuggable (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            mapProjOtherOptions (fun ops ->
                ops
                |> Array.keepOrAdd (fun ops -> ops.StartsWith "-o:" || ops.StartsWith "--out:") ("-o:" + singleTargetCrackedFsproj.ObjTargetFile)
                |> Array.keepOrAdd (fun ops -> ops.StartsWith "--target:") ("--target:library")
                |> Array.keepOrAdd (fun ops -> ops.StartsWith "--debug" || ops.StartsWith "-g") ("--debug:portable")
                |> Array.keepOrAdd (fun ops -> ops.StartsWith "--optimize" || ops.StartsWith "-O") ("--optimize-")
            ) singleTargetCrackedFsproj


    type CrackedFsproj = CrackedFsproj of SingleTargetCrackedFsproj list

    with
        member x.AsList =
            let (CrackedFsproj value) = x
            value

        member x.ProjectTarget = x.AsList.[0].ProjectTarget

        member x.ProjRefs = x.AsList.[0].ProjRefs

        member x.ProjPath = x.AsList.[0].ProjPath

        member x.Name = Path.GetFileNameWithoutExtension x.ProjPath

        member x.SourceFiles =
            x.AsList.[0].SourceFiles

    [<RequireQualifiedAccess>]
    module CrackedFsproj =

        let create projectFile = async {

            match! ProjectCoreCracker.getProjectOptionsFromProjectFile projectFile with

            | [||] -> return failwithf "no frameworks is found in project file %s" projectFile
            | results ->
                return
                    results
                    |> Array.map (SingleTargetCrackedFsproj.create projectFile)
                    |> List.ofSeq
                    |> CrackedFsproj
        }

        let mapProjOptions mapping (crackedFsProj: CrackedFsproj) =
            crackedFsProj.AsList
            |> List.map (SingleTargetCrackedFsproj.mapProjOptions mapping)
            |> CrackedFsproj


        let mapProjOtherOptions mapping =
            mapProjOptions (fun projOptions ->
                { projOptions with
                    OtherOptions = projOptions.OtherOptions |> Array.map mapping  }
            )

        let compile (checker: FSharpChecker) (crackedFsProj: CrackedFsproj) =
            crackedFsProj.AsList
            |> List.map (SingleTargetCrackedFsproj.compile checker)
            |> Async.Parallel


        let mapProjOtherOptionsObjRefOnly (crackedFsprojs: seq<CrackedFsproj>) =
            crackedFsprojs
            |> Seq.map (fun info ->
                let projRefs = info.ProjRefs |> List.map (fun ref ->
                    let refInfo =
                        crackedFsprojs
                        |> Seq.find (fun otherInfo -> otherInfo.ProjPath = ref)
                    refInfo
                )

                let allRefProjInfos =
                    projRefs
                    |> List.collect (fun crackedFsproj ->
                        crackedFsproj.AsList
                    )

                mapProjOtherOptions (fun line ->
                    allRefProjInfos
                    |> List.tryFind (fun ref ->
                        "-r:" + ref.TargetPath = line)
                    |> function
                        | Some ref -> "-r:" + ref.ObjTargetFile
                        | None -> line
                ) info
            )


        let mapProjOtherOptionsDebuggable (CrackedFsproj singleTargetCrackedFsprojs) =
            singleTargetCrackedFsprojs
            |> List.map SingleTargetCrackedFsproj.mapProjOptionsDebuggable 
            |> CrackedFsproj
