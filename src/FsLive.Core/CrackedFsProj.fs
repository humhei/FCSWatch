namespace FsLive.Core
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open Microsoft.FSharp.Compiler.SourceCodeServices

module CrackedFsproj =
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
          Result: Result<FSharpImplementationFileContents option ,FSharpImplementationFileContents option>
          ProjPath: string }

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

    type CrackedFsprojSingleTarget =
        { FSharpProjectOptions: FSharpProjectOptions
          ProjRefs: string list
          Props: Map<string,string>
          ProjPath: string }

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
            let relative = Path.toRelativeFrom projDir x.TargetPath
            let objRelative =
                if relative.StartsWith ".\\bin" || relative.StartsWith "./bin" then  "obj" + relative.Substring 5
                else failwithf "is not a valid bin relativePath %s" relative
            projDir </> objRelative

        member x.ObjTargetPdb = Path.changeExtension ".pdb" x.ObjTargetFile

        member x.RefDlls =
            x.FSharpProjectOptions.OtherOptions
            |> Array.filter(fun op ->
                op.StartsWith "-r:" && x.ProjRefs |> List.exists (fun ref -> Path.GetFileName op = Path.GetFileNameWithoutExtension ref + ".dll")
            ) |> Array.map (fun op -> op.Remove(0,3))


    [<RequireQualifiedAccess>]
    module CrackedFsprojSingleTarget =

        let compile (checker: FSharpChecker) (crackedProjectSingleTarget: CrackedFsprojSingleTarget) = async {
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

        let mapProjOptions mapping (crackedFsprojSingleTarget: CrackedFsprojSingleTarget) =
            { crackedFsprojSingleTarget with FSharpProjectOptions = mapping crackedFsprojSingleTarget.FSharpProjectOptions }




    type CrackedFsproj = private CrackedFsproj of CrackedFsprojSingleTarget list

    with
        member x.AsList =
            let (CrackedFsproj value) = x
            value

        member x.ProjectTarget = x.AsList.[0].ProjectTarget

        member x.ProjRefs = x.AsList.[0].ProjRefs

        member x.ProjPath = x.AsList.[0].ProjPath

        member x.Name = Path.GetFileNameWithoutExtension x.ProjPath

        member x.SourceFiles =
            x.AsList.[0].FSharpProjectOptions.OtherOptions
            |> Array.filter(fun op -> op.EndsWith ".fs" && not <| op.EndsWith "AssemblyInfo.fs" )
            |> Array.map Path.getFullName

    [<RequireQualifiedAccess>]
    module CrackedFsproj =

        let create projectFile = async {
            match! ProjectCoreCracker.getProjectOptionsFromProjectFile projectFile with
            | [|projOptions,projRefs,props|] ->
                return
                    { FSharpProjectOptions = projOptions
                      Props = props; ProjPath = projectFile
                      ProjRefs = projRefs }
                    |> List.singleton
                    |> CrackedFsproj

            | [||] -> return failwithf "no frameworks is found in project file %s" projectFile
            | results ->
                return
                    results
                    |> Array.map (fun (projOptions, projRefs ,props) -> { FSharpProjectOptions = projOptions; Props = props; ProjPath = projectFile; ProjRefs = projRefs })
                    |> List.ofSeq
                    |> CrackedFsproj
        }

        let mapProjOptions mapping (crackedFsProj: CrackedFsproj) =
            crackedFsProj.AsList
            |> List.map (CrackedFsprojSingleTarget.mapProjOptions mapping)
            |> CrackedFsproj


        let mapProjOtherOptions mapping =
            mapProjOptions (fun projOptions ->
                { projOptions with
                    OtherOptions = projOptions.OtherOptions |> Array.map mapping  }
            )

        let compile (checker: FSharpChecker) (crackedFsProj: CrackedFsproj) =
            crackedFsProj.AsList
            |> List.map (CrackedFsprojSingleTarget.compile checker)
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


