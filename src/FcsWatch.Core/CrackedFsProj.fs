namespace FcsWatch.Core
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open FSharp.Compiler.SourceCodeServices
open System


type ICompilerOrCheckResult =
      abstract member Errors: FSharpErrorInfo []
      abstract member ExitCode: int
      abstract member ProjPath: string


module CrackedFsproj =

    let internal getSourceFilesFromOtherOptions (otherOptions: string []) =
        otherOptions
        |> Array.filter(fun op -> op.EndsWith ".fs" && not <| op.EndsWith "AssemblyInfo.fs" )

    [<RequireQualifiedAccess>]
    module private Array =
        let keepOrAdd (keeper: string -> bool) added list =
            match Array.tryFind keeper list with 
            | Some _ -> list
            | None -> Array.append [|added|] list

    let private frameworkValue (framework: string) =
        if framework.StartsWith "netcoreapp" 
        then (2,framework.Substring(10) |> Double.Parse)
        elif framework.StartsWith "netstandard"
        then (1,framework.Substring(11) |> Double.Parse)
        elif framework.StartsWith "net" 
        then 
            let frameworkNum =
                let frameworkText = framework.Substring(3)
                let main = frameworkText.[0]
                let minor = frameworkText.Substring(1)
                sprintf "%c.%s" main minor
            (0,Double.Parse frameworkNum)
        else failwithf "Unknown framework %s" framework


    [<RequireQualifiedAccess>]
    module internal FSharpProjectOptions =
        let mapOtherOption mapping (fsharpProjectOptions: FSharpProjectOptions) =
            { fsharpProjectOptions with
                OtherOptions = fsharpProjectOptions.OtherOptions |> Array.map mapping }

    [<RequireQualifiedAccess>]
    type ProjectTarget =
        | Exe
        | Library

    [<CustomComparison; CustomEquality>]
    type SingleTargetCrackedFsproj =
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

        member x.ProjDir = Path.getDirectory x.ProjPath

        member x.TargetPath = x.Props.["TargetPath"]

        member x.TargetDir = Path.getDirectory x.TargetPath

        member x.AdditionalTargetDirs =
            x.Props |> Map.toSeq
            |> Seq.choose (function
                | "RunArguments", "blazor serve" -> Some(x.TargetDir </> "dist/_framework/_bin")
                | _ -> None)

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

        member x.SourceFiles =
            x.FSharpProjectOptions.SourceFiles

        member x.RefDlls =
            x.FSharpProjectOptions.OtherOptions
            |> Array.filter(fun op ->
                op.StartsWith "-r:" && x.ProjRefs |> List.exists (fun ref -> Path.GetFileName op = Path.GetFileNameWithoutExtension ref + ".dll")
            ) |> Array.map (fun op -> op.Remove(0,3))

        override x.GetHashCode() = hash (x.ProjPath,x.ProjectTarget,x.TargetFramework)

        override x.Equals(yobj) = 
            match yobj with
            | :? SingleTargetCrackedFsproj as y -> 
                x.ProjPath = y.ProjPath
                && x.ProjectTarget = y.ProjectTarget 
                && (frameworkValue x.TargetFramework) = (frameworkValue y.TargetFramework)

            | _ -> false

        interface System.IComparable with 
            member x.CompareTo yobj =
                match yobj with 
                | :? SingleTargetCrackedFsproj as y ->
                    match x.ProjectTarget, y.ProjectTarget with 
                    | ProjectTarget.Exe, ProjectTarget.Library -> 1
                    | _ ->
                        compare (frameworkValue x.TargetFramework) (frameworkValue y.TargetFramework) 

                | _ -> invalidArg "yobj" "cannot compare values of different types"
            

    [<RequireQualifiedAccess>]
    module SingleTargetCrackedFsproj =

        let mapProjOptions mapping (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            { singleTargetCrackedFsproj with FSharpProjectOptions = mapping singleTargetCrackedFsproj.FSharpProjectOptions }

        let mapProjOtherOptions mapping (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            mapProjOptions (fun projOptions ->
                { projOptions with 
                    OtherOptions = mapping projOptions.OtherOptions }
            ) singleTargetCrackedFsproj

        let mapProjOtherOption mapping (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            mapProjOtherOptions (Array.map mapping) singleTargetCrackedFsproj

        /// https://github.com/humhei/FCSWatch/issues/30
        let private mapOtherOptionToFullPathByPrefix prefix (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            mapProjOtherOption (fun ops ->
                if ops.StartsWith prefix 
                then 
                    let path = ops.Substring(prefix.Length)
                    if Path.IsPathRooted path 
                    then ops
                    else prefix + Path.Combine(singleTargetCrackedFsproj.ProjDir, path)
                else ops
            ) singleTargetCrackedFsproj

        let mapOtherOptionToFullPath (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            singleTargetCrackedFsproj
            |> mapOtherOptionToFullPathByPrefix "--doc:"
            |> mapOtherOptionToFullPathByPrefix "-o:"
            |> mapOtherOptionToFullPathByPrefix "--out:"

        let private mapProjOptionsDebuggable (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            mapProjOtherOptions (fun ops ->
                ops
                |> Array.keepOrAdd (fun ops -> ops.StartsWith "-o:" || ops.StartsWith "--out:") ("-o:" + singleTargetCrackedFsproj.ObjTargetFile)
                |> Array.keepOrAdd (fun ops -> ops.StartsWith "--debug:" || ops.StartsWith "-g:") ("--debug:portable")
                |> Array.keepOrAdd (fun ops -> ops.StartsWith "--debug-" || ops.StartsWith "-g-") ("--debug+")
                |> Array.keepOrAdd (fun ops -> ops.StartsWith "--optimize" || ops.StartsWith "-O") ("--optimize-")
            ) singleTargetCrackedFsproj

        let private mapProjOptionsReleasable (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            mapProjOtherOptions (fun ops ->
                ops
                |> Array.keepOrAdd (fun ops -> ops.StartsWith "-o:" || ops.StartsWith "--out:") ("-o:" + singleTargetCrackedFsproj.ObjTargetFile)
                |> Array.keepOrAdd (fun ops -> ops.StartsWith "--debug+" || ops.StartsWith "-g+") ("--debug-")
                |> Array.keepOrAdd (fun ops -> ops.StartsWith "--optimize" || ops.StartsWith "-O") ("--optimize+")
            ) singleTargetCrackedFsproj


        let mapProjOptionsByConfiguration (configuration: Configuration) (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            match configuration with 
            | Configuration.Debug -> mapProjOptionsDebuggable singleTargetCrackedFsproj
            | Configuration.Release -> mapProjOptionsReleasable singleTargetCrackedFsproj


    type CrackedFsproj = internal CrackedFsproj of SingleTargetCrackedFsproj list

    with
        member x.AsList =
            let (CrackedFsproj value) = x
            value

        member x.ProjectTarget = 
            x.AsList
            |> List.tryFind (fun stcf -> stcf.ProjectTarget = ProjectTarget.Exe)
            |> function
                | Some _ -> ProjectTarget.Exe
                | None -> ProjectTarget.Library

        member x.ProjRefs = x.AsList.[0].ProjRefs

        member x.ProjPath = x.AsList.[0].ProjPath

        member x.Name = Path.GetFileNameWithoutExtension x.ProjPath

        member x.SourceFiles =
            x.AsList.[0].SourceFiles

        member x.PreferFramework =
            (List.max x.AsList).TargetFramework

        member x.PreferSingleTargetCrackedFsproj =
            x.AsList |> List.find (fun s -> s.TargetFramework = x.PreferFramework)

    [<RequireQualifiedAccess>]
    module CrackedFsproj =

        let create configuration projectFile = async {
            match! ProjectCoreCracker.getProjectOptionsFromProjectFile configuration projectFile with
            | [||] -> return failwithf "no frameworks is found in project file %s" projectFile
            | results ->
                return
                    results
                    |> Array.map (fun (projOptions, projRefs ,props) -> 
                        { FSharpProjectOptions = { projOptions with SourceFiles = getSourceFilesFromOtherOptions projOptions.OtherOptions }
                          Props = props
                          ProjPath = projectFile
                          ProjRefs = projRefs }
                        |> SingleTargetCrackedFsproj.mapOtherOptionToFullPath
                        |> SingleTargetCrackedFsproj.mapProjOptionsByConfiguration configuration
                    )
                    |> List.ofSeq
                    |> CrackedFsproj
        }

    let mapProjOptions mapping (crackedFsProj: CrackedFsproj) =
        crackedFsProj.AsList
        |> List.map (SingleTargetCrackedFsproj.mapProjOptions mapping)
        |> CrackedFsproj


    let mapProjOtherOption mapping =
        mapProjOptions (fun projOptions ->
            { projOptions with
                OtherOptions = projOptions.OtherOptions |> Array.map mapping  }
        )

    let internal mapProjOtherOptionsObjRefOnly (allCrackedFsprojs: seq<CrackedFsproj>) =
        allCrackedFsprojs
        |> Seq.map (fun info ->
            let projRefs = info.ProjRefs |> List.map (fun ref ->
                let refInfo =
                    allCrackedFsprojs
                    |> Seq.find (fun otherInfo -> otherInfo.ProjPath = ref)
                refInfo
            )

            let allRefProjInfos =
                projRefs
                |> List.collect (fun crackedFsproj ->
                    crackedFsproj.AsList
                )

            mapProjOtherOption (fun line ->
                allRefProjInfos
                |> List.tryFind (fun ref ->
                    "-r:" + ref.TargetPath = line)
                |> function
                    | Some ref -> "-r:" + ref.ObjTargetFile
                    | None -> line
            ) info
        )

