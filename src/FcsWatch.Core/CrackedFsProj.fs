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
    module FSharpProjectOptions =
        let mapOtherOptions mapping (fsharpProjectOptions: FSharpProjectOptions) =
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

        member x.SourceFiles =
            x.FSharpProjectOptions.OtherOptions
            |> Array.filter(fun op -> op.EndsWith ".fs" && not <| op.EndsWith "AssemblyInfo.fs" )
            |> Array.map Path.getFullName

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


    type CrackedFsproj = private CrackedFsproj of SingleTargetCrackedFsproj list

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
        |> List.map (SingleTargetCrackedFsproj.mapProjOptions mapping)
        |> CrackedFsproj


    let mapProjOtherOptions mapping =
        mapProjOptions (fun projOptions ->
            { projOptions with
                OtherOptions = projOptions.OtherOptions |> Array.map mapping  }
        )

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
