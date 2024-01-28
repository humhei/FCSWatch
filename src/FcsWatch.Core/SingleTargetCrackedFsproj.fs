namespace FcsWatch.Core
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open FSharp.Compiler.CodeAnalysis
open System
open Ionide.ProjInfo.Types
open System.Xml.Linq
open FSharp.Compiler.Diagnostics





type ICompilerOrCheckResult =
      abstract member Errors: SerializableFSharpDiagnostic []
      abstract member ExitCode: int
      abstract member ProjPath: string



[<AutoOpen>]
module _SingleTargetCrackedFsProj =
    type AdditionalProjInfoConfig =
        {
            IncludeGlobs : string []
            ExcludeGlobs : string []
        } with
        static member Empty =
            { IncludeGlobs = [|  |]; ExcludeGlobs = [|  |] }


    let private parseAdditionalWatchGlobs (file: string) =
        let document = XDocument.Load(file)
        document.Descendants(XName.op_Implicit("Watch")) |> Seq.map (fun node -> {
                    IncludeGlobs =
                        node.Attributes(XName.op_Implicit("Include"))
                            |> Seq.map (fun a -> a.Value.Split[| ';' |])
                            |> Seq.fold (fun a b -> Array.concat [ a; b ]) [|  |]
                    ExcludeGlobs =
                        node.Attributes(XName.op_Implicit("Exclude"))
                            |> Seq.map (fun a -> a.Value.Split[| ';' |])
                            |> Seq.fold (fun a b -> Array.concat [ a ; b ]) [|  |]
                }) |> Seq.fold (fun a b ->
                {
                    IncludeGlobs = Array.concat [ a.IncludeGlobs; b.IncludeGlobs ]
                    ExcludeGlobs = Array.concat [ a.ExcludeGlobs; b.ExcludeGlobs ]
                }) AdditionalProjInfoConfig.Empty


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
          TargetPath: string
          TargetFramework: string
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

        member x.TargetDir = Path.getDirectory x.TargetPath

        member x.AdditionalTargetDirs =
            x.Props |> Map.toSeq
            |> Seq.choose (function
                | "RunArguments", "blazor serve" -> Some(x.TargetDir </> "dist/_framework/_bin")
                | _ -> None)


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
    module private Array =
        let keepOrAdd (keeper: string -> bool) added list =
            match Array.tryFind keeper list with 
            | Some _ -> list
            | None -> Array.append [|added|] list



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
        let private mapOtherOptionToFullPathByPrefix (prefix: string) (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            mapProjOtherOption (fun ops ->
                if ops.StartsWith prefix 
                then 
                    let path = ops.Substring(prefix.Length)
                    if Path.IsPathRooted path 
                    then ops
                    else prefix + Path.Combine(singleTargetCrackedFsproj.ProjDir, path)
                else ops
            ) singleTargetCrackedFsproj

        let private mapOtherOptionToFullPath (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
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


        let private mapProjOptionsByConfiguration (configuration: Configuration) (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            match configuration with 
            | Configuration.Debug -> mapProjOptionsDebuggable singleTargetCrackedFsproj
            | Configuration.Release -> mapProjOptionsReleasable singleTargetCrackedFsproj


        let create configuration otherFlags (currentProjOptions: ProjectOptions) allProjectOptions =
            let file = currentProjOptions.ProjectFileName
            let cwd =
                System.IO.Path.GetDirectoryName file
                |> System.IO.DirectoryInfo

            let projectAssetsJsonPath = Path.Combine(cwd.FullName, "obj", "project.assets.json")
            if not(File.Exists(projectAssetsJsonPath)) then
               failwithf "Cannot find restored info for project %s" file

            let additionalConfig = parseAdditionalWatchGlobs file

            let fsharpProjOptions = 
                let fsharpOptions =  
                    Ionide.ProjInfo.FCS.mapToFSharpProjectOptions currentProjOptions allProjectOptions

                { fsharpOptions with 
                    OtherOptions = Array.concat [|fsharpOptions.OtherOptions; fsharpOptions.SourceFiles; otherFlags|] 
                }

            let projRefs = 
              currentProjOptions.ReferencedProjects
              |> List.map(fun m -> m.ProjectFileName)

            let props = 
              currentProjOptions.CustomProperties
              |> List.map(fun m -> m.Name, m.Value)
              |> Map.ofList


            {
                TargetPath = currentProjOptions.TargetPath
                TargetFramework = currentProjOptions.TargetFramework
                ProjRefs = projRefs
                Props = props
                FSharpProjectOptions = fsharpProjOptions
                ProjPath = fsharpProjOptions.ProjectFileName
            }
            |> mapOtherOptionToFullPath
            |> mapProjOptionsByConfiguration configuration