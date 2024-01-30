namespace FcsWatch.Core
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open Fake.IO.Globbing.Operators
open ProjectCoreCracker
open System
open System.Text.RegularExpressions
open Ionide.ProjInfo
open Ionide.ProjInfo.Types



module CrackedFsproj =
    let internal createRegexFromGlob glob =
        Regex.Escape(glob).Replace(@"\*", ".*").Replace(@"\?", ".")


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

        let create configuration otherFlags (currentProjOptions: ProjectOptions) allProjOptions =
            SingleTargetCrackedFsproj.create configuration otherFlags currentProjOptions allProjOptions
            |> List.singleton
            |> CrackedFsproj

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
                    |> Seq.tryFind (fun otherInfo -> otherInfo.ProjPath = ref)
                    |> function
                        | None -> 
                            let  allCrackedFsprojPaths = allCrackedFsprojs |> List.ofSeq |> List.map(fun m -> m.ProjPath)
                            failwithf "Cannot find %s in %A" ref allCrackedFsprojPaths

                        | Some crackedFsProj -> crackedFsProj
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

