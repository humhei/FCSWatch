namespace FsLive.Core
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Collections.Generic
open System.Xml
open FsLive.Core.CrackedFsproj
open FSharp.Compiler.SourceCodeServices
open FullCrackedFsproj

module CrackedFsprojBundle =
    [<RequireQualifiedAccess>]
    module private ProjectMap =
        let sourceFileMap (projectMap: Map<string,CrackedFsproj>) =
            let dict = new Dictionary<string, string list>()
            projectMap |> Seq.iter (fun pair ->
                pair.Value.SourceFiles |> Seq.iter (fun sourceFile ->
                    match dict.TryGetValue sourceFile with 
                    | true, projPaths ->
                        dict.[sourceFile] <- pair.Key :: projPaths
                    | false, _ -> dict.Add(sourceFile,[pair.Key])
                )
            )
            Dictionary.toMap dict

        let getProjectLevelMap (projectMap: Map<string,CrackedFsproj>) (entryFsproj: FullCrackedFsproj) =
            let projects = projectMap |> Seq.map (fun pair -> pair.Key)
            projects |> Seq.map (fun proj ->
                (proj, FullCrackedFsproj.getLevel proj entryFsproj)
            )
            |> Map.ofSeq


    type CrackedFsprojBundleCache =
        {
            /// projectFile, project refers (ALL)
            ProjRefersMap: Map<string, CrackedFsproj list>

            ProjectMap: Map<string ,CrackedFsproj>

            /// sourceFile,projectFiles
            SourceFileMap: Map<string, string list>

            /// entry project file level is 0
            ProjLevelMap: Map<string, int>

            EntryProjectFile: string
        }
    with
        member x.AllProjectFiles = x.ProjectMap |> Seq.map (fun pair -> pair.Key)


    [<RequireQualifiedAccess>]
    module CrackedFsprojBundleCache =
        let create entryProjectFile fsproj (projectMap: Map<string, CrackedFsproj>) =
            { ProjectMap = projectMap
              SourceFileMap = ProjectMap.sourceFileMap projectMap
              ProjRefersMap = FullCrackedFsproj.getProjectRefersMap fsproj
              ProjLevelMap = ProjectMap.getProjectLevelMap projectMap fsproj
              EntryProjectFile = entryProjectFile }

        let update projPaths (cache: CrackedFsprojBundleCache) = async {

            let addOrUpdate (projectMap: Map<string, CrackedFsproj>) =
                projPaths
                |> List.map CrackedFsproj.create
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.fold (fun projectMap crackedFsproj ->
                    Map.add crackedFsproj.ProjPath crackedFsproj projectMap
                ) projectMap

            let objRefOnly (projectMap: Map<string, CrackedFsproj>) =
                let projPaths = projectMap |> Seq.map (fun pair -> pair.Key)
                let crackedFsProjs = projectMap |> Seq.map (fun pair -> pair.Value)
                Seq.zip projPaths (CrackedFsproj.mapProjOtherOptionsObjRefOnly crackedFsProjs)
                |> Map.ofSeq

            let newProjectMap =
                cache.ProjectMap |> addOrUpdate |> objRefOnly

            let newSourceFileMap = ProjectMap.sourceFileMap newProjectMap

            return
                { cache with
                    ProjectMap = newProjectMap
                    SourceFileMap = newSourceFileMap }
        }

        let sortProjPathsDescendByLevel (projPaths: seq<string>) (cache: CrackedFsprojBundleCache) =
            projPaths
            |> Seq.sortByDescending (fun projectFile -> cache.ProjLevelMap.[projectFile])

    [<RequireQualifiedAccess>]
    type CrackedFsprojBundleMsg =
        | GetCache of replyChannel: AsyncReplyChannel<CrackedFsprojBundleCache>
        | DetectProjectFileChanges of FileChange list * AsyncReplyChannel<CrackedFsprojBundleCache>

    let crackedFsprojBundle checker (config: Config) (projectFile: string option) = MailboxProcessor<CrackedFsprojBundleMsg>.Start(fun inbox ->
        let rec loop (entry: FullCrackedFsproj) cache = async {
            let! msg = inbox.Receive()
            match msg with
            | CrackedFsprojBundleMsg.GetCache replyChannel ->
                replyChannel.Reply cache
                return! loop entry cache

            | CrackedFsprojBundleMsg.DetectProjectFileChanges (fileChanges, replyChannel) ->
                let projectFiles = fileChanges |> List.map (fun fileChange -> fileChange.FullPath)
                let! newCache = CrackedFsprojBundleCache.update projectFiles cache
                replyChannel.Reply newCache
                return! loop entry newCache
        }

        let (project, cache) = FullCrackedFsproj.create checker config projectFile |> Async.RunSynchronously
        loop project (CrackedFsprojBundleCache.create project.Value.AsList.[0].ProjPath project cache)
    )
