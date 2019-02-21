module FcsWatch.Types 
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Collections.Generic
open System.Xml
open FcsWatch.CrackedFsproj

[<RequireQualifiedAccess>]
module Dictionary =

    let toMap dictionary = 
        (dictionary :> seq<_>)
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq


[<AutoOpen>]
module internal Global =
    let mutable logger = Logger.create (Logger.Level.Minimal)

type Logger.Logger with 
    member x.CopyFile src dest =
        File.Copy(src,dest,true)
        logger.Info "%s ->\n%s" src dest

    member x.ProcessCompileResult (errors,exitCode) =
        if exitCode = 0 then 
            if not <| Array.isEmpty errors then 
                logger.Warn "WARNINGS:\n%A" errors

        else logger.Error "ERRORS:\n%A" errors


module internal CompilerResult =
    let processCompileResult compilerResult =
        logger.ProcessCompileResult (compilerResult.Errors,compilerResult.ExitCode)

[<RequireQualifiedAccess>]
module CrackedFsprojSingleTarget =

    let copyFileFromRefDllToBin originProjectFile (destCrackedFsprojSingleTarget: CrackedFsprojSingleTarget) =
        
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

[<RequireQualifiedAccess>]
module CrackedFsproj =
    let copyFileFromRefDllToBin projectFile (destCrackedFsproj: CrackedFsproj) =
        destCrackedFsproj.AsList
        |> List.iter (CrackedFsprojSingleTarget.copyFileFromRefDllToBin projectFile)

type Plugin =
    { Load: unit -> unit 
      Unload: unit -> unit 
      Calculate: unit -> unit 
      DebuggerAttachTimeDelay: int }

[<RequireQualifiedAccess>]
type DevelopmentTarget =
    | Program 
    | Plugin of Plugin

type Config =
    { LoggerLevel: Logger.Level
      WorkingDir: string
      DevelopmentTarget: DevelopmentTarget }
    



type FullCrackedFsproj =
    { Value: CrackedFsproj
      Refs: FullCrackedFsproj list }


[<RequireQualifiedAccess>]
module FullCrackedFsproj =
    let private easyGetAllProjPaths (entryProjectFile: string) =
        let values = new HashSet<string>()
        let add projectFile = values.Add projectFile |> ignore
        let rec loop (projectFile: string) = 
            add projectFile

            let dir = Path.getDirectory projectFile
            let doc = new XmlDocument()
            doc.Load(projectFile)

            for node in doc.GetElementsByTagName "ProjectReference" do
                let includeAttr = node.Attributes.GetNamedItem ("Include")
                let includeValue = includeAttr.Value
                let path = Path.getFullName (dir </> includeValue)
                loop path

        loop entryProjectFile 
        Set.ofSeq values       

    let private getAllCrackedFsprojs projectFile =
        let allProjects = easyGetAllProjPaths projectFile
        allProjects
        |> Seq.map CrackedFsproj.create
        |> Async.Parallel

    /// entry level is 0
    let internal getLevel projectFile (entryFsproj: FullCrackedFsproj) =
        let rec loop level (fsproj: FullCrackedFsproj) = 
            [
                if fsproj.Value.ProjPath = projectFile 
                then yield! [level]
                yield! fsproj.Refs |> List.collect (loop (level + 1)) 
            ]

        loop 0 entryFsproj   
        |> List.max


    let internal getProjectRefersMap (fsproj: FullCrackedFsproj) =
        let cacheMutable = new Dictionary<string, CrackedFsproj list>()
        let rec loop (stack: CrackedFsproj list) (fsproj: FullCrackedFsproj) = 
            match cacheMutable.TryGetValue fsproj.Value.ProjPath with 
            | true,stack2 -> 
                let newStack = stack @ stack2 
                cacheMutable.[fsproj.Value.ProjPath] <- newStack
            | false,_ ->
                let newStack = fsproj.Value :: stack
                cacheMutable.Add(fsproj.Value.ProjPath,newStack)
                fsproj.Refs |> List.iter (loop newStack)

        loop [] fsproj
        cacheMutable 
        |> Dictionary.toMap
        |> Map.map (fun proj crackedFsprojs ->
            crackedFsprojs
            |> List.distinctBy (fun crackedFsproj -> crackedFsproj.ProjPath)
        )


    let create projectFile = async {

        let projectMapsMutable = Dictionary<string,CrackedFsproj>()

        let allCrackedFsprojsObjRefOnly = 
            let allCrackedFsprojs = getAllCrackedFsprojs projectFile |> Async.RunSynchronously
            CrackedFsproj.mapProjOtherOptionsObjRefOnly allCrackedFsprojs

        allCrackedFsprojsObjRefOnly |> Seq.iter (fun crakedFsproj -> projectMapsMutable.Add (crakedFsproj.ProjPath,crakedFsproj))

        let rec loop projectFile = 
            let projectInfo = projectMapsMutable.[projectFile]         
            { Value = projectInfo
              Refs = projectInfo.ProjRefs |> List.map loop }
    
        let project = loop projectFile

        let projectMap = 
            projectMapsMutable 
            |> Dictionary.toMap 




        return (project, projectMap)
    }
    

[<RequireQualifiedAccess>]
module private ProjectMap =
    let sourceFileMap (projectMap: Map<string,CrackedFsproj>) = 
        let dict = new Dictionary<string, string>()
        projectMap |> Seq.iter (fun pair -> 
            pair.Value.SourceFiles |> Seq.iter (fun sourceFile ->
                dict.Add(sourceFile,pair.Key)
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
        /// projectFile, project refers
        ProjRefersMap: Map<string, CrackedFsproj list>
        
        ProjectMap: Map<string ,CrackedFsproj>

        /// sourceFile,projectFile
        SourceFileMap: Map<string, string>

        /// entry project file level is 0
        ProjLevelMap: Map<string, int>
    }
with 
    member x.AllProjectFiles = x.ProjectMap |> Seq.map (fun pair -> pair.Key)


[<RequireQualifiedAccess>]
module CrackedFsprojBundleCache =
    let create fsproj (projectMap: Map<string, CrackedFsproj>) =
        { ProjectMap = projectMap
          SourceFileMap = ProjectMap.sourceFileMap projectMap
          ProjRefersMap = FullCrackedFsproj.getProjectRefersMap fsproj
          ProjLevelMap = ProjectMap.getProjectLevelMap projectMap fsproj }

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

let crackedFsprojBundle (projectFile: string) = MailboxProcessor<CrackedFsprojBundleMsg>.Start(fun inbox ->
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

    let (project, cache) = FullCrackedFsproj.create projectFile |> Async.RunSynchronously
    loop project (CrackedFsprojBundleCache.create project cache)
)
