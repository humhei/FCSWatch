module FcsWatch.Core.Types
open System.IO
open FcsWatch
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Collections.Generic
open System.Xml
open FcsWatch.Core.CrackedFsproj
open System
open Fake.DotNet
open System.Threading.Tasks
open FSharp.Compiler.SourceCodeServices


[<AutoOpen>]
module internal Extensions =


    [<RequireQualifiedAccess>]
    module internal Path =
        let nomarlizeToUnixCompitiable path =
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


    [<RequireQualifiedAccess>]
    module internal Dictionary =

        let toMap dictionary =
            (dictionary :> seq<_>)
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq


[<AutoOpen>]
module internal Global =
    open Fake.Core

    let mutable logger = Logger.create (Logger.Level.Minimal)

    let private dotnetWith command args dir =
        DotNet.exec
            (fun ops -> {ops with WorkingDirectory = dir})
            command
            (Args.toWindowsCommandLine args)

    let dotnet command args dir =
        let result = dotnetWith command args dir
        if result.ExitCode <> 0
        then failwithf "Error while running %s with args %A" command (List.ofSeq args)


[<RequireQualifiedAccess>]
module FileSystem =
    let internal editDirAndFile (fileName: string) (useEditFiles) =
        assert useEditFiles
        let infoDir = Path.Combine(Path.GetDirectoryName fileName,".fsharp")
        let editFile = Path.Combine(infoDir,Path.GetFileName fileName + ".edit")
        if not (Directory.Exists infoDir) then 
            Directory.CreateDirectory infoDir |> ignore
        infoDir, editFile

    let readFile (fileName: string) (useEditFiles) = 
        if useEditFiles then 
            let infoDir, editFile = editDirAndFile fileName useEditFiles
            let preferEditFile =
                try 
                    Directory.Exists infoDir && File.Exists editFile && File.Exists fileName && File.GetLastWriteTime(editFile) > File.GetLastWriteTime(fileName)
                with _ -> 
                    false
            if preferEditFile then 
                logger.Info "*** preferring %s to %s ***" editFile fileName
                File.ReadAllText editFile
            else
                File.ReadAllText fileName
        else
            File.ReadAllText fileName


type internal Logger.Logger with
    member x.ProcessCompileOrCheckResult (errors: FSharpErrorInfo [],exitCode) =
        if exitCode = 0 then
            if not <| Array.isEmpty errors then
                logger.Warn "WARNINGS:\n%A" errors

        else logger.Error "ERRORS:\n%A" errors

[<RequireQualifiedAccess>]
module internal ICompilerOrCheckResult =
    let processCompileOrCheckResult (result: ICompilerOrCheckResult) =
        logger.ProcessCompileOrCheckResult (result.Errors,result.ExitCode)


type FullCrackedFsproj =
    { Value: CrackedFsproj
      Refs: FullCrackedFsproj list }


[<RequireQualifiedAccess>]
module FullCrackedFsproj =
    let private easyGetAllProjPaths (entryProjectFile: string) =
        let values = new HashSet<string>()
        let add projectFile = values.Add projectFile |> ignore
        let rec loop (projectFile: string) =
            let normarlizedPath = Path.nomarlizeToUnixCompitiable projectFile
            add normarlizedPath

            let dir = Path.getDirectory projectFile
            let doc = new XmlDocument()
            doc.Load(normarlizedPath)

            for node in doc.GetElementsByTagName "ProjectReference" do
                let includeAttr = node.Attributes.GetNamedItem ("Include")
                let includeValue = includeAttr.Value

                let path = dir </> includeValue

                loop path

        loop entryProjectFile
        Set.ofSeq values

   ///async works may fail due to fetch proj options is not thread safe
   /// retry many times will solve it
    let private fetchUnsafeDataAsync maxRetryCount taskInterval task prediate taskResultToTaskArgMapping allTaskArgs = async {
        let rec loop retryCount accum allTaskArgs =
            logger.Info "try fetch unsafe thread datas at %d time" retryCount

            if retryCount > maxRetryCount then failwith "exceed max retry times"

            if Seq.isEmpty allTaskArgs then accum
            else
                let allTaskResults =
                    allTaskArgs
                    |> Seq.mapi (fun i project -> async {
                        /// Set time delay to reduce the mistake times
                        do! Async.Sleep (taskInterval * i)
                        return! task project
                    }
                    )
                    |> Async.Parallel
                    |> Async.RunSynchronously

                let success,unsuccess = allTaskResults |> Array.partition prediate

                let newAccum = Array.append accum success

                loop (retryCount + 1) newAccum (unsuccess |> Array.map taskResultToTaskArgMapping)

        return loop 1 [||] allTaskArgs
    }

    let getAllCrackedFsprojs projectFile =
        let prediate (crackedFsproj: CrackedFsproj) =
            crackedFsproj.AsList
            |> List.forall (fun singleTargetCrackedFsproj ->
                singleTargetCrackedFsproj.FSharpProjectOptions.OtherOptions.Length <> 0
            )
        let allProjects = easyGetAllProjPaths projectFile

        fetchUnsafeDataAsync
            100
            50
            CrackedFsproj.create
            prediate
            (fun crackedFsproj -> crackedFsproj.ProjPath) (Array.ofSeq allProjects)


    /// entry level is 0
    let getLevel projectFile (entryFsproj: FullCrackedFsproj) =
        let rec loop level (fsproj: FullCrackedFsproj) =
            [
                if fsproj.Value.ProjPath = projectFile
                then yield! [level]
                yield! fsproj.Refs |> List.collect (loop (level + 1))
            ]

        loop 0 entryFsproj
        |> List.max


    let getProjectRefersMap (fsproj: FullCrackedFsproj) =
        let cacheMutable = new Dictionary<string, CrackedFsproj list>()
        let rec loop (stack: CrackedFsproj list) (fsproj: FullCrackedFsproj) =
            match cacheMutable.TryGetValue fsproj.Value.ProjPath with
            | true,stack2 ->
                let newStack = stack @ stack2
                cacheMutable.[fsproj.Value.ProjPath] <- newStack
            | false,_ ->
                cacheMutable.Add(fsproj.Value.ProjPath, stack)

                let newStack = fsproj.Value :: stack

                fsproj.Refs |> List.iter (loop newStack)

        loop [] fsproj
        cacheMutable
        |> Dictionary.toMap
        |> Map.map (fun proj crackedFsprojs ->
            crackedFsprojs
            |> List.distinctBy (fun crackedFsproj -> crackedFsproj.ProjPath)
        )


    let create projectFile = async {
        logger.Infots "Begin crack project"
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

        logger.Infots "End crack project"



        return (project, projectMap)
    }


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
        /// projectFile, project refers
        ProjRefersMap: Map<string, CrackedFsproj list>

        ProjectMap: Map<string ,CrackedFsproj>

        /// sourceFile,projectFiles
        SourceFileMap: Map<string, string list>

        EditSourceFileMap: Map<string, string list>

        /// entry project file level is 0
        ProjLevelMap: Map<string, int>

        EntryProjectFile: string

    }
with
    member x.AllProjectFiles = x.ProjectMap |> Seq.map (fun pair -> pair.Key)

    member x.EntryCrackedFsproj = x.ProjectMap.[x.EntryProjectFile]




[<RequireQualifiedAccess>]
module CrackedFsprojBundleCache =
    let create useEditFiles fsproj (projectMap: Map<string, CrackedFsproj>) =
        let sourceFileMap = ProjectMap.sourceFileMap projectMap

        let editSourceFileMap =
            if useEditFiles then 
                sourceFileMap |> Seq.map (fun pair ->
                    let sourceFile = pair.Key
                    let (_, file) = FileSystem.editDirAndFile sourceFile useEditFiles
                    (file, pair.Value)
                )
                |> Map.ofSeq
            else sourceFileMap

        { ProjectMap = projectMap
          SourceFileMap = sourceFileMap
          ProjRefersMap = FullCrackedFsproj.getProjectRefersMap fsproj
          ProjLevelMap = ProjectMap.getProjectLevelMap projectMap fsproj
          EditSourceFileMap = editSourceFileMap
          EntryProjectFile = fsproj.Value.ProjPath }

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

let crackedFsprojBundle useEditFiles (projectFile: string) = MailboxProcessor<CrackedFsprojBundleMsg>.Start(fun inbox ->
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
    loop project (CrackedFsprojBundleCache.create useEditFiles project cache)
)


[<RequireQualifiedAccess>]
type WhyCompile =
    | WarmCompile
    | DetectFileChange

type CompilerTask<'Result when 'Result :> ICompilerOrCheckResult> = CompilerTask of why:WhyCompile * startTime: DateTime * task: Task<'Result list>
with 
    member x.Why = 
        match x with 
        | CompilerTask(why = m) -> m

    member x.StartTime = 
        match x with 
        | CompilerTask(startTime = m) -> m

    member x.Task = 
        match x with 
        | CompilerTask(task = m) -> m

