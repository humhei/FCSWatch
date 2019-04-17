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
open Fake.IO.Globbing.Operators

type IMailboxProcessor<'Msg> =
    abstract member PostAndReply: buildMsg: (AsyncReplyChannel<'Reply> -> 'Msg) -> 'Reply
    abstract member PostAndAsyncReply: buildMsg: (AsyncReplyChannel<'Reply> -> 'Msg) -> Async<'Reply>

[<AutoOpen>]
module internal Extensions =


    [<RequireQualifiedAccess>]
    module internal Path =
        let nomalizeToUnixCompatiable path =
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


    [<RequireQualifiedAccess>]
    module internal MailboxProcessor =
        let toChildInterface (mapping: 'ChildMsg -> 'Msg) (agent: MailboxProcessor<'Msg>) =
            { new IMailboxProcessor<'ChildMsg> with 
                member x.PostAndReply buildMsg =
                    agent.PostAndReply (fun replyChannel -> mapping (buildMsg replyChannel))

                member x.PostAndAsyncReply buildMsg =
                    agent.PostAndAsyncReply(fun replyChannel -> mapping (buildMsg replyChannel))
            }


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




type NoFrameworkCrackedFsprojBuilder =
    { OtherFlags: string [] 
      UseEditFiles: bool
      Checker: FSharpChecker }

type ScriptCrackedFsprojBuilder =
    { OtherFlags: string [] 
      File: string 
      Checker: FSharpChecker }

type ProjectCrackedFsprojBuilder =
    { OtherFlags: string [] 
      File: string }

[<RequireQualifiedAccess>]
type FullCrackedFsprojBuilder =
    | Script of ScriptCrackedFsprojBuilder
    | Project of ProjectCrackedFsprojBuilder
    | FSharpArgs of NoFrameworkCrackedFsprojBuilder



[<RequireQualifiedAccess>]
module FullCrackedFsprojBuilder =

    /// <param name="entryFileOp">file end with *.fs;*.fsproj;*.fsx;*.fsi; If None then otherflags will be applied</param>
    let create workingDir useEditFiles checker (entryFileOp: string option) (otherFlags: string []) =
        let workingDir = Path.nomalizeToUnixCompatiable workingDir

        let entryFileOp = 
            match entryFileOp with 
            | Some file -> Some (Path.nomalizeToUnixCompatiable file)
            | None ->
                !! (workingDir </> "*.fsproj")
                |> List.ofSeq
                |> function 
                    | [ ] ->  None
                    | [ file ] -> 
                        logger.Important "using implicit project file '%s'" file
                        Some file
                    | file1 :: file2 :: _ -> 
                        failwithf "multiple project files found, e.g. %s and %s" file1 file2 

        match entryFileOp with 
        | Some file ->
            match file with 
            | projectFile when projectFile.EndsWith ".fsproj" ->
                { OtherFlags = otherFlags
                  File = projectFile }
                |> FullCrackedFsprojBuilder.Project

            | scriptFile when file.EndsWith ".fs" || file.EndsWith ".fsx" || file.EndsWith ".fsi" ->
                { OtherFlags = otherFlags
                  File = scriptFile
                  Checker = checker }
                |> FullCrackedFsprojBuilder.Script

            | _ -> failwithf "entry file %s should end with .fsproj;.fs;.fsx;.fsi;" file
        | None ->
            match otherFlags with 
            | [||] -> failwithf "no project file found, no compilation arguments given and no project file found in \"%s\"" workingDir 
            | _ ->
                { OtherFlags = otherFlags
                  UseEditFiles = useEditFiles 
                  Checker = checker }
                |> FullCrackedFsprojBuilder.FSharpArgs


type FullCrackedFsproj =
    { Value: CrackedFsproj
      Refs: FullCrackedFsproj list }

[<RequireQualifiedAccess>]
module FullCrackedFsproj =
    let easyGetAllProjPaths (entryProjectFile: string) =
        let values = new HashSet<string>()
        let add projectFile = values.Add projectFile |> ignore
        let rec loop (projectFile: string) =
            let normarlizedPath = 
                logger.Debug "easyGetAllProjPaths: before nomalizeToUnixCompatiable: %s" projectFile

                let path = Path.nomalizeToUnixCompatiable projectFile

                logger.Debug "easyGetAllProjPaths: after nomalizeToUnixCompatiable: %s" path

                path

            add normarlizedPath

            let dir = Path.getDirectory normarlizedPath
            logger.Debug "easyGetAllProjPaths: directory is %s" dir
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

    let private getSourceFilesFromOtherOptions (otherOptions: string []) =
        otherOptions
        |> Array.filter(fun op -> op.EndsWith ".fs" && not <| op.EndsWith "AssemblyInfo.fs" )
        |> Array.map Path.getFullName

    /// if create with ProjectFullCrackedProjectBuilder, you may need dotnet restore first

    let create builder = async {

        let noframwork fsharpProjectOptions (file: string) =

            let outputDll = 
                let fileName = Path.GetFileNameWithoutExtension file
                let dir = Path.getDirectory file
                dir </> "bin/Debug/noframework" </> fileName + ".dll"
                |> Path.nomalizeToUnixCompatiable

            let props =
                [ "TargetPath", outputDll
                  "TargetFramework", "noframework"]
                |> Map.ofList

            let project = 
                { Value = 
                    { FSharpProjectOptions = fsharpProjectOptions
                      ProjRefs = []
                      Props = props
                      ProjPath = fsharpProjectOptions.ProjectFileName }
                    |> List.singleton
                    |> CrackedFsproj.CrackedFsproj

                  Refs = [] }

            logger.Infots "End crack project"
            ( project, Map.ofList [fsharpProjectOptions.ProjectFileName, project.Value] )


        match builder with
        | FullCrackedFsprojBuilder.Project builder ->
            logger.Infots "Begin crack project"
            let projectMapsMutable = Dictionary<string,CrackedFsproj>()
            let! allCrackedFsprojs = getAllCrackedFsprojs builder.File

            let applyOtherFlagsToEntryProject (allCrackedFsprojs: seq<CrackedFsproj> ) =
                allCrackedFsprojs |> Seq.map (fun crackedFsproj ->
                    if crackedFsproj.ProjPath = builder.File then 
                        CrackedFsproj.mapProjOptions (fun ops -> 
                            { ops with 
                                OtherOptions = 
                                    [| yield! ops.OtherOptions; yield! builder.OtherFlags |]
                                    |> Array.distinct }
                        ) crackedFsproj
                    else crackedFsproj
                )


            allCrackedFsprojs 
            |> CrackedFsproj.mapProjOtherOptionsObjRefOnly 
            |> Seq.map CrackedFsproj.mapProjOtherOptionsDebuggable
            |> Seq.map (CrackedFsproj.mapProjOptions (fun ops ->
                { ops with 
                    SourceFiles = getSourceFilesFromOtherOptions ops.OtherOptions }
            ))
            |> applyOtherFlagsToEntryProject 
            |> Seq.iter (fun crakedFsproj -> projectMapsMutable.Add (crakedFsproj.ProjPath,crakedFsproj))

            let rec loop projectFile =
                let projectInfo = projectMapsMutable.[projectFile]
                { Value = projectInfo
                  Refs = projectInfo.ProjRefs |> List.map loop }

            let project = loop builder.File

            let projectMap =
                projectMapsMutable
                |> Dictionary.toMap

            logger.Infots "End crack project"
            return (project, projectMap)

        | FullCrackedFsprojBuilder.Script builder ->
            let fsharpProjectOptions = 
                ProjectCoreCracker.getProjectOptionsFromScript builder.Checker builder.File
                    
            let otherOptions = Array.append fsharpProjectOptions.OtherOptions builder.OtherFlags |> Array.distinct

            let fsharpProjectOptions =
                { fsharpProjectOptions with 
                    OtherOptions = otherOptions
                    SourceFiles = getSourceFilesFromOtherOptions otherOptions }

            return noframwork fsharpProjectOptions builder.File

        | FullCrackedFsprojBuilder.FSharpArgs builder -> 
            let otherFlags = builder.OtherFlags
            let useEditFiles = builder.UseEditFiles
            let checker = builder.Checker

            let sourceFiles, otherFlags2 = otherFlags |> Array.partition (fun arg -> arg.EndsWith(".fs") || arg.EndsWith(".fsi") || arg.EndsWith(".fsx"))
            let sourceFiles = sourceFiles |> Array.map Path.nomalizeToUnixCompatiable

            match sourceFiles with 
            | [| script |] when script.EndsWith ".fsx" ->
                let text = FileSystem.readFile script useEditFiles
                let fsharpProjectOptions, errors = checker.GetProjectOptionsFromScript(script, text, otherFlags=otherFlags2) |> Async.RunSynchronously
                if errors.Length > 0 then 
                    failwithf "%A" errors

                return noframwork fsharpProjectOptions script

            | _ -> 
                let fsproj = Path.GetTempFileName() |> Path.changeExtension ".fsproj"
                let fsharpProjectOptions = checker.GetProjectOptionsFromCommandLineArgs(fsproj, otherFlags2)

                let fsharpProjectOptions = 
                    { fsharpProjectOptions with SourceFiles = sourceFiles }

                return noframwork fsharpProjectOptions fsproj 
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

let crackedFsprojBundle useEditFiles builder = MailboxProcessor<CrackedFsprojBundleMsg>.Start(fun inbox ->
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

    let (project, cache) = 
        try 
            FullCrackedFsproj.create builder |> Async.RunSynchronously
        with ex ->
            logger.Error "ERRORS: %A" ex
            failwithf "ERRORS: %A" ex

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

