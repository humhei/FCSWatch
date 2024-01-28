module FcsWatch.Core.Types
open System.IO
open FcsWatch
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Collections.Generic
open System.Xml
open FcsWatch.Core.CrackedFsproj
open System
open Fake
open System.Threading.Tasks
open Fake.IO.Globbing.Operators
open FcsWatch.Core.ProjectCoreCracker
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open Ionide.ProjInfo
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module internal ICompilerOrCheckResult =
    let processCompileOrCheckResult (result: ICompilerOrCheckResult) =
        logger.ProcessCompileOrCheckResult (result.Errors,result.ExitCode)



type NoFrameworkCrackedFsprojBuilder =
    { OtherFlags: string [] 
      UseEditFiles: bool
      Checker: RemotableFSharpChecker
      Config: Config  }
with 
    member x.Configuration = x.Config.Configuration


type ScriptCrackedFsprojBuilder =
    { OtherFlags: string [] 
      File: string 
      Checker: RemotableFSharpChecker
      Config: Config }



type ProjectCrackedFsprojBuilder =
    { OtherFlags: string [] 
      File: string
      Config: Config }

[<RequireQualifiedAccess>]
type EntryFile =
    | FsProj of string
    | ScriptingFile of string
with 
    member x.Path =
        match x with 
        | FsProj v
        | ScriptingFile v -> v 

    static member Create(file: string) =
        let file = Path.nomalizeToUnixCompatiable file
        match file with 
        | projectFile when projectFile.EndsWith ".fsproj" -> FsProj projectFile
        | scriptFile when file.EndsWith ".fs" || file.EndsWith ".fsx" || file.EndsWith ".fsi" ->    
            ScriptingFile scriptFile
        | _ -> failwithf "entry file %s should end with .fsproj;.fs;.fsx;.fsi;" file

[<RequireQualifiedAccess>]
type FullCrackedFsprojBuilder =
    | Script of ScriptCrackedFsprojBuilder
    | Project of ProjectCrackedFsprojBuilder
    | FSharpArgs of NoFrameworkCrackedFsprojBuilder

[<RequireQualifiedAccess>]
module FullCrackedFsprojBuilder =


    let getConfig = function
        | FullCrackedFsprojBuilder.Script builder -> builder.Config
        | FullCrackedFsprojBuilder.FSharpArgs builder -> builder.Config
        | FullCrackedFsprojBuilder.Project builder-> builder.Config

    let getConfigurationText builder = 
        let configuration = getConfig builder
        Configuration.name configuration.Configuration

    /// <param name="entryFileOp">file end with *.fs;*.fsproj;*.fsx;*.fsi; If None then otherflags will be applied</param>
    let create (config: Config)  checker (entryFileOp: EntryFile option) =
        let workingDir = Path.nomalizeToUnixCompatiable (config.WorkingDir)

        let entryFileOp = 
            match entryFileOp with 
            | Some entryFile -> Some (entryFile)
            | None ->
                !! (workingDir </> "*.fsproj")
                |> List.ofSeq
                |> function 
                    | [ ] ->  None
                    | [ file ] -> 
                        logger.Important "using implicit project file '%s'" file
                        Some (EntryFile.FsProj file)
                    | file1 :: file2 :: _ -> 
                        failwithf "multiple project files found, e.g. %s and %s" file1 file2 

        match entryFileOp with 
        | Some file ->
            match file with 
            | EntryFile.FsProj projectFile ->
                { OtherFlags = config.OtherFlags
                  File = projectFile
                  Config = config }
                |> FullCrackedFsprojBuilder.Project

            | EntryFile.ScriptingFile scriptFile ->
                { OtherFlags = config.OtherFlags
                  File = scriptFile
                  Checker = checker
                  Config = config }
                |> FullCrackedFsprojBuilder.Script

        | None ->
            match config.OtherFlags with 
            | [||] -> failwithf "no project file found, no compilation arguments given and no project file found in \"%s\"" workingDir 
            | _ ->
                { OtherFlags = config.OtherFlags
                  UseEditFiles = config.UseEditFiles 
                  Checker = checker
                  Config = config }
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
            let normarlizedPath = Path.nomalizeToUnixCompatiable projectFile

            add normarlizedPath

            let dir = Path.getDirectory normarlizedPath
            let doc = new XmlDocument()
            doc.Load(normarlizedPath)

            for node in doc.GetElementsByTagName "ProjectReference" do
                let includeAttr = node.Attributes.GetNamedItem ("Include")
                let includeValue = includeAttr.Value

                let path = dir </> includeValue

                loop path

        loop entryProjectFile
        Set.ofSeq values

    let internal getAllCrackedFsprojs_internal (config: Config) (projectFile: string) allProjects =

        let allProjOptions =
            let cwd =
                System.IO.Path.GetDirectoryName projectFile
                |> System.IO.DirectoryInfo

            let toolsPath = Ionide.ProjInfo.Init.init cwd None

            let props =
                let configurationText = Configuration.name config.Configuration
                let targetFramework =
                    match config.TargetFramework with 
                    | None -> []
                    | Some targetFramework -> [ "TargetFramework",targetFramework]
                        
                targetFramework @ [ "Configuration", configurationText ]

            let loader = WorkspaceLoaderViaProjectGraph.Create(toolsPath, props)
            loader.LoadProjects(Set.toList allProjects)
            |> List.ofSeq
            |> function 
                | [] ->
                    let loaderError =
                        ProjectLoader.loadProject projectFile BinaryLogGeneration.Off []

                    failwithf "%A" loaderError

                | v -> v


        allProjOptions
        |> List.map(fun projOptions ->
            CrackedFsproj.create config.Configuration config.OtherFlags projOptions allProjOptions
        )


    let getAllCrackedFsprojs (config: Config) projectFile =

        let allProjects = easyGetAllProjPaths projectFile
        getAllCrackedFsprojs_internal config projectFile allProjects



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

    /// if create with ProjectFullCrackedProjectBuilder, you may need dotnet restore first

    let create builder = 

        let config = FullCrackedFsprojBuilder.getConfig builder

        let configurationText = Configuration.name config.Configuration

        let noframwork fsharpProjectOptions (file: string) =

            let outputDll = 


                let fileName = Path.GetFileNameWithoutExtension file
                let dir = Path.getDirectory file
                dir </> sprintf "bin/%s/noframework" configurationText </> fileName + ".dll"
                |> Path.nomalizeToUnixCompatiable

            let props =
                [ "TargetPath", outputDll
                  "TargetFramework", "noframework"]
                |> Map.ofList

            let project = 
                { Value = 
                    let proj = 
                        { FSharpProjectOptions = fsharpProjectOptions
                          ProjRefs = []
                          Props = props
                          TargetPath = outputDll
                          TargetFramework = "noframework"
                          ProjPath = fsharpProjectOptions.ProjectFileName
                          }

                    let proj =
                        proj
                        |> SingleTargetCrackedFsproj.mapProjOtherOptions(fun otherOptions ->
                            Array.append otherOptions [|sprintf "-o:%s" proj.ObjTargetFile|]
                        )

                    proj
                    |> List.singleton
                    |> CrackedFsproj.CrackedFsproj

                  Refs = [] }


            logger.Infots "End crack project"
            ( project, Map.ofList [fsharpProjectOptions.ProjectFileName, project.Value] )


        match builder with
        | FullCrackedFsprojBuilder.Project builder ->
            logger.Infots "Begin crack project"
            let projectMapsMutable = Dictionary<string,CrackedFsproj>()
            let allCrackedFsprojs = getAllCrackedFsprojs config builder.File

            allCrackedFsprojs
            |> CrackedFsproj.mapProjOtherOptionsObjRefOnly
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
            (project, projectMap)

        | FullCrackedFsprojBuilder.Script builder ->
            let getSourceFilesFromOtherOptions (otherOptions: string []) (additionalConfig: AdditionalProjInfoConfig ) projDirectory =
                let includePaths = additionalConfig.IncludeGlobs |> Array.map (fun glob -> Path.Combine(projDirectory, glob))
                let excludePaths = additionalConfig.ExcludeGlobs |> Array.map (fun glob -> Path.Combine(projDirectory, glob))

                let includeGlob = includePaths |> Array.fold (fun globs s -> globs ++ s) (!! "")
                let finalGlob = excludePaths |> Array.fold (fun globs s -> globs -- s) includeGlob
                let extraSources = finalGlob |> Seq.toArray

                otherOptions
                |> Array.filter(fun op ->
                    (op.EndsWith ".fs" && not <| op.EndsWith "AssemblyInfo.fs"))
                |> Array.append (Seq.toArray extraSources)
                |> Array.distinct

            let fsharpProjectOptions = 
                ProjectCoreCracker.getProjectOptionsFromScript builder.Checker builder.File

            let otherOptions = 
                Array.append fsharpProjectOptions.OtherOptions builder.OtherFlags 
                |> Array.append fsharpProjectOptions.SourceFiles
                |> Array.distinct

            let fsharpProjectOptions =
                { fsharpProjectOptions with 
                    OtherOptions = otherOptions
                    SourceFiles = getSourceFilesFromOtherOptions otherOptions AdditionalProjInfoConfig.Empty "" }

            noframwork fsharpProjectOptions builder.File

        | FullCrackedFsprojBuilder.FSharpArgs builder -> 
            let otherFlags = builder.OtherFlags
            let useEditFiles = builder.UseEditFiles
            let checker = builder.Checker

            let sourceFiles, otherFlags2 = otherFlags |> Array.partition (fun arg -> arg.EndsWith(".fs") || arg.EndsWith(".fsi") || arg.EndsWith(".fsx"))
            let sourceFiles = sourceFiles |> Array.map Path.nomalizeToUnixCompatiable

            match sourceFiles with 
            | [| script |] when script.EndsWith ".fsx" ->
                let text = 
                    FileSystem.readFile script useEditFiles

                let fsharpProjectOptions, errors = checker.GetProjectOptionsFromScript_Serializable(script, text, otherFlags=otherFlags2) |> Async.RunSynchronously
                if errors.Length > 0 then 
                    failwithf "%A" errors

                noframwork fsharpProjectOptions script

            | _ -> 
                let fsproj = Path.GetTempFileName() |> Path.changeExtension ".fsproj"
                let fsharpProjectOptions = checker.GetProjectOptionsFromCommandLineArgs(fsproj, otherFlags2)

                let fsharpProjectOptions = 
                    { fsharpProjectOptions with SourceFiles = sourceFiles }

                noframwork fsharpProjectOptions fsproj 



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

    member x.FindedProjects_ReferTo(proj: string) =
        x.ProjectMap
        |> List.ofSeq
        |> List.filter(fun m ->
            m.Key <> proj
        )
        |> List.filter(fun m ->
            let refProjects = x.ProjRefersMap.[m.Key]
            let rec loop (refProjects: CrackedFsproj list) =
                match refProjects with 
                | [] -> false 
                | _ ->
                    let b = 
                        refProjects
                        |> List.exists(fun refProj -> refProj.ProjPath = proj)
                    match b with 
                    | true -> true
                    | false -> 
                        refProjects
                        |> List.exists(fun refProj -> loop x.ProjRefersMap.[refProj.ProjPath])
                    
            loop refProjects
        )
        |> List.map (fun m -> m.Key)
            


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

    let update projPaths builder (cache: CrackedFsprojBundleCache) = async {

        let config = FullCrackedFsprojBuilder.getConfig builder

        let addOrUpdate (projectMap: Map<string, CrackedFsproj>) =
            let relatedProjectPaths =
                projPaths
                |> List.collect(fun m -> cache.FindedProjects_ReferTo m)
                |> Set.ofList

            let relatedProjects =
                FullCrackedFsproj.getAllCrackedFsprojs_internal config cache.EntryProjectFile relatedProjectPaths

            (projectMap, relatedProjects)
            ||> List.fold (fun projectMap crackedFsproj ->
                Map.add crackedFsproj.ProjPath crackedFsproj projectMap
            )

        let objRefOnly (projectMap: Map<string, CrackedFsproj>) =
            let values = 
                projectMap
                |> Seq.map (fun pair -> pair.Value)
                |> CrackedFsproj.mapProjOtherOptionsObjRefOnly


            values |> Seq.map (fun value -> value.ProjPath, value) |> Map.ofSeq

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
    inbox.Error.Add(fun error -> logger.Error "%A" error)
    let rec loop (entry: FullCrackedFsproj) cache = async {
        let! msg = inbox.Receive()
        match msg with
        | CrackedFsprojBundleMsg.GetCache replyChannel ->
            replyChannel.Reply cache
            return! loop entry cache

        | CrackedFsprojBundleMsg.DetectProjectFileChanges (fileChanges, replyChannel) ->
            let projectFiles = fileChanges |> List.map (fun fileChange -> fileChange.FullPath)
            let! newCache = CrackedFsprojBundleCache.update projectFiles builder cache
            replyChannel.Reply newCache
            return! loop entry newCache
    }

    let (project, cache) = 
        try 
            FullCrackedFsproj.create builder 
        with ex ->
            logger.Error "ERRORS: %A" ex
            reraise()

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

