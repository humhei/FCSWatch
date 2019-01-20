module FcsWatch.Types 
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Fake.IO
open Fake.IO.FileSystemOperators
open Atrous.Core
open Atrous.Core.Utils
open System.Collections.Generic
open System.Xml
open System.Collections.Generic

type Config =
    {
        Logger: Logger
        WorkingDir: string
        BeforeEmitTmp: unit -> unit 
        AfterEmitTmp: unit -> unit
        EnableDebugging: bool 
    }
    
module Logger =
    let copyFile src dest logger =
        File.Copy(src,dest,true)
        let msg = sprintf "copy file %s to %s" src dest
        Logger.info msg logger

    let internal processCompileResult logger (errors,exitCode) =
        if exitCode = 0 then 
            if not <| Array.isEmpty errors then 
                Logger.warn (sprintf "WARNINGS:\n%A" errors) logger
        else Logger.error (sprintf "ERRORS:\n%A" errors) logger

type CompilerResult =
    { Dll: string
      Errors: FSharpErrorInfo []
      ExitCode: int }
with 
    member x.Pdb = Path.changeExtension ".pdb" x.Dll  

module internal CompilerResult =
    let processCompileResult logger compilerResult =
        Logger.processCompileResult logger (compilerResult.Errors,compilerResult.ExitCode)
        
type CrackedFsprojInfo = 
    {
        ProjOptions: FSharpProjectOptions
        ProjRefs: string list
        Props: Map<string,string>
        Path: string
    }
with 
    member x.TargetPath = x.Props.["TargetPath"]
    member x.TargetPdbPath = Path.changeExtension ".pdb" x.TargetPath

    member x.TargetFileName = Path.GetFileName(x.TargetPath)
    member x.TargetPdbName = Path.changeExtension ".pdb" x.TargetFileName
    member x.Dir =
        Path.getDirectory x.Path
    member x.ObjTargetFile = 
        
        let relative = Path.toRelativeFrom x.Dir x.TargetPath
        let objRelative = 
            if relative.StartsWith ".\\bin" then  "obj" + relative.Substring 5
            else failwithf "is not a valid bin relativePath %s" relative
        x.Dir </> objRelative
    member x.ObjTargetPdb = Path.changeExtension ".pdb" x.ObjTargetFile

    member x.SourceFiles = 
        x.ProjOptions.OtherOptions
        |> Array.filter(fun op -> op.EndsWith ".fs" && not <| op.EndsWith "AssemblyInfo.fs" )
         
         
        

module internal CrackedFsprojInfo = 
    let create projectFile =
        let projOptions,projRefs,props = ProjectCoreCracker.getProjectOptionsFromProjectFile projectFile 
        {
            ProjOptions = projOptions
            ProjRefs = projRefs
            Props = props 
            Path = projectFile
        }

    let compile (checker: FSharpChecker) (crackedFsProj: CrackedFsprojInfo) = async {
        let tmpDll = crackedFsProj.ObjTargetFile
        let baseOptions = crackedFsProj.ProjOptions.OtherOptions |> Array.mapi (fun i op -> if i = 0 then "-o:" + tmpDll else op)
        let fscArgs = Array.concat [[|"fsc.exe"|]; baseOptions;[|"--nowin32manifest"|]] 
        let! errors,exitCode = checker.Compile(fscArgs)
        return 
            {
                Errors = errors
                ExitCode = exitCode
                Dll = tmpDll
            }
    }

type CrackerFsprojFileTree =
    {
        ObjTargetFile: string
        TargetPath: string
        ProjPath: string
        TargetFileName: string
        ObjTargetPdb: string
        TargetPdbName: string
        TargetPdbPath: string
    }
with member x.TargetDir = Path.getDirectory x.TargetPath


module CrackerFsprojFileTree =
    let ofCrackedFsproj (crackedFsProj: CrackedFsprojInfo) =
        {
            ObjTargetFile = crackedFsProj.ObjTargetFile
            TargetPath = crackedFsProj.TargetPath
            ProjPath = crackedFsProj.Path
            TargetFileName = crackedFsProj.TargetFileName
            ObjTargetPdb = crackedFsProj.ObjTargetPdb
            TargetPdbName = crackedFsProj.TargetPdbName
            TargetPdbPath = crackedFsProj.TargetPdbPath
        }

    let copyFile  (logger: Logger) (crackerFsprojFileTree1: CrackerFsprojFileTree) (crackerFsprojFileTree2: CrackerFsprojFileTree)=                
        let targetFileName = crackerFsprojFileTree1.TargetFileName
        let targetPdbName = crackerFsprojFileTree1.TargetPdbName
        let originFile = crackerFsprojFileTree1.ObjTargetFile  
        let originDll = crackerFsprojFileTree1.ObjTargetPdb
        let targetPath =  crackerFsprojFileTree2.TargetDir </> targetFileName
        Logger.copyFile originFile targetPath logger
        let targetPdb = crackerFsprojFileTree2.TargetDir </> targetPdbName
        Logger.copyFile originDll targetPdb logger

module Dictionary =

    let toMap dictionary = 
        (dictionary :> seq<_>)
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq

type Fsproj =
    {
        Value: CrackedFsprojInfo
        Refs: Fsproj list
    }


[<RequireQualifiedAccess>]
module Fsproj =
    let private easyGetAllProjects (projectFile: string) =
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
        loop projectFile 
        Set.ofSeq values       

    let sourceFileMaps (cache: Map<string,CrackedFsprojInfo>) = 
        let dict = new Dictionary<string, string>()
        cache |> Seq.iter (fun pair -> 
            pair.Value.SourceFiles |> Seq.iter (fun sourceFile ->
                dict.Add(sourceFile,pair.Key)
            )
        )
        Dictionary.toMap dict

    let getAllFsprojInfos projectFile =
        let allProjects = easyGetAllProjects projectFile
        allProjects
        |> Seq.map (fun projectFile -> async {
            return CrackedFsprojInfo.create projectFile
        })
        |> Async.Parallel
        |> Async.RunSynchronously

    let create (cache: Map<string,CrackedFsprojInfo>) projectFile = 
        let cacheMutable = Dictionary<string,CrackedFsprojInfo>(cache)
        let allProjectInfos = getAllFsprojInfos projectFile
        allProjectInfos |> Seq.iter (fun projectInfo -> cacheMutable.Add (projectInfo.Path,projectInfo))

        let rec loop projectFile = 
            let projectInfo = cacheMutable.[projectFile]         
            {
                Value = projectInfo
                Refs = projectInfo.ProjRefs |> List.map loop
            }
    
        let project = loop projectFile

        let newCache = 
            cacheMutable 
            |> Dictionary.toMap

        project,newCache,sourceFileMaps newCache    

    let scan (fsproj: Fsproj) =
        let cacheMutable = new Dictionary<string,CrackerFsprojFileTree list>()
        let rec loop (stack: CrackerFsprojFileTree list) (fsproj: Fsproj) = 
            match cacheMutable.TryGetValue fsproj.Value.Path with 
            | true,_ -> ()
            | false,_ ->
                let newStack = (CrackerFsprojFileTree.ofCrackedFsproj fsproj.Value) :: stack
                cacheMutable.Add(fsproj.Value.Path,newStack)
                fsproj.Refs |> List.iter (loop newStack)

        loop [] fsproj
        cacheMutable 
        |> Dictionary.toMap
            

type CrackerFsprojFileBundleCache =
    {
        /// projectFile, project refers
        FileTreesMaps: Map<string,CrackerFsprojFileTree list>
        

        ProjectMaps: Map<string,CrackedFsprojInfo>

        /// sourceFile,projectFile
        SourceFileMaps: Map<string,string>
    }
with 
    member x.AllProjectFiles = x.ProjectMaps |> Seq.map (fun pair -> pair.Key)

[<RequireQualifiedAccess>]
module CrackerFsprojFileBundleCache =
    let update projectFile (cache:CrackerFsprojFileBundleCache) =
        let info = CrackedFsprojInfo.create projectFile
        let newProjectMaps = Map.add projectFile info cache.ProjectMaps
        let newSourceFileMaps = Fsproj.sourceFileMaps newProjectMaps
        { cache with 
            ProjectMaps = newProjectMaps 
            SourceFileMaps = newSourceFileMaps }

[<RequireQualifiedAccess>]
type CrackedFsprojBundleMsg =
    | Cache of replyChannel: AsyncReplyChannel<CrackerFsprojFileBundleCache>
    | DetectProjectFileChange of FileChange * AsyncReplyChannel<CrackerFsprojFileBundleCache>

let crackedFsprojBundle(projectFile: string) = MailboxProcessor<CrackedFsprojBundleMsg>.Start(fun inbox ->
    let rec loop (entry: Fsproj) cache = async {
        let! msg = inbox.Receive()
        match msg with 
        | CrackedFsprojBundleMsg.Cache replyChannel ->
            replyChannel.Reply cache
            return! loop entry cache
        | CrackedFsprojBundleMsg.DetectProjectFileChange (fileChange,replyChannel) ->
            let projectFile = fileChange.FullPath
            let newCache = CrackerFsprojFileBundleCache.update projectFile cache
            replyChannel.Reply newCache
            return! loop entry newCache
    }
    let (project,searchCache,sourceFileMap) = Fsproj.create Map.empty projectFile
    let scanCache = Fsproj.scan project
    let cache =
        {
            FileTreesMaps = scanCache
            ProjectMaps = searchCache
            SourceFileMaps = sourceFileMap 
    /// warm start    
        }
    loop project cache
)
