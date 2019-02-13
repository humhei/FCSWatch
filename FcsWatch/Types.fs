module FcsWatch.Types 
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Fake.IO
open Fake.IO.FileSystemOperators
open Atrous.Core
open System.Collections.Generic
open System.Xml
open Microsoft.FSharp.Compiler.SourceCodeServices

type Plugin =
    { Load: unit -> unit 
      Unload: unit -> unit 
      Calculate: unit -> unit 
      DebuggerAttachTimeDelay: int }

[<RequireQualifiedAccess>]
type DevelopmentTarget =
    | Program 
    | AtOnce of load: (unit -> unit) * unLoad: (unit -> unit)
    | Plugin of Plugin


type Config =
    {
        Logger: Logger
        WorkingDir: string
        DevelopmentTarget: DevelopmentTarget
    }
    
module Logger =
    let copyFile src dest logger =
        File.Copy(src,dest,true)
        let msg = sprintf "%s ->\n%s" src dest
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
        
[<RequireQualifiedAccess>]    
module FSharpProjectOptions =
    let mapOtherOptions mapping (fsharpProjectOptions: FSharpProjectOptions) =
        { fsharpProjectOptions with 
            OtherOptions = fsharpProjectOptions.OtherOptions |> Array.map mapping }

        


type CrackedFsprojInfo =
    { FSharpProjectOptions: FSharpProjectOptions 
      ProjRefs: string list
      Props: Map<string,string>
      ProjPath: string }

with 
    member x.TargetPath = x.Props.["TargetPath"]
    member x.TargetFramework = x.Props.["TargetFramework"]
    member x.TargetPdbPath = Path.changeExtension ".pdb" x.TargetPath
    member x.TargetFileName = Path.GetFileName(x.TargetPath)
    member x.TargetPdbName = Path.changeExtension ".pdb" x.TargetFileName
    member x.ObjTargetFile = 
        let projDir = Path.getDirectory x.ProjPath
        let relative = Path.toRelativeFrom projDir x.TargetPath
        let objRelative = 
            if relative.StartsWith ".\\bin" then  "obj" + relative.Substring 5
            else failwithf "is not a valid bin relativePath %s" relative
        projDir </> objRelative

    member x.ObjTargetPdb = Path.changeExtension ".pdb" x.ObjTargetFile

[<RequireQualifiedAccess>]
module CrackedFsprojInfo =
    let compile (checker: FSharpChecker) (crackedProjectOptions: CrackedFsprojInfo) = async {
        let tmpDll = crackedProjectOptions.ObjTargetFile

        let baseOptions = 
            crackedProjectOptions.FSharpProjectOptions.OtherOptions 
            |> Array.map (fun op -> if op.StartsWith "-o:" then "-o:" + tmpDll else op)
        
        let fscArgs = Array.concat [[|"fsc.exe"|]; baseOptions;[|"--nowin32manifest"|]] 
        let! errors,exitCode = checker.Compile(fscArgs)
        return
            { Errors = errors
              ExitCode = exitCode
              Dll = tmpDll }
    }

    let mapProjOptions mapping (crackedProjectOptions: CrackedFsprojInfo) =
        { crackedProjectOptions with FSharpProjectOptions = mapping crackedProjectOptions.FSharpProjectOptions }

                
[<RequireQualifiedAccess>]
type CrackedFsprojInfoTarget =
    | Single of CrackedFsprojInfo
    | Multiple of CrackedFsprojInfo list

with    
    static member asList = function 
            | CrackedFsprojInfoTarget.Single projOption -> [projOption]
            | CrackedFsprojInfoTarget.Multiple projOptions -> projOptions
    static member getCrackedFsprojInfo projOptionsKind =
        let list = CrackedFsprojInfoTarget.asList projOptionsKind
        list.[0]

    member x.ProjRefs =
        let crackedFsprojInfo = CrackedFsprojInfoTarget.getCrackedFsprojInfo x
        crackedFsprojInfo.ProjRefs

    member x.ProjPath =
        let crackedFsprojInfo = CrackedFsprojInfoTarget.getCrackedFsprojInfo x
        crackedFsprojInfo.ProjPath

    member x.SourceFiles =
        CrackedFsprojInfoTarget.getCrackedFsprojInfo x
        |> fun crackedFsprojInfo -> crackedFsprojInfo.FSharpProjectOptions.OtherOptions
        |> Array.filter(fun op -> op.EndsWith ".fs" && not <| op.EndsWith "AssemblyInfo.fs" )

    

[<RequireQualifiedAccess>]
module CrackedFsprojInfoTarget =
    let create projectFile =
        match ProjectCoreCracker.getProjectOptionsFromProjectFile projectFile with 
        | [|projOptions,projRefs,props|] ->
                CrackedFsprojInfoTarget.Single 
                    { FSharpProjectOptions = projOptions
                      Props = props; ProjPath = projectFile
                      ProjRefs = projRefs }

        | [||] -> failwith "empty array"
        | results -> 
            results 
            |> Array.map (fun (projOptions, projRefs ,props) -> { FSharpProjectOptions = projOptions; Props = props; ProjPath = projectFile; ProjRefs = projRefs }) 
            |> List.ofSeq
            |> CrackedFsprojInfoTarget.Multiple
                    

    let mapProjOptions mapping  = function 
        | CrackedFsprojInfoTarget.Single projOptions -> 
            CrackedFsprojInfo.mapProjOptions mapping projOptions
            |> CrackedFsprojInfoTarget.Single 

        | CrackedFsprojInfoTarget.Multiple projOptionsList -> 
            projOptionsList 
            |> List.map (CrackedFsprojInfo.mapProjOptions mapping) 
            |> CrackedFsprojInfoTarget.Multiple

    let mapPropOtherOptions mapping = 
        mapProjOptions (fun projOptions ->
            { projOptions with 
                OtherOptions = projOptions.OtherOptions |> Array.map mapping  }
        )


    let compile (checker: FSharpChecker) (crackedFsProjInfoTarget: CrackedFsprojInfoTarget) = async {
        match crackedFsProjInfoTarget with 
        | CrackedFsprojInfoTarget.Single projOptions -> 
            let! result = CrackedFsprojInfo.compile checker projOptions
            return [result]

        | CrackedFsprojInfoTarget.Multiple projOptionsList ->
            let results = 
                projOptionsList           
                |> List.map (CrackedFsprojInfo.compile checker)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> List.ofArray

            return results
    }

    let objRefOnlyForAll (crackedFsProjInfoTargets: seq<CrackedFsprojInfoTarget>) =
        crackedFsProjInfoTargets
        |> Seq.map (fun info ->
            let projRefs = info.ProjRefs |> List.map (fun ref ->
                let refInfo = 
                    crackedFsProjInfoTargets
                    |> Seq.find (fun otherInfo -> otherInfo.ProjPath = ref)
                refInfo
            )

            let allRefProjInfos = 
                projRefs
                |> List.collect CrackedFsprojInfoTarget.asList


            mapPropOtherOptions (fun line ->
                allRefProjInfos
                |> List.tryFind (fun ref -> 
                    "-r:" + ref.TargetPath = line)
                |> function 
                    | Some ref -> "-r:" + ref.ObjTargetFile
                    | None -> line 
            ) info
        )




type CrackedFsprojFileTree =
    {
        ProjOtherOptions: string []
        ObjTargetFile: string
        TargetPath: string
        ProjPath: string
        TargetFileName: string
        ObjTargetPdb: string
        TargetPdbName: string
        TargetPdbPath: string
        TargetFramework: string
        ProjRefs: string list
    }
with 
    member x.TargetDir = Path.getDirectory x.TargetPath
    member x.RefDlls = x.ProjOtherOptions |> Array.filter(fun op ->
        op.StartsWith "-r:" && x.ProjRefs |> List.exists (fun ref -> Path.GetFileName op = Path.GetFileName ref + ".dll")
    )


[<RequireQualifiedAccess>]
module CrackedFsprojFileTree =
    let ofCrackedProjectOptions (crackedProjectOptions: CrackedFsprojInfo) =
        {
            ProjOtherOptions = crackedProjectOptions.FSharpProjectOptions.OtherOptions
            ObjTargetFile = crackedProjectOptions.ObjTargetFile
            TargetPath = crackedProjectOptions.TargetPath
            ProjPath = crackedProjectOptions.ProjPath
            TargetFileName = crackedProjectOptions.TargetFileName
            ObjTargetPdb = crackedProjectOptions.ObjTargetPdb
            TargetPdbName = crackedProjectOptions.TargetPdbName
            TargetPdbPath = crackedProjectOptions.TargetPdbPath
            TargetFramework = crackedProjectOptions.TargetFramework
            ProjRefs = crackedProjectOptions.ProjRefs
        }

    /// from ref projs to bin
    let copyFileFromRefs projectFile logger (destCrackedFsprojFileTree: CrackedFsprojFileTree) =
        let originDllName = Path.GetFileNameWithoutExtension projectFile
        let targetDir = destCrackedFsprojFileTree.TargetDir
        destCrackedFsprojFileTree.RefDlls
        |> Array.filter(fun refDll -> Path.GetFileNameWithoutExtension refDll = originDllName)
        |> Array.iter (fun originDll ->
            let fileName = Path.GetFileName originDll
            let destDll = targetDir </> fileName
            Logger.copyFile originDll destDll logger
            let originPdb = originDll |> Path.changeExtension ".pdb"
            let destPdb = targetDir </> (Path.changeExtension ".pdb" fileName)
            Logger.copyFile originPdb destPdb logger
        )

[<RequireQualifiedAccess>]
type CrackedFsprojFileTreeTarget =
    | Single of CrackedFsprojFileTree
    | Multiple of CrackedFsprojFileTree list
    
with 
    member x.ProjPath = 
        match x with 
        | CrackedFsprojFileTreeTarget.Single crackedFsprojFileTree -> crackedFsprojFileTree.ProjPath
        | CrackedFsprojFileTreeTarget.Multiple crackedFsprojFileTrees -> 
            crackedFsprojFileTrees 
            |> List.map (fun crackedFsprojFileTree -> crackedFsprojFileTree.ProjPath)
            |> List.distinct
            |> List.exactlyOne


[<RequireQualifiedAccess>]
module CrackedFsprojFileTreeTarget =

    let asList = function 
        | CrackedFsprojFileTreeTarget.Single fileTree -> [fileTree]
        | CrackedFsprojFileTreeTarget.Multiple fileTrees -> fileTrees

    let ofCrackedFsprojInfoTarget (crackedFsprojInfoTarget: CrackedFsprojInfoTarget) =
        match crackedFsprojInfoTarget with 
        | CrackedFsprojInfoTarget.Single crackedFsprojInfo -> 
            CrackedFsprojFileTree.ofCrackedProjectOptions crackedFsprojInfo
            |> CrackedFsprojFileTreeTarget.Single
        | CrackedFsprojInfoTarget.Multiple crackedFsprojInfos ->
            crackedFsprojInfos
            |> List.map CrackedFsprojFileTree.ofCrackedProjectOptions
            |> CrackedFsprojFileTreeTarget.Multiple



    let copyFileFromRefs projectFile logger (destCrackedFsprojFileTreeTarget: CrackedFsprojFileTreeTarget) =
        asList destCrackedFsprojFileTreeTarget
        |> List.iter (CrackedFsprojFileTree.copyFileFromRefs projectFile logger)


[<RequireQualifiedAccess>]
module Dictionary =

    let toMap dictionary = 
        (dictionary :> seq<_>)
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq

type Fsproj =
    {
        Value: CrackedFsprojInfoTarget
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

    let sourceFileMap (cache: Map<string,CrackedFsprojInfoTarget>) = 
        let dict = new Dictionary<string, string>()
        cache |> Seq.iter (fun pair -> 
            pair.Value.SourceFiles |> Seq.iter (fun sourceFile ->
                dict.Add(sourceFile,pair.Key)
            )
        )
        Dictionary.toMap dict

    let private getAllFsprojInfos projectFile =
        let allProjects = easyGetAllProjects projectFile
        allProjects
        |> Seq.map (fun projectFile -> async {
            return CrackedFsprojInfoTarget.create projectFile
        })
        |> Async.Parallel
        |> Async.RunSynchronously
                

    /// "bin ref may be locked by program"
    let getAllFsprojInfosObjRefOnly projectFile =
        let allInfos = getAllFsprojInfos projectFile 
        CrackedFsprojInfoTarget.objRefOnlyForAll allInfos


    let create (projectMaps: Map<string,CrackedFsprojInfoTarget>) projectFile = 
        let projectMapsMutable = Dictionary<string,CrackedFsprojInfoTarget>(projectMaps)
        let allProjectInfos = getAllFsprojInfosObjRefOnly projectFile
        allProjectInfos |> Seq.iter (fun projectInfo -> projectMapsMutable.Add (projectInfo.ProjPath,projectInfo))

        let rec loop projectFile = 
            let projectInfo = projectMapsMutable.[projectFile]         
            {
                Value = projectInfo
                Refs = projectInfo.ProjRefs |> List.map loop
            }
    
        let project = loop projectFile

        let newProjectMaps = 
            projectMapsMutable 
            |> Dictionary.toMap

        project,newProjectMaps,sourceFileMap newProjectMaps    

    let scan (fsproj: Fsproj) =
        let cacheMutable = new Dictionary<string,CrackedFsprojFileTreeTarget list>()
        let rec loop (stack: CrackedFsprojFileTreeTarget list) (fsproj: Fsproj) = 
            match cacheMutable.TryGetValue fsproj.Value.ProjPath with 
            | true,stack2 -> 
                let newStack = stack @ stack2 
                cacheMutable.[fsproj.Value.ProjPath] <- newStack
            | false,_ ->
                let newStack = (CrackedFsprojFileTreeTarget.ofCrackedFsprojInfoTarget fsproj.Value) :: stack
                cacheMutable.Add(fsproj.Value.ProjPath,newStack)
                fsproj.Refs |> List.iter (loop newStack)

        loop [] fsproj
        cacheMutable 
        |> Dictionary.toMap
        |> Map.map (fun key fileTrees ->
            fileTrees
            |> List.distinctBy (fun fileTree -> fileTree.ProjPath)
        )

    let getLevel projectFile (entryFsproj: Fsproj) =
        let rec loop level (fsproj: Fsproj) = 
            [
                if fsproj.Value.ProjPath = projectFile 
                then yield! [level]
                yield! fsproj.Refs |> List.collect (loop (level + 1)) 
            ]

        loop 0 entryFsproj   
        |> List.max


type CrackedFsprojFileBundleCache =
    {
        /// projectFile, project refers
        FileTreeTargetsMap: Map<string,CrackedFsprojFileTreeTarget list>
        

        ProjectMap: Map<string,CrackedFsprojInfoTarget>

        /// sourceFile,projectFile
        SourceFileMap: Map<string,string>

        /// from top to bottom
        DescendedProjectLevelMap: Map<string,int>
    }
with 
    member x.AllProjectFiles = x.ProjectMap |> Seq.map (fun pair -> pair.Key)

[<RequireQualifiedAccess>]
module CrackedFsprojFileBundleCache =
    let update projectFile (cache: CrackedFsprojFileBundleCache) =
        let infoTarget = CrackedFsprojInfoTarget.create projectFile
        let newProjectMap = Map.add projectFile infoTarget cache.ProjectMap

        let objRefOnly =
            let keys = newProjectMap |> Seq.map (fun pair -> pair.Key)
            let values = newProjectMap |> Seq.map (fun pair -> pair.Value)
            Seq.zip keys (CrackedFsprojInfoTarget.objRefOnlyForAll values)
            |> Map.ofSeq

        let newSourceFileMap = Fsproj.sourceFileMap newProjectMap
        { cache with 
            ProjectMap = objRefOnly
            SourceFileMap = newSourceFileMap }


    let sortDescendingProjects (projectFiles: seq<string>) (cache:CrackedFsprojFileBundleCache) =
        projectFiles 
        |> Seq.sortByDescending (fun projectFile -> cache.DescendedProjectLevelMap.[projectFile])
    

[<RequireQualifiedAccess>]
type CrackedFsprojBundleMsg =
    | Cache of replyChannel: AsyncReplyChannel<CrackedFsprojFileBundleCache>
    | DetectProjectFileChange of FileChange * AsyncReplyChannel<CrackedFsprojFileBundleCache>

let crackedFsprojBundle(projectFile: string) = MailboxProcessor<CrackedFsprojBundleMsg>.Start(fun inbox ->
    let rec loop (entry: Fsproj) cache = async {
        let! msg = inbox.Receive()
        match msg with 
        | CrackedFsprojBundleMsg.Cache replyChannel ->
            replyChannel.Reply cache
            return! loop entry cache
        | CrackedFsprojBundleMsg.DetectProjectFileChange (fileChange,replyChannel) ->
            let projectFile = fileChange.FullPath
            let newCache = CrackedFsprojFileBundleCache.update projectFile cache
            replyChannel.Reply newCache
            return! loop entry newCache
    }
    let (project,projectMap,sourceFileMap) = Fsproj.create Map.empty projectFile
    let scanCache = Fsproj.scan project
    let descendedProjectLevelMap = 
        let projects = projectMap |> Seq.map (fun pair -> pair.Key)
        projects |> Seq.map (fun proj -> 
            proj,Fsproj.getLevel proj project
        )
        |> Map.ofSeq

    let cache =
        {
            FileTreeTargetsMap = scanCache
            ProjectMap = projectMap
            SourceFileMap = sourceFileMap 
            DescendedProjectLevelMap = descendedProjectLevelMap
        }
    loop project cache
)
