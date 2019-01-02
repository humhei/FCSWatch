module FcsWatch.Types 
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Fake.IO
open Fake.IO.FileSystemOperators
open Atrous.Core
open Atrous.Core.Utils
open Fake.IO
open System.Collections.Generic

type Config =
    {
        Logger: Logger
    }
    
module Logger =
    let copyFile src dest logger =
        File.Copy(src,dest,true)
        Logger.info (sprintf "copy file %s to %s" src dest) logger

    let internal processCompileResult logger (errors,exitCode) =
        if exitCode = 0 then Logger.warn (sprintf "%A" errors) logger
        else Logger.error (sprintf "%A" errors) logger

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
         
         
        

module CrackedFsprojInfo = 
    let create projectFile =
        let projOptions,projRefs,props = ProjectCoreCracker.getProjectOptionsFromProjectFile projectFile 
        {
            ProjOptions = projOptions
            ProjRefs = projRefs
            Props = props 
            Path = projectFile
        }


    let internal warmupCompile logger (checker: FSharpChecker) (crackedFsProj: CrackedFsprojInfo) = async {
        let baseOptions = 
            crackedFsProj.ProjOptions.OtherOptions 
            |> Array.mapi (fun i op -> if i = 0 then "-o:" + Path.GetTempFileName() else op)
            |> Array.filter (fun op -> not <| op.EndsWith ".fs")

        let fscArgs = Array.concat [[|"fsc.exe"|]; baseOptions;[|"--nowin32manifest"|]] 
        checker.Compile(fscArgs)
        |> Async.RunSynchronously
        |> Logger.processCompileResult logger
    }
        
    let compile (checker: FSharpChecker) (crackedFsProj: CrackedFsprojInfo) =
        let baseOptions = crackedFsProj.ProjOptions.OtherOptions |> Array.mapi (fun i op -> if i = 0 then "-o:" + crackedFsProj.ObjTargetFile else op)
        let fscArgs = Array.concat [[|"fsc.exe"|]; baseOptions;[|"--nowin32manifest"|]] 
        checker.Compile(fscArgs)

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

    let copyFileInLocker (lockerFactory: LockerFactory<string>) (logger: Logger) (crackerFsprojFileTree1: CrackerFsprojFileTree) (crackerFsprojFileTree2: CrackerFsprojFileTree)=                
        let targetFileName = crackerFsprojFileTree1.TargetFileName
        let targetPdbName = crackerFsprojFileTree1.TargetPdbName
        let originFile = crackerFsprojFileTree1.ObjTargetFile  
        let originDll = crackerFsprojFileTree1.ObjTargetPdb
        let targetPath =  crackerFsprojFileTree2.TargetDir </> targetFileName
        lockerFactory.Lock targetPath (fun _ -> 
            Logger.copyFile originFile targetPath logger
        )
        let targetPdb = crackerFsprojFileTree2.TargetDir </> targetPdbName
        lockerFactory.Lock targetPdb (fun _ -> 
            Logger.copyFile originDll targetPdb logger
        )   


type ReferenceProject =
    {
        Value: CrackedFsprojInfo
        Refs: ReferenceProject list
    }

[<RequireQualifiedAccess>]
module ReferenceProject =
    let create buildCrackerFsproj projectFile = 
        let rec loop projectFile = 
            let entry = buildCrackerFsproj projectFile         
            {
                Value = entry
                Refs = entry.ProjRefs |> List.map loop
            }
        loop projectFile        

    let rec getAllFsprojs (referenceProject: ReferenceProject) =
        [
            yield referenceProject.Value
            yield! List.collect getAllFsprojs referenceProject.Refs 
        ] |> List.distinctBy (fun proj -> proj.Path) 


[<RequireQualifiedAccess>]
type Fsproj =
    | Entry of CrackedFsprojInfo
    | Ref of ReferenceProject
with 
    member x.Info = 
        match x with 
        | Fsproj.Entry info -> info
        | Fsproj.Ref ref -> ref.Value
    member x.TargetFileName = x.Info.TargetFileName
    member x.Path = x.Info.Path

    member x.ProjRefs = x.Info.ProjRefs
    static member asReference = function
        | Fsproj.Entry _ -> None
        | Fsproj.Ref ref -> Some ref
    
       
and CrackedFsprojBundle(projectFile: string, logger: Logger,checker: FSharpChecker) =

    let lockerFactory = new LockerFactory<string>()
    let searchCache = new BufferedConcurrentDictionany<string,Fsproj>(100,ignore)
    let entry = (searchCache.GetOrAddF (CrackedFsprojInfo.create >> Fsproj.Entry)) projectFile
    let refs = 
        entry.ProjRefs 
        |> List.map (
                searchCache.GetOrAddF(
                    (ReferenceProject.create (fun projectFile -> CrackedFsprojInfo.create projectFile)) 
                    >> Fsproj.Ref
                )
        )
        |> List.choose(Fsproj.asReference)



    let scanProject projectFile : Fsproj * CrackerFsprojFileTree list =
        let entryFileTree = CrackerFsprojFileTree.ofCrackedFsproj entry.Info
        match searchCache.TryGet projectFile with 
        | Some fsproj ->
            match fsproj with 
            | Fsproj.Entry _ -> fsproj,[entryFileTree]     
            | Fsproj.Ref _ ->
                let cache = new HashSet<string>()
                let rec loop (stack: CrackerFsprojFileTree list) (refs: ReferenceProject list) = 
                    refs |> List.collect (fun ref ->
                        if cache.Contains ref.Value.Path then stack
                        else 
                            cache.Add(ref.Value.Path) |> ignore
                            if ref.Value.Path = projectFile then stack
                            else
                                let fileTree = CrackerFsprojFileTree.ofCrackedFsproj ref.Value
                                loop (fileTree :: stack) ref.Refs        
                    )
                let result = loop  [entryFileTree] refs |> List.distinctBy (fun fileTree -> fileTree.ProjPath)
                fsproj,result
        | None -> failwithf "Cannot find project file %s" projectFile

    let getAllFsprojs() =
        entry.Info :: (refs |> List.collect ReferenceProject.getAllFsprojs)

    member x.FileMaps() =
        getAllFsprojs()
        |> List.map (fun proj -> proj.Path,proj.SourceFiles)
        |> dict


    member __.Entry = entry
    member __.CompileProject projectFile = async {

        let (fsproj,stack) = scanProject projectFile

        Logger.info (sprintf "Compiling %s" projectFile) logger
        let! errors,exitCode = CrackedFsprojInfo.compile checker fsproj.Info
        if exitCode = 0 then 
            let fsprojFileTree = CrackerFsprojFileTree.ofCrackedFsproj fsproj.Info
            fsprojFileTree :: stack 
            |> List.iter (CrackerFsprojFileTree.copyFileInLocker lockerFactory logger fsprojFileTree)
        Logger.processCompileResult logger (errors,exitCode) 
    }     

