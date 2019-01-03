module FcsWatch.Types 
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Fake.IO
open Fake.IO.FileSystemOperators
open Atrous.Core
open Atrous.Core.Utils
open System.Collections.Generic

type Config =
    {
        Logger: Logger
        DebuggingServerPort: int
        WorkingDir: string
        /// the timer waiting the watcher get the file change when press F5
        // FileSavingTimeBeforeDebugging: int 
    }
    
module Logger =
    let copyFile src dest logger =
        File.Copy(src,dest,true)
        let msg = sprintf "copy file %s to %s" src dest
        Logger.info msg logger

    let internal processCompileResult logger (errors,exitCode) =
        if exitCode = 0 then Logger.warn (sprintf "WARNINGS:\n%A" errors) logger
        else Logger.error (sprintf "ERRORS:\n%A" errors) logger

type internal CompilerResult =
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


    let internal warmupCompile logger (checker: FSharpChecker) (crackedFsProj: CrackedFsprojInfo) = async {
        let baseOptions = 
            crackedFsProj.ProjOptions.OtherOptions 
            |> Array.mapi (fun i op -> if i = 0 then "-o:" + Path.GetTempFileName() else op)
            |> Array.filter (fun op -> not <| op.EndsWith ".fs")

        let fscArgs = Array.concat [[|"fsc.exe"|]; baseOptions;[|"--nowin32manifest"|]] 
        checker.Compile(fscArgs)
        |> Async.RunSynchronously
        |> ignore
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
    
       
type CrackedFsprojBundle(projectFile: string, logger: Logger,checker: FSharpChecker) =
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


    let getAllFsprojs() =
        entry.Info :: (refs |> List.collect ReferenceProject.getAllFsprojs)


    member __.ScanProject projectFile =
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
        
    member __.FileMaps() =
        getAllFsprojs()
        |> List.map (fun proj -> proj.Path,proj.SourceFiles)
        |> dict


    member __.Entry = entry

        


