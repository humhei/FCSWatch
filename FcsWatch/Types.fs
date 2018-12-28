module FcsWatch.Types 
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Collections.Generic
open Atrous.Core.Extensions.InLocker
open Atrous.Core
open Atrous.Core.Utils

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

type CrackedFsproj = 
    {
        ProjOptions: FSharpProjectOptions
        ProjRefs: string list
        Props: Map<string,string>
        Path: string
    }
with 
    member x.TargetPath = x.Props.["TargetPath"]

    member x.Dir =
        Path.getDirectory x.Path
    member x.ObjTargetFile = 
        
        let relative = Path.toRelativeFrom x.Dir x.TargetPath
        let objRelative = 
            if relative.StartsWith ".\\bin" then  "obj" + relative.Substring 5
            else failwithf "is not a valid bin relativePath %s" relative
        x.Dir </> objRelative

    member x.SourceFiles = 
        x.ProjOptions.OtherOptions
        |> Array.filter(fun op -> op.EndsWith ".fs" && not <| op.EndsWith "AssemblyInfo.fs" )
         
         
module CrackedFsproj = 
    let create projectFile =
        let projOptions,projRefs,props = ProjectCoreCracker.getProjectOptionsFromProjectFile projectFile 
        {
            ProjOptions = projOptions
            ProjRefs = projRefs
            Props = props 
            Path = projectFile
        }


    let internal warmupCompile logger (checker: FSharpChecker) (crackedFsProj: CrackedFsproj) = async {
        let baseOptions = 
            crackedFsProj.ProjOptions.OtherOptions 
            |> Array.mapi (fun i op -> if i = 0 then "-o:" + Path.GetTempFileName() else op)
            |> Array.filter (fun op -> not <| op.EndsWith ".fs")

        let fscArgs = Array.concat [[|"fsc.exe"|]; baseOptions;[|"--nowin32manifest"|]] 
        checker.Compile(fscArgs)
        |> Async.RunSynchronously
        |> Logger.processCompileResult logger
    }
        
    let compile (checker: FSharpChecker) (crackedFsProj: CrackedFsproj) =
        let baseOptions = crackedFsProj.ProjOptions.OtherOptions |> Array.mapi (fun i op -> if i = 0 then "-o:" + crackedFsProj.ObjTargetFile else op)
        let fscArgs = Array.concat [[|"fsc.exe"|]; baseOptions;[|"--nowin32manifest"|]] 
        checker.Compile(fscArgs)

type CrackerFsprojFileTree =
    {
        ObjTargetFile: string
        TargetPath: string
        ProjPath: string
    }

module CrackerFsprojFileTree =
    let ofCrackedFsproj (crackedFsProj: CrackedFsproj) =
        {
            ObjTargetFile = crackedFsProj.ObjTargetFile
            TargetPath = crackedFsProj.TargetPath
            ProjPath = crackedFsProj.Path
        }

type CrackedFsprojBundle = 
    {
        Entry: CrackedFsproj
        Refs: CrackedFsprojBundle list
    }
    
[<RequireQualifiedAccess>]
module CrackedFsprojBundle =
    let private createCommon buildCrackerFsproj projectFile =  
        let dict = Dictionary<string,CrackedFsproj>()
        let rec loop projectFile =
            let entry = dict.GetOrAdd(projectFile,fun _ -> buildCrackerFsproj projectFile)            
            {
                Entry = entry
                Refs = entry.ProjRefs |> List.map loop
            }

        loop projectFile
    
    let create projectFile =
        createCommon (fun projectFile -> CrackedFsproj.create projectFile) projectFile  

    let rec private getAllFsprojs (crackedFsProjBundle: CrackedFsprojBundle) =
        [
            yield crackedFsProjBundle.Entry
            yield! List.collect getAllFsprojs crackedFsProjBundle.Refs 
        ] |> List.distinctBy (fun proj -> proj.Path) 


    let tryFindByProject projectFile (crackedFsProjBundle: CrackedFsprojBundle) =
        let rec loop projectFile stack crackedFsProjBundle =
            let entryPath = crackedFsProjBundle.Entry.Path
            let fileTree = CrackerFsprojFileTree.ofCrackedFsproj crackedFsProjBundle.Entry
            if entryPath = projectFile then Some (crackedFsProjBundle,stack)
            else crackedFsProjBundle.Refs |> List.tryPick (loop projectFile (fileTree::stack))       
        loop projectFile [] crackedFsProjBundle
    
    let compileProject (lockerFactory: LockerFactory<string>) logger projectFile checker (crackedFsProjBundle: CrackedFsprojBundle) = async {
        
        let copyFileInLocker origin target = lockerFactory.Lock target (fun _ -> Logger.copyFile origin target logger)
        
        match tryFindByProject projectFile crackedFsProjBundle with 
        | Some (bundle,stack) ->
            Logger.info (sprintf "Compiling %s" projectFile) logger
            let! errors,exitCode = CrackedFsproj.compile checker bundle.Entry
            if exitCode = 0 then 
                let entry = bundle.Entry
                let originFile = entry.ObjTargetFile
                copyFileInLocker originFile entry.TargetPath      
                stack 
                |> List.iter (fun fileTree ->
                    copyFileInLocker originFile fileTree.ObjTargetFile      
                    copyFileInLocker originFile fileTree.TargetPath      
                )
            Logger.processCompileResult logger (errors,exitCode) 
        | None -> return (failwithf "Cannot find project file %s" projectFile)   
    }     

    let fileMaps (crackedFsProjBundle: CrackedFsprojBundle) =
        getAllFsprojs crackedFsProjBundle
        |> List.map (fun proj -> proj.Path,proj.SourceFiles)
        |> dict

