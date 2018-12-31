module FcsWatch.Types 
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Fake.IO
open Fake.IO.FileSystemOperators
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


type ReferenceProject =
    {
        Value: CrackedFsproj
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

    let tryFindByProject projectFile (referenceProject: ReferenceProject) =
        let rec loop projectFile stack referenceProject =
            let entryPath = referenceProject.Value.Path
            let fileTree = CrackerFsprojFileTree.ofCrackedFsproj referenceProject.Value
            if entryPath = projectFile then Some (referenceProject,stack)
            else referenceProject.Refs |> List.tryPick (loop projectFile (fileTree::stack))       
        loop projectFile [] referenceProject


type CrackedFsprojBundle(projectFile: string, logger: Logger,checker: FSharpChecker) =

    let lockerFactory = new LockerFactory<string>()
    let crackedFsprojCache = new BufferedConcurrentDictionany<string,CrackedFsproj>(100,ignore)
    let entry = CrackedFsproj.create projectFile
    let refs = 
        entry.ProjRefs 
        |> List.map (crackedFsprojCache.GetOrAddF CrackedFsproj.create)
        

    let compileProject projectFile = async {
        let copyFileInLocker origin target = lockerFactory.Lock target (fun _ -> Logger.copyFile origin target logger)
        match crackedFsprojCache.TryGet projectFile with 
        | Some project -> 
            
            return ()
        | None -> return (failwithf "Cannot find project file %s" projectFile)
        // match tryFindByProject projectFile crackedFsProjBundle with 
        // | Some (bundle,stack) ->
        //     Logger.info (sprintf "Compiling %s" projectFile) logger
        //     let! errors,exitCode = CrackedFsproj.compile checker bundle.Entry
        //     if exitCode = 0 then 
        //         let entry = bundle.Entry
        //         let originFile = entry.ObjTargetFile
        //         copyFileInLocker originFile entry.TargetPath      
        //         stack 
        //         |> List.iter (fun fileTree ->
        //             copyFileInLocker originFile fileTree.ObjTargetFile      
        //             copyFileInLocker originFile fileTree.TargetPath      
        //         )
        //     Logger.processCompileResult logger (errors,exitCode) 
        // | None -> return (failwithf "Cannot find project file %s" projectFile)   
    }     

        // | Some projectFile -> 
        // let rec loop projectFile stack crackedFsProjBundle =
        //     let entryPath = crackedFsProjBundle.Entry.Path
        //     let fileTree = CrackerFsprojFileTree.ofCrackedFsproj crackedFsProjBundle.Entry
        //     if entryPath = projectFile then Some (crackedFsProjBundle,stack)
        //     else crackedFsProjBundle.Refs |> List.tryPick (loop projectFile (fileTree :: stack))       
        // loop projectFile [] crackedFsProjBundle
    
// [<RequireQualifiedAccess>]
// module CrackedFsprojBundle =

    
//     let compileProject logger projectFile checker (crackedFsProjBundle: CrackedFsprojBundle) = async {
        
//         let copyFileInLocker origin target = lockerFactory.Lock target (fun _ -> Logger.copyFile origin target logger)
        
//         match tryFindByProject projectFile crackedFsProjBundle with 
//         | Some (bundle,stack) ->
//             Logger.info (sprintf "Compiling %s" projectFile) logger
//             let! errors,exitCode = CrackedFsproj.compile checker bundle.Entry
//             if exitCode = 0 then 
//                 let entry = bundle.Entry
//                 let originFile = entry.ObjTargetFile
//                 copyFileInLocker originFile entry.TargetPath      
//                 stack 
//                 |> List.iter (fun fileTree ->
//                     copyFileInLocker originFile fileTree.ObjTargetFile      
//                     copyFileInLocker originFile fileTree.TargetPath      
//                 )
//             Logger.processCompileResult logger (errors,exitCode) 
//         | None -> return (failwithf "Cannot find project file %s" projectFile)   
//     }     

//     let fileMaps (crackedFsProjBundle: CrackedFsprojBundle) =
//         getAllFsprojs crackedFsProjBundle
//         |> List.map (fun proj -> proj.Path,proj.SourceFiles)
//         |> dict

