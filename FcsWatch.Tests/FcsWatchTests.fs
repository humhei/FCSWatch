module FcsWatchTests 
open Expecto
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Atrous.Core.Utils.FakeHelper
open Fake.IO.Globbing.Operators
open Fake.IO
open System
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing
let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let root = Path.GetDirectoryName(__SOURCE_DIRECTORY__)

type Package = string

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
            if relative.StartsWith ".\\bin" then  ".\\obj" + relative.Substring 5
            else failwithf "is not a valid bin relativePath %s" relative
        x.Dir </> objRelative

    member x.SourceFiles = 
        x.ProjOptions.OtherOptions
        |> Array.filter(fun op -> op.EndsWith ".fs")
         
module CrackedFsproj = 
    let create projectFile =
        let projOptions,projRefs,props = ProjectCoreCracker.getProjectOptionsFromProjectFile projectFile 
        {
            ProjOptions = projOptions
            ProjRefs = projRefs
            Props = props 
            Path = projectFile
        }
    let compile (checker: FSharpChecker) (crackedFsProj :CrackedFsproj) =
        
        let baseOptions = crackedFsProj.ProjOptions.OtherOptions |> Array.mapi (fun i op -> if i = 0 then "-o:" + crackedFsProj.ObjTargetFile else op)
        let fscArgs = Array.concat [[|"fsc.exe"|]; baseOptions;[|"--nowin32manifest"|]] 
        checker.Compile(fscArgs) |> Async.RunSynchronously

type CrackedFsProjBundle = 
    {
        Entry: CrackedFsproj
        Refs: CrackedFsProjBundle list
    }

module CrackedFsProjBundle =
    let create projectFile = 
        let rec loop (map: Map<string,CrackedFsproj>) projectFile =
            let entry = 
                match Map.tryFind projectFile map with 
                | Some crackerFsproj -> crackerFsproj
                | None ->
                    let dir = Path.getDirectory projectFile
                    dotnet dir "restore" []
                    dotnet dir "msbuild /target:GenerateBuildDependencyFile" []
                    CrackedFsproj.create projectFile
            let newMap = map.Add (projectFile,entry)
            {
                Entry = entry
                Refs = entry.ProjRefs |> List.map (loop newMap)
            }

        loop Map.empty projectFile     
    let rec private getAllFsprojs (crackedFsProjBundle: CrackedFsProjBundle) =
        [
            yield crackedFsProjBundle.Entry
            yield! List.collect getAllFsprojs crackedFsProjBundle.Refs 
        ] |> List.distinctBy (fun proj -> proj.Path)              

    let compile checker (crackedFsProjBundle: CrackedFsProjBundle) = 
        let ascendingProjs = getAllFsprojs  crackedFsProjBundle
        let rec loop deAscendingProjs = 
            match deAscendingProjs with 
            | h::t -> 
                let errors,exitCode = CrackedFsproj.compile checker h
                if exitCode <> 0 then 
                    errors,exitCode
                else 
                    loop t         
            | [] -> Array.empty,0
        loop (List.rev ascendingProjs)    

    let fileMaps (crackedFsProjBundle: CrackedFsProjBundle) =
        getAllFsprojs crackedFsProjBundle
        |> List.map (fun proj -> proj.Path,proj.SourceFiles)
        |> dict

let projectDir = root </> "TestProject"
let projectFile  =  projectDir </> "TestProject.fsproj"
/// For project references of main project, ignore dll and package references
let tests =
    testList "main tests" [
        ftestCase "compile full project successly" <| fun _ ->
            ["TestProject";"TestLib";"TestLib2"] |> cleanBinAndObj
            let checker = FSharpChecker.Create()
            let bundle = CrackedFsProjBundle.create projectFile
            let errors,exitCode = CrackedFsProjBundle.compile checker bundle
            if exitCode = 0 then pass() else fail()
        
        // testCase "watch mode" <| fun _ ->
        //     dotnet projectDir "build" []
        //     let bundle = CrackedFsProjBundle.create projectFile
        //     let fileMaps = CrackedFsProjBundle.fileMaps bundle
        //     let pattern = 
        //         let files = fileMaps |> Seq.collect (fun pair -> pair.Value) |> List.ofSeq
        //         { BaseDirectory = Path.getFullName "./"
        //           Includes = files
        //           Excludes = [] }
        //     use watcher = pattern |> ChangeWatcher.run (fun changes ->
        //         match List.ofSeq changes with 
        //         | [change] ->
        //             change.FullPath
        //         | _ -> 
        //     )
        //     ()

    ]