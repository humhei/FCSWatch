module FcsWatchTests 
open Expecto
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let root = Path.GetDirectoryName(__SOURCE_DIRECTORY__)
let (</>) path1 path2 = Path.Combine(path1,path2) 
type Package = string
type CrackedFsproj =
    { ProjectFile: string
      SourceFiles: string list
      ProjectReferences: string list
      DllReferences: string list
      PackageReferences: Package list
      OtherCompilerOptions: string list }

/// For project references of main project, ignore dll and package references

let tests =
    testList "main tests" [
        testCase "compile full project successly" <| fun _ ->
            let checker = FSharpChecker.Create(keepAssemblyContents = true)
            let projectFile  =  root </> @"TestProject\TestProject.fsproj"
            let projOptions,_,_ = ProjectCoreCracker.getProjectOptionsFromProjectFile projectFile    
            let fscArgs = Array.concat [[|"fsc.exe"|]; projOptions.OtherOptions;[|"--nowin32manifest"|]] 
            let _, exitcode = checker.Compile(fscArgs) |> Async.RunSynchronously
            if exitcode = 0 then pass()
            else fail()
    ]