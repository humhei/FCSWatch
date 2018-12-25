module FcsWatchTests 
open Expecto
open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch.Tests.ProjectCoreCracker

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
        ftestCase "compile project" <| fun _ ->
            let compileArgWithDebuggerInfo dll script =
                [| "fsc.exe"; "-o"; dll; "-a"; script;"--optimize-";"--debug"; |]
                
            let checker = FSharpChecker.Create()
            let projectFile  = @"D:\VsCode\Github\FCSWatch\TestProject\TestProject.fsproj"
            let projOptions = get
            let dll = @"D:\VsCode\Github\FCSWatch\TestLib\bin\Debug\netstandard2.0\TestLib.dll"
            let result = checker.Compile(compileArgWithDebuggerInfo dll projectFile) |> Async.RunSynchronously
            printfn "Hello World from F#!"

            printf ""


    ]