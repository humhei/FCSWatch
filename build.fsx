#r "paket:
nuget Atrous.Core.Utils prerelease
nuget Fake.Core.Target
nuget FcsWatch prerelease//"

#load "./.fake/build.fsx/intellisense.fsx"

// start build
open Fake.Core
open Fake.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.FakeHelper


Target.create "FcsWatch" (fun _ -> 
    let projectFile = Path.getFullName "TestProject/TestProject.fsproj"
    let checker = FSharpChecker.Create()
    runFcsWatcher checker projectFile
)

Target.create "Default" ignore

Target.runOrDefault "Default"