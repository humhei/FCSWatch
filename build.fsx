#if FAKE
#r "paket: groupref build //"
#endif
#if !FAKE
#r "netstandard" // windows
#endif
#load "./.fake/build.fsx/intellisense.fsx"
open Fake.Core
open Atrous.Core.Utils.FakeHelper.Build
open Fake.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.FakeHelper
open Fake.IO.FileSystemOperators
open Microsoft.Build.Evaluation
open Microsoft.Build.Definition
open System.Collections.Generic
open System.Xml

  

let publisher = lazy (MyPublisher.create(id))


Target.create "MyPublisher.NextBuild" (fun _ ->
    publisher.Value.PublishPackages(VersionStatus.Build)
)

Target.create "MyPublisher.NextRelease" (fun _ ->
    publisher.Value.PublishPackages(VersionStatus.Release)
)

Target.create "MyPublisher.UpdateDependencies" (fun _ ->
    publisher.Value.UpdateDependencies()
)

Target.create "FcsWatch" (fun _ -> 
    let projectFile = Path.getFullName "TestProject/TestProject.fsproj"
    let checker = FSharpChecker.Create()
    runFcsWatcher checker projectFile
)

Target.create "Default" ignore

Target.runOrDefault "Default"