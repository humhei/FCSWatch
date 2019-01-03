#if FAKE
#r "paket: groupref build //"
#endif
#if !FAKE
#r "netstandard" // windows
#endif
#load "./.fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.Core.TargetOperators
open Atrous.Core.Utils.FakeHelper.Build
open Atrous.Core.Utils.FakeHelper
open Atrous.Core
open System.IO
open Fake.IO.Globbing.Operators
open Fake.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.FakeHelper


let publisher = new MyBetaPublisher(id)


Target.create "BetaVersion.Publish" (fun _ ->
    publisher.Publish()
)

Target.create "BetaVersion.UpdateDependencies" (fun _ ->
    publisher.UpdateDependencies()
)

Target.create "FcsWatch" (fun _ -> 
    let projectFile = Path.getFullName "TestProject/TestProject.fsproj"
    let checker = FSharpChecker.Create()
    runFcsWatcher checker projectFile
)

Target.create "Default" ignore

Target.runOrDefault "Default"