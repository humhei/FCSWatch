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
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open FcsWatch.FakeHelper
let root = Path.getFullName "./"
let projectFile =  Path.getFullName "./TestProject/TestProject.fsproj"
let path = Path.GetRandomFileName()
Target.create "BetaVersionPush" (fun _ ->
    let publisher = new MyBetaPublisher(id)
    publisher.Publish()
)

// Target.create "RunFcsWatcher" (fun _ -> 
//     runWatcher(FSharpChecker.Create(),projectFile,root)
// )

Target.create "Default" ignore
Target.create "Publish" ignore


"BetaVersionPush"
    ==> "Publish"


Target.runOrDefault "Default"