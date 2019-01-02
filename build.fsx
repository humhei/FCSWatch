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

let path = Path.GetRandomFileName()
Target.create "BetaVersionPush" (fun _ ->
    let publisher = new MyBetaPublisher(id)
    publisher.Publish()
)


Target.create "Default" ignore
Target.create "Publish" ignore


"BetaVersionPush"
    ==> "Publish"


Target.runOrDefault "Default"