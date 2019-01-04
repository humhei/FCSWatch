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
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.Types
open Fake.IO.Globbing
open Fake.IO
open System

let webApp =
    choose [
        route "/emitCompilerTmp"   >=>  emitCompilerTmpHandle
    ]


let app = application {
    url (sprintf "http://0.0.0.0:%d" config.DebuggingServerPort) 
    use_router webApp
}
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