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
open Newtonsoft.Json

type CodeTask =
    {
        label: string
        args: string list
    }

type CodeTasks = 
    {
        tasks: CodeTask list
    }
let root = Path.getFullName "./"
let taskJson = root </> ".vscode" </> "tasks.json"
let text = File.readAsString taskJson

let obj = JsonConvert.DeserializeObject<CodeTasks>(text)
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