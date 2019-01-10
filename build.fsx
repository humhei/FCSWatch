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

let private easyGetAllProjects (projectFile: string) =
    let values = new HashSet<string>()
    let add projectFile = values.Add projectFile |> ignore
    let rec loop (projectFile: string) = 
        add projectFile

        let dir = Path.getDirectory projectFile
        let doc = new XmlDocument()
        doc.Load(projectFile)
        for node in doc.GetElementsByTagName "ProjectReference" do
            let includeAttr = node.Attributes.GetNamedItem ("Include")
            let includeValue = includeAttr.Value
            let path = Path.getFullName (dir </> includeValue)
            loop path
    loop projectFile 
    Set.ofSeq values       
let projectFile = Path.getFullName "TestProject/TestProject.fsproj"
easyGetAllProjects projectFile
let project = Project.FromFile(projectFile,new ProjectOptions())
let s = project.ProjectCollection

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