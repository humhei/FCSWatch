module FcsWatchTests 
open Expecto
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Atrous.Core.Utils.FakeHelper
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Threading
open FcsWatch.Types
open FcsWatch.FcsWatcher
open FcsWatch.Tests.Types
open FcsWatch.CompilerTmpEmiiter
open Atrous.Core

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let root = Path.getDirectory(__SOURCE_DIRECTORY__)



let projectDir = root </> "TestProject"
let projectFile  =  projectDir </> "TestProject.fsproj"

let makeFileChange fullPath : FileChange =
    let fullPath = Path.getFullName fullPath
    { FullPath = fullPath
      Name = Path.GetFileName fullPath
      Status = FileStatus.Changed }

type TestModel =
    { Watcher: MailboxProcessor<FcsWatcherMsg> 
      ProjectFile: string
      FileChange: FileChange
      GetCompilerTmp: unit -> string list }

let inTest buildingConfig f =
    let testProject = root </> @"TestLib2/TestLib2.fsproj"

    let testFile = root </> @"TestLib2/Library.fs"
    dotnet root "build" []
    let watcher = 
        let buildConfig = 
            fun config -> 
                {config with WorkingDir = root; Logger = Logger.Normal} 
                |> buildingConfig

        let checker = FSharpChecker.Create()
        fcsWatcher buildConfig checker projectFile
    let fileChange = makeFileChange testFile 
    let tmpEmitterAgent = watcher.PostAndReply FcsWatcherMsg.GetEmitterAgent
    let getCompilerTmp () = 
        tmpEmitterAgent.PostAndReply CompilerTmpEmitterMsg.GetCompilerTmp
        |> List.ofSeq
    f { Watcher = watcher; ProjectFile = testProject; FileChange = fileChange; GetCompilerTmp = getCompilerTmp }

let tests =
    testList "watch mode" [
        testCase "change file in TestLib2/Library.fs will trigger compiling" <| fun _ ->
            // Modify fs files in TestLib2
            inTest id (fun model ->
                let allCompilerTaskNumber = model.Watcher.PostAndReply (FcsWatcherMsg.DetectSourceFileChange <!> model.FileChange)
                match allCompilerTaskNumber,model.GetCompilerTmp() with 
                /// warmCompile + TestLib2/Library.fs
                | 2,[a] -> pass()
                | _ -> fail()    
            ) 

        testCase "add fs file in fsproj will update watcher" <| fun _ ->
            inTest id (fun model ->
                try 
                    Fsproj.addFileToProject "Added.fs" model.ProjectFile
                    model.Watcher.Post(FcsWatcherMsg.DetectProjectFileChange (makeFileChange model.ProjectFile))

                    let fileChange = makeFileChange (root </> "TestLib2" </> "Added.fs") 
                    let allCompilerTaskNumber = model.Watcher.PostAndReply (FcsWatcherMsg.DetectSourceFileChange <!> fileChange)
                    match allCompilerTaskNumber,model.GetCompilerTmp() with 
                    /// warmCompile + TestLib2/Added.fs
                    | 2,[a] -> pass()
                    | _ -> fail()  

                finally
                    Fsproj.removeFileFromProject "Added.fs" model.ProjectFile
            )

        testCase "in at once mode emit cache everytime when file change is deteched" <| fun _ ->
            let installPlugin() = printfn "install plugin" 
            let unInstallPlugin() = printfn "uninstall plugin" 
            let atOnceMode config = { config with DevelopmentTarget = DevelopmentTarget.AtOnce(installPlugin,unInstallPlugin) } 
            inTest atOnceMode (fun model ->
                let allCompilerTaskNumber = model.Watcher.PostAndReply (FcsWatcherMsg.DetectSourceFileChange <!> model.FileChange)
                let tmps = model.GetCompilerTmp()
                match allCompilerTaskNumber, tmps with 
                /// warmCompile + TestLib2/Library.fs
                /// all compiler tmp is emitted as atOnceMode
                | 2, [] -> pass()
                | _ -> fail()    
            )
        ftestCase "in plugin mode " <| fun _ ->
            let installPlugin() = printfn "install plugin" 
            let unInstallPlugin() = printfn "uninstall plugin" 
            let plugin = 
                { Load = installPlugin 
                  Unload = unInstallPlugin
                  Calculate = (fun _ -> Thread.Sleep(100))
                  DebuggerAttachTimeDelay = 1000 }
            let pluginMode config = { config with DevelopmentTarget = DevelopmentTarget.Plugin plugin } 
            inTest pluginMode (fun model ->
                let allCompilerTaskNumber = model.Watcher.PostAndReply (FcsWatcherMsg.DetectSourceFileChange <!> model.FileChange)
                let tmps = model.GetCompilerTmp()
                match allCompilerTaskNumber, tmps with 
                /// warmCompile + TestLib2/Library.fs
                /// all compiler tmp is emitted as atOnceMode
                | 2, [] -> pass()
                | _ -> fail()    
            )
            
    ]