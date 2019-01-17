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
      FileChange: FileChange }

let inTest f =
    let testProject = root </> @"TestLib2/TestLib2.fsproj"

    let testFile = root </> @"TestLib2/Library.fs"
    let manualSet = new ManualResetEventSlim(false)
    dotnet root "build" []
    let watcher = 
        let buildConfig = fun config -> {config with WorkingDir = root}
        let checker = FSharpChecker.Create()
        fcsWatcher buildConfig checker projectFile
        
    let fileChange = makeFileChange testFile 
    f { Watcher = watcher; ProjectFile = testProject; FileChange = fileChange}
    manualSet.Wait()

let tests =
    testList "watch mode" [
        testCase "change file in TestLib2/Library.fs will trigger compiling" <| fun _ ->
            // Modify fs files in TestLib2
            inTest (fun model ->
                let allCompilerTaskNumber = model.Watcher.PostAndReply (FcsWatcherMsg.DetectSourceFileChange <!> model.FileChange)
                match allCompilerTaskNumber with 
                /// warmCompile + TestLib2/Library.fs
                | 2 -> pass()
                | _ -> fail()    
            ) 

        testCase "add fs file in fsproj will update watcher" <| fun _ ->
            inTest (fun model ->
                try 
                    Fsproj.addFileToProject "Added.fs" model.ProjectFile
                    model.Watcher.Post(FcsWatcherMsg.DetectProjectFileChange (makeFileChange model.ProjectFile))

                    let fileChange = makeFileChange (root </> "TestLib2" </> "Added.fs") 
                    let allCompilerTaskNumber = model.Watcher.PostAndReply (FcsWatcherMsg.DetectSourceFileChange <!> fileChange)
                    match allCompilerTaskNumber with 
                    /// warmCompile + TestLib2/Added.fs
                    | 2 -> pass()
                    | _ -> fail() 

                finally
                    Fsproj.removeFileFromProject "Added.fs" model.ProjectFile
            )
            
    ]