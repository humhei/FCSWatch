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

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let root = Path.getDirectory(__SOURCE_DIRECTORY__)



let projectDir = root </> "TestProject"
let projectFile  =  projectDir </> "TestProject.fsproj"
let tests =
    testList "main tests" [
        testCase "watch mode" <| fun _ ->

            let testFile = root </> @"TestLib2/Library.fs"
            let manualSet = new ManualResetEventSlim(false)
            dotnet root "build" []
            let watcher = 
                let buildConfig = fun config -> {config with WorkingDir = root}
                let checker = FSharpChecker.Create()
                fcsWatcher buildConfig checker projectFile
            /// Modify fs files in TestLib2
            // File.append testFile ["\n"]
            manualSet.Wait()
    ]