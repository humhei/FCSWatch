module FcsWatchTests 
open Expecto
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Atrous.Core.Utils.FakeHelper
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing
open System.Threading
open System.Collections.Generic
open Atrous.Core
open FcsWatch.Types

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let root = Path.GetDirectoryName(__SOURCE_DIRECTORY__)



let projectDir = root </> "TestProject"
let projectFile  =  projectDir </> "TestProject.fsproj"
let tests =
    testList "main tests" [
        testCase "watch mode" <| fun _ ->
            let testFile = Path.getFullName @"TestLib2/Library.fs"
            let config = {Logger = Logger.Normal}
            let manualSet = new ManualResetEventSlim(false)
            dotnet "./" "build" []
            let watcher = new FcsWatcher(config,FSharpChecker.Create(),projectFile)
            /// Modify fs files in TestLib2
            File.append testFile ["\n"]
            manualSet.Wait()
    ]