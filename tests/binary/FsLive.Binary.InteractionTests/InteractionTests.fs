module FsLive.Binary.InteractionTests.InteractionTests
open Expecto
open FSharp.Compiler.SourceCodeServices
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Threading
open FsLive.Core.FullCrackedFsproj
open FsLive.Core
open FsLive.Binary.Main
open Fake.DotNet

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"

let root = __SOURCE_DIRECTORY__ </> "../../../"

let datas = Path.getDirectory(__SOURCE_DIRECTORY__) </> "datas"

let entryProjDir = datas </> "TestProject"

let entryProjPath  = entryProjDir </> "TestProject.fsproj"

let testProjPath = datas </> @"TestLib2/TestLib2.fsproj"

let testSourceFile1 = datas </> @"TestLib2/Library.fs"

let testSourceFile2 = datas </> @"TestLib2/Library2.fs"

let testSourceFileAdded = datas </> @"TestLib2/Added.fs"

let testSourceFile1InTestLib = datas </> @"TestLib1/Library.fs"


let createWatcher buildingConfig =
    let buildingConfig (config: Config) =
        {config with WorkingDir = root; LoggerLevel = Logger.Level.Normal }
        |> buildingConfig

    let checker = FSharpChecker.Create()

    fsLive buildingConfig checker (Some entryProjPath)


DotNet.build (fun ops ->
    { ops with
        Configuration = DotNet.BuildConfiguration.Debug }
) entryProjDir

let interactionTests =
    testCase "base interaction test" <| fun _ ->
        let manualSet = new ManualResetEventSlim()
        let watcher = createWatcher id
        manualSet.Wait()
