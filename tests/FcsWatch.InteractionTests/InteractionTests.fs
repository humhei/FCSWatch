module FcsWatch.InteractionTests.InteractionTests
open Expecto
open Fake.IO
open Fake.IO.FileSystemOperators
open FcsWatch.Types

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"

let root = __SOURCE_DIRECTORY__ </> "../../"

let datas = Path.getDirectory(__SOURCE_DIRECTORY__) </> "datas"

let entryProjDir = datas </> "TestProject"

let entryProjPath  = entryProjDir </> "TestProject.fsproj"

let testProjPath = datas </> @"TestLib2/TestLib2.fsproj"

let testSourceFile1 = datas </> @"TestLib2/Library.fs"

let testSourceFile2 = datas </> @"TestLib2/Library2.fs"

let testSourceFileAdded = datas </> @"TestLib2/Added.fs"

let testSourceFile1InTestLib = datas </> @"TestLib1/Library.fs"


let interactionTests =
    testList "interaction tests" [
        testCase "base interaction test" <| fun _ ->
            FcsWatch.Cli.Main.main [|"--project-file"; entryProjPath|]
            |> ignore

        ftestCase "auto reload test" <| fun _ ->
            FcsWatch.Cli.Main.main [|"--project-file"; entryProjPath; "--no-build"|]
            |> ignore
    ]