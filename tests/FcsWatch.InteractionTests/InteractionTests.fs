module FcsWatch.InteractionTests.InteractionTests
open Expecto
open Fake.IO
open Fake.IO.FileSystemOperators
open System.IO

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
        testCase "manual reload test" <| fun _ ->
            FcsWatch.Cli.Main.main [|"--project-file"; entryProjPath;"--logger-level"; "normal"; "--debuggable" |]
            |> ignore

        testCase "auto reload test" <| fun _ ->
            FcsWatch.Cli.Main.main [|"--project-file"; entryProjPath|]
            |> ignore

        ftestCase "auto reload for script file test" <| fun _ ->
            let scriptingFile = Path.Combine(__SOURCE_DIRECTORY__ , "Tests.fsx")
            FcsWatch.Cli.Main.main [|"--project-file"; scriptingFile;"--logger-level"; "normal"; "--debuggable" |]
            |> ignore

        testCase "auto reload release" <| fun _ ->
            FcsWatch.Cli.Main.main [|"--project-file"; entryProjPath; "--configuration"; "release"|]
            |> ignore

        testCase "fslive cli test" <| fun _ ->
            FsLive.Driver.main [| entryProjPath; "--watch"; "--loggerlevel:2"; "--send"|]
            |> ignore



    ]