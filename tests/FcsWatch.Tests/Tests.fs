module FcsWatch.Tests.Tests
open Expecto
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Threading
open FcsWatch.Types
open FcsWatch.FcsWatcher
open FcsWatch.CompilerTmpEmiiter
open FcsWatch.Tests.Types
open Fake.DotNet

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

let expectCompilerNumber excepted (CompilerNumber compilerNumber) =
    Expect.equal excepted compilerNumber (sprintf "expected compiler number %d,while current compiler number is %d" excepted compilerNumber)

let makeFileChange fullPath : FileChange =
    let fullPath = Path.getFullName fullPath

    { FullPath = fullPath
      Name = Path.GetFileName fullPath
      Status = FileStatus.Changed }

let makeSourceFileChanges fullPaths =
    FcsWatcherMsg.DetectSourceFileChanges <!> List.map makeFileChange fullPaths

let makeProjectFileChanges fullPaths =
    FcsWatcherMsg.DetectProjectFileChanges <!> List.map makeFileChange fullPaths


let createWatcher buildingConfig =
    lazy
        let buildingConfig config =
            {config with WorkingDir = root; LoggerLevel = Logger.Level.Normal }
            |> buildingConfig

        let checker = FSharpChecker.Create()

        fcsWatcher buildingConfig checker entryProjPath

DotNet.build (fun ops ->
    { ops with
        Configuration = DotNet.BuildConfiguration.Debug }
) entryProjDir


let programTests =
    let watcher = createWatcher id


    testList "program tests" [

        testCase "change file in TestLib2/Library.fs will trigger compiling" <| fun _ ->
            // Modify fs files in TestLib2

            watcher.Value.PostAndReply (makeSourceFileChanges [testSourceFile1])
           /// (TestLib2/Library.fs)
            |> expectCompilerNumber 1

        testCase "change multiple file in TestLib2 will trigger compiling" <| fun _ ->
            // Modify fs files in TestLib2

            watcher.Value.PostAndReply (makeSourceFileChanges [testSourceFile1; testSourceFile2])
            /// (TestLib2/Library.fs + TestLib2/Library2.fs)
            |> expectCompilerNumber 1

        testCase "change file in TestLib1 and TestLib2 will trigger compiling" <| fun _ ->
            // Modify fs files in TestLib2
            watcher.Value.PostAndReply (makeSourceFileChanges [testSourceFile1; testSourceFile2; testSourceFile1InTestLib])
            /// (TestLib2/*.fs) + (TestLib1/*.fs)
            |> expectCompilerNumber 2

        testCase "add fs file in fsproj will update watcher" <| fun _ ->
            try
                Fsproj.addFileToProject "Added.fs" testProjPath

                Thread.Sleep(1000)

                watcher.Value.PostAndReply (makeSourceFileChanges [testSourceFileAdded])
                /// TestLib2/Added.fs
                |> expectCompilerNumber 1

            finally
                Fsproj.removeFileFromProject "Added.fs" testProjPath

    ]


let pluginTests =
    let watcher =
        let buildConfig =
            fun config ->
                let installPlugin() = printfn "install plugin"

                let unInstallPlugin() = printfn "uninstall plugin"

                let plugin =
                    { Load = installPlugin
                      Unload = unInstallPlugin
                      Calculate = (fun _ ->
                        Thread.Sleep(100)
                        printf "Calculate" )
                      DebuggerAttachTimeDelay = 1000 }

                {config with DevelopmentTarget = DevelopmentTarget.Plugin plugin }

        createWatcher buildConfig


    testList "plugin Tests" [
        testCase "in plugin mode " <| fun _ ->
            // Modify fs files in TestLib2
            watcher.Value.PostAndReply (makeSourceFileChanges [testSourceFile1])
            /// TestLib2/Library.fs
            |> expectCompilerNumber 1
    ]

open FcsWatch.Tests
let functionTests =

    testList "functionTests"
        [
            /// "bin ref may be locked by program
            testCaseAsync "obj ref only" <| async {
                let! fullCracekdFsproj, _  =
                    FullCrackedFsproj.create entryProjPath

                let otherOptions =
                    fullCracekdFsproj.Value.AsList |> Seq.collect (fun crackedFsprojSingleTarget ->
                        crackedFsprojSingleTarget.FSharpProjectOptions.OtherOptions
                    ) |> List.ofSeq

                let p1 =
                    otherOptions
                    |> Seq.exists (fun option ->
                        option.Contains @"\bin\Debug\"
                        || option.Contains @"/bin/Debug/"
                    )
                    |> not

                let p2 =
                    otherOptions
                    |> Seq.exists (fun option ->
                        option.Contains @"\obj\Debug\"
                        || option.Contains @"/obj/Debug/"
                    )

                if p1 && p2 then pass()
                else fail()
            }
        ]