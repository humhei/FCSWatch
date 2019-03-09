module FsLive.Binary.Tests.Tests
open Expecto
open FSharp.Compiler.SourceCodeServices
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Threading
open FsLive.Core.CrackedFsprojBundle
open FsLive.Core.FsLive
open Fake.DotNet
open FsLive.Core
open FsLive.Binary.Main
open Types
open FsLive.Core.FullCrackedFsproj


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

let expectCompilerNumber excepted (CompilerNumber compilerNumber) =
    Expect.equal excepted compilerNumber (sprintf "expected compiler number %d,while current compiler number is %d" excepted compilerNumber)

let makeFileChange fullPath : FileChange =
    let fullPath = Path.getFullName fullPath

    { FullPath = fullPath
      Name = Path.GetFileName fullPath
      Status = FileStatus.Changed }

let makeSourceFileChanges fullPaths =
    FsLiveMsg.DetectSourceFileChanges <!> List.map makeFileChange fullPaths

let makeProjectFileChanges fullPaths =
    FsLiveMsg.DetectProjectFileChanges <!> List.map makeFileChange fullPaths

let checker = FSharpChecker.Create()

let createWatcher otherFlags developmentTarget buildingConfig entryPath =
    lazy
        let buildingConfig (config: Config) =
            {config with WorkingDir = root; LoggerLevel = Logger.Level.Normal; OtherFlags = otherFlags }
            |> buildingConfig


        fsLive buildingConfig checker entryPath

DotNet.build (fun ops ->
    { ops with
        Configuration = DotNet.BuildConfiguration.Debug }
) entryProjDir


let scriptTests =
    let testScriptFile1 = datas </> @"Scripts/Entry.fsx"

    let watcher = createWatcher [testScriptFile1] DevelopmentTarget.Program id None
    testList "script tests" [

        testCase "change file in Scripts/Entry.fsx will trigger compiling" <| fun _ ->

            watcher.Value.PostAndReply (makeSourceFileChanges [testScriptFile1])
           /// (TestLib2/Library.fs)
            |> expectCompilerNumber 1
    ]


let programTests =
    let watcher = createWatcher [] DevelopmentTarget.Program id (Some entryProjPath)


    testList "program tests" [

        testCase "change file in TestLib2/Library.fs will trigger compiling" <| fun _ ->

            watcher.Value.PostAndReply (makeSourceFileChanges [testSourceFile1])
           /// (TestLib2/Library.fs)
            |> expectCompilerNumber 1

        testCase "change multiple file in TestLib2 will trigger compiling" <| fun _ ->

            watcher.Value.PostAndReply (makeSourceFileChanges [testSourceFile1; testSourceFile2])
            /// (TestLib2/Library.fs + TestLib2/Library2.fs)
            |> expectCompilerNumber 1

        testCase "change file in TestLib1 and TestLib2 will trigger compiling" <| fun _ ->

            watcher.Value.PostAndReply (makeSourceFileChanges [testSourceFile1; testSourceFile2; testSourceFile1InTestLib])
            /// (TestLib2/*.fs) + (TestLib1/*.fs)
            |> expectCompilerNumber 2

        testCase "add fs file in fsproj will update watcher" <| fun _ ->
            try
                Proj.addFileToProject "Added.fs" testProjPath
                /// how to effectively test actor model? While Akka using TestKit
                /// https://doc.akka.io/docs/akka/2.5/testing.html
                Thread.Sleep(1000)

                watcher.Value.PostAndReply (makeSourceFileChanges [testSourceFileAdded])
                /// TestLib2/Added.fs
                |> expectCompilerNumber 1

            finally
                Proj.removeFileFromProject "Added.fs" testProjPath

    ]


let pluginTests =
    let watcher =
        let plugin =
                let installPlugin() = printfn "install plugin"

                let unInstallPlugin() = printfn "uninstall plugin"

                { Load = installPlugin
                  Unload = unInstallPlugin
                  Calculate = (fun _ ->
                  Thread.Sleep(100)
                  printf "Calculate" )
                  DebuggerAttachTimeDelay = 1000 }
                |> DevelopmentTarget.Plugin

        createWatcher [] plugin id (Some entryProjPath)


    testList "plugin Tests" [
        testCase "in plugin mode " <| fun _ ->
            watcher.Value.PostAndReply (makeSourceFileChanges [testSourceFile1])
            /// TestLib2/Library.fs
            |> expectCompilerNumber 1
    ]

let functionTests =

    testList "functionTests"
        [
            /// "bin ref may be locked by program
            testCaseAsync "obj ref only" <| async {
                let! fullCracekdFsproj, _  =
                    FullCrackedFsproj.create checker Config.DeafultValue (Some entryProjPath)

                let otherOptions =
                    fullCracekdFsproj.Value.AsList |> Seq.collect (fun singleTargetCrackedFsproj ->
                        singleTargetCrackedFsproj.FSharpProjectOptions.OtherOptions
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