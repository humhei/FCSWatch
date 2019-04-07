module FcsWatch.Tests.Tests
open Expecto
open FSharp.Compiler.SourceCodeServices
open System.IO
open FcsWatch
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Threading
open FcsWatch.Core.Types
open FcsWatch.Core.FcsWatcher
open FcsWatch.Tests.Types
open Fake.DotNet
open FcsWatch.Core
open FcsWatch.Binary

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
    FcsWatcherMsg.DetectSourceFileChanges <!> (List.map makeFileChange fullPaths)

let makeProjectFileChanges fullPaths =
    FcsWatcherMsg.DetectProjectFileChanges (List.map makeFileChange fullPaths)


let createWatcher (config: BinaryConfig) =
    lazy
        let config =
            { config with 
                WorkingDir = root
                LoggerLevel = Logger.Level.Normal
                WarmCompile = false }

        binaryFcsWatcher config entryProjPath

let testSourceFilesChanged (watcher: Lazy<MailboxProcessor<FcsWatcherMsg>>) sourceFiles expectedCompilerNumber  =
    watcher.Value.PostAndReply (makeSourceFileChanges sourceFiles)
    watcher.Value.PostAndReply FcsWatcherMsg.WaitCompiled
    |> expectCompilerNumber expectedCompilerNumber

let programTests =
    let watcher = createWatcher { BinaryConfig.DefaultValue with DevelopmentTarget = DevelopmentTarget.debuggableProgram }

    testList "program tests" [

        testCase "change file in TestLib2/Library.fs will trigger compiling" <| fun _ ->
            /// TestLib2/Library.fs
            testSourceFilesChanged watcher [testSourceFile1] 1

        testCase "change multiple file in TestLib2 will trigger compiling" <| fun _ ->
            /// (TestLib2/Library.fs + TestLib2/Library2.fs)
            testSourceFilesChanged watcher [testSourceFile1; testSourceFile2] 1

        testCase "change file in TestLib1 and TestLib2 will trigger compiling" <| fun _ ->
            /// (TestLib2/*.fs) + (TestLib1/*.fs) 
            testSourceFilesChanged watcher [testSourceFile1; testSourceFile2; testSourceFile1InTestLib] 2

        testCase "add fs file in fsproj will update watcher" <| fun _ ->
            try
                Fsproj.addFileToProject "Added.fs" testProjPath

                Thread.Sleep(1000)
                testSourceFilesChanged watcher [testSourceFileAdded] 1

            finally
                Fsproj.removeFileFromProject "Added.fs" testProjPath
    ]


let pluginTests =
    let watcher =
        let config =
            let installPlugin() = printfn "install plugin"

            let unInstallPlugin() = printfn "uninstall plugin"

            let plugin: AutoReload.Plugin =
                { Load = installPlugin
                  Unload = unInstallPlugin
                  Calculate = (fun _ ->
                    Thread.Sleep(100)
                    printf "Calculate" )
                  PluginDebugInfo = None }

            {BinaryConfig.DefaultValue with DevelopmentTarget = DevelopmentTarget.autoReloadPlugin plugin }

        createWatcher config


    testList "plugin Tests" [
        testCase "in plugin mode " <| fun _ ->
            /// TestLib2/Library.fs
            testSourceFilesChanged watcher [testSourceFile1] 1
    ]


let functionTests =

    testList "functionTests"
        [
            /// "bin ref may be locked by program
            testCaseAsync "obj ref only" <| async {
                let! fullCracekdFsproj, _  =
                    FullCrackedFsproj.create entryProjPath

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