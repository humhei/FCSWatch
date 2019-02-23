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

let makeFileChange fullPath : FileChange =
    let fullPath = Path.getFullName fullPath

    { FullPath = fullPath
      Name = Path.GetFileName fullPath
      Status = FileStatus.Changed }

let makeFileChanges fullPaths =
    fullPaths |> List.map makeFileChange |> FcsWatcherMsg.DetectSourceFileChanges


let createWatcher buildingConfig = 

    let buildingConfig config =
        {config with WorkingDir = root; LoggerLevel = Logger.Level.Normal }
        |> buildingConfig

    let checker = FSharpChecker.Create()

    let fcsWatcher =
        fcsWatcher buildingConfig checker entryProjPath

    let testData = createTestData()
    /// consume warm compile testData
    testData.SourceFileManualSet.Wait()

    fcsWatcher

let _testAfterWarmCompile testCase (lazyWatcher: Lazy<MailboxProcessor<FcsWatcherMsg>>) name (test: MailboxProcessor<FcsWatcherMsg> -> TestData -> unit) =
    testCase name (fun _ -> 
        let watcher = lazyWatcher.Force() 

        let testData = createTestData()
        
        test watcher testData)


DotNet.build (fun ops ->
    { ops with 
        Configuration = DotNet.BuildConfiguration.Debug }
) entryProjDir


let programTests =
    let watcher = 
        lazy createWatcher id

    let testAfterWarmCompile name (test: MailboxProcessor<FcsWatcherMsg> -> TestData -> unit) =
        _testAfterWarmCompile testCase watcher name test

    testList "program tests" [

        testAfterWarmCompile "change file in TestLib2/Library.fs will trigger compiling" <| fun watcher testData ->
            // Modify fs files in TestLib2

            watcher.Post (makeFileChanges [testSourceFile1])
            testData.SourceFileManualSet.Wait()

            match testData.AllCompilerNumber with
            /// TestLib2/Library.fs
            | 1 -> pass()
            | _ -> fail()

        testAfterWarmCompile "change multiple file in TestLib2 will trigger compiling" <| fun watcher testData ->
            // Modify fs files in TestLib2

            watcher.Post (makeFileChanges [testSourceFile1; testSourceFile2])

            testData.SourceFileManualSet.Wait()

            match testData.AllCompilerNumber with
            /// (TestLib2/Library.fs + TestLib2/Library2.fs)
            | 1 -> pass()
            | _ -> fail()


        testAfterWarmCompile "change file in TestLib1 and TestLib2 will trigger compiling" <| fun watcher testData ->
            // Modify fs files in TestLib2

            watcher.Post (makeFileChanges [testSourceFile1; testSourceFile2; testSourceFile1InTestLib])

            testData.SourceFileManualSet.Wait()

            match testData.AllCompilerNumber with
            /// (TestLib2/*.fs) + (TestLib1/*.fs)
            | 2 -> pass()
            | _ -> fail()

        testAfterWarmCompile "add fs file in fsproj will update watcher" <| fun watcher testData ->
            try

                Fsproj.addFileToProject "Added.fs" testProjPath

                testData.ProjectFileManualSet.Wait()

                watcher.Post (makeFileChanges [testSourceFileAdded])

                testData.SourceFileManualSet.Wait()

                match testData.AllCompilerNumber with
                /// TestLib2/Added.fs
                | 1 -> pass()
                | _ -> fail()

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

                {config with WorkingDir = root; LoggerLevel = Logger.Level.Normal; DevelopmentTarget = DevelopmentTarget.Plugin plugin }

        lazy createWatcher buildConfig


    let testAfterWarmCompile name (test: MailboxProcessor<FcsWatcherMsg> -> TestData -> unit) =
        _testAfterWarmCompile testCase watcher name test

    let ftestAfterWarmCompile name (test: MailboxProcessor<FcsWatcherMsg> -> TestData -> unit) =
        _testAfterWarmCompile ftestCase watcher name test

    testList "plugin Tests" [
        ftestAfterWarmCompile "in plugin mode " <| fun watcher testData ->
            // Modify fs files in TestLib2

            watcher.Post (makeFileChanges [testSourceFile1])

            testData.SourceFileManualSet.Wait()

            match testData.AllCompilerNumber with
            /// TestLib2/Library.fs
            | 1 -> pass()
            | _ -> fail()
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
                    )
                    |> not

                let p2 =
                    otherOptions
                    |> Seq.exists (fun option ->
                        option.Contains @"\obj\Debug\"
                    )

                if p1 && p2 then pass()
                else fail()
            }
        ]