module FcsWatch.Tests.Tests
open Expecto
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Threading
open FcsWatch.Core.Types
open FcsWatch.Core.FcsWatcher
open FcsWatch.Tests.Types
open FcsWatch.Core
open FcsWatch.Binary
open Suave
open Suave.Operators
open Suave.Filters

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
                LoggerLevel = Logger.Level.Debug
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
                  TimeDelayAfterUninstallPlugin = 500
                  PluginDebugInfo = None }

            {BinaryConfig.DefaultValue with DevelopmentTarget = DevelopmentTarget.autoReloadPlugin plugin }

        createWatcher config


    testList "plugin Tests" [
        testCase "in plugin mode " <| fun _ ->
            /// TestLib2/Library.fs
            testSourceFilesChanged watcher [testSourceFile1] 1
    ]


let webhookTests =
    let watcher =
        createWatcher { 
            BinaryConfig.DefaultValue with 
                DevelopmentTarget = DevelopmentTarget.autoReloadProgram
                Webhook = Some "http://localhost:9867/update" }

    testList "webhook Tests" [
        testCase "can send webhook and recieve it " <| fun _ ->
            let cts = new CancellationTokenSource() 
            let suaveConfig = 
                { defaultConfig with
                    bindings   = [ HttpBinding.createSimple HTTP "127.0.0.1" 9867 ]
                    bufferSize = 2048
                    cancellationToken = cts.Token }
            
            let mutable recieveAccount = 0

            let webApp =
                choose 
                    [ path "/update" >=> 
                        (fun ctx ->
                            recieveAccount <- recieveAccount + 1
                            Successful.OK "recieved web hook" ctx
                        )
                    ]

            let listening, server = startWebServerAsync suaveConfig webApp
            Async.Start server 

            /// TestLib2/Library.fs
            testSourceFilesChanged watcher [testSourceFile1] 1

            let cache = watcher.Value.PostAndReply FcsWatcherMsg.GetCache

            tryKill AutoReload.DevelopmentTarget.Program cache.EntryCrackedFsproj 
            /// run + rerun
            Expect.equal 2 recieveAccount "can send webhook and recieve it"

    ]

let functionTests =

    testList "functionTests"
        [
            /// "bin ref may be locked by program
            testCaseAsync "obj ref only" <| async {
                let! fullCracekdFsproj, _  =
                    FullCrackedFsproj.create (FullCrackedFsprojBuilder.Project {OtherFlags = [||]; File = entryProjPath})

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

            testCase "EasyGetAllFsProjects for complex projects" <| fun _ ->
                /// no exception should be threw in unix
                /// https://github.com/humhei/FCSWatch/issues/19
                FullCrackedFsproj.easyGetAllProjPaths (datas </> "repro-projects\src\Masse.API\Masse.API.fsproj")
                |> ignore
                
        ]