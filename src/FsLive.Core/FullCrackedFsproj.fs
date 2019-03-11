namespace FsLive.Core
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open System.Collections.Generic
open System.Xml
open FsLive.Core.CrackedFsproj
open FSharp.Compiler.SourceCodeServices

[<AutoOpen>]
module internal Global =
    let mutable logger = Logger.create (Logger.Level.Minimal)

type Config =
    { LoggerLevel: Logger.Level
      OtherFlags: string list
      Eval: bool
      WarmCompile: bool
      UseEditFiles: bool
      WorkingDir: string
      BuildingFSharpProjectOptions: FSharpProjectOptions -> FSharpProjectOptions }

with 
    static member DeafultValue = 
        { LoggerLevel = Logger.Level.Minimal
          Eval = false
          WarmCompile = false
          OtherFlags = []
          UseEditFiles = false
          WorkingDir = Directory.GetCurrentDirectory()
          BuildingFSharpProjectOptions = id }

[<AutoOpen>]
module Extensions =

    [<RequireQualifiedAccess>]
    module internal Dictionary =

        let toMap dictionary =
            (dictionary :> seq<_>)
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq

    [<RequireQualifiedAccess>]
    module FileSystem =
        let editDirAndFile (fileName: string) (config: Config) =
            assert config.UseEditFiles
            let infoDir = Path.Combine(Path.GetDirectoryName fileName,".fsharp")
            let editFile = Path.Combine(infoDir,Path.GetFileName fileName + ".edit")
            if not (Directory.Exists infoDir) then 
                Directory.CreateDirectory infoDir |> ignore
            infoDir, editFile

        let readFile (fileName: string) (config: Config) = 
            if config.UseEditFiles then 
                let infoDir, editFile = editDirAndFile fileName config
                let preferEditFile =
                    try 
                        Directory.Exists infoDir && File.Exists editFile && File.Exists fileName && File.GetLastWriteTime(editFile) > File.GetLastWriteTime(fileName)
                    with _ -> 
                        false
                if preferEditFile then 
                    logger.Info "*** preferring %s to %s ***" editFile fileName
                    File.ReadAllText editFile
                else
                    File.ReadAllText fileName
            else
                File.ReadAllText fileName


    type Logger.Logger with
        member x.CopyFile src dest =
            File.Copy(src,dest,true)
            logger.Info "%s ->\n%s" src dest

    [<RequireQualifiedAccess>]
    module CompileResult =

        let summary (elapsed: int64) (compileResult: CompileResult) =
            sprintf "Summary:
    -- origin: %s
    -- dest： %s
    -- elapsed: %d milliseconds"
                compileResult.ProjPath compileResult.Dll elapsed

    [<RequireQualifiedAccess>]
    module CompileOrCheckResult =
        let processResult (compileOrCheckResult: CompileOrCheckResult) =
            let errors = CompileOrCheckResult.errors compileOrCheckResult
    
            if CompileOrCheckResult.isSuccess compileOrCheckResult then 
                if not <| Array.isEmpty errors then
                    logger.Warn "WARNINGS:\n%A" errors

            else logger.Error "ERRORS:\n%A" errors

        let summary elased = function 
            | CompileOrCheckResult.CompileResult compileResult ->
                CompileResult.summary elased compileResult
                |> logger.Important "%s"

            | CompileOrCheckResult.CheckResult checkResult ->
        
                ()

    [<RequireQualifiedAccess>]
    module SingleTargetCrackedFsproj =

        /// copy ref dll from proj other options  
        /// e.g.
        /// arg-projectFile is D:\VsCode\Github\FCSWatch\tests\binary\datas\TestLib2.fsproj
        /// otherOptions contains :r:D:\VsCode\Github\FCSWatch\tests\binary\datas\TestLib2\obj\Debug\netstandard2.0\TestLib2.dll
        let copyFileFromRefDllToBin originProjectFile (destCrackedFsprojSingleTarget: SingleTargetCrackedFsproj) =

            let targetDir = destCrackedFsprojSingleTarget.TargetDir

            let originDll =
                let projName = Path.GetFileNameWithoutExtension originProjectFile

                destCrackedFsprojSingleTarget.RefDlls
                |> Array.find(fun refDll -> Path.GetFileNameWithoutExtension refDll = projName)

            let fileName = Path.GetFileName originDll

            let destDll = targetDir </> fileName

            logger.CopyFile originDll destDll

            let originPdb = originDll |> Path.changeExtension ".pdb"

            let destPdb = targetDir </> (Path.changeExtension ".pdb" fileName)

            logger.CopyFile originPdb destPdb

        let copyObjToBin (singleTargetCrackedFsproj: SingleTargetCrackedFsproj) =
            logger.CopyFile singleTargetCrackedFsproj.ObjTargetFile singleTargetCrackedFsproj.TargetPath
            logger.CopyFile singleTargetCrackedFsproj.ObjTargetPdb singleTargetCrackedFsproj.TargetPdbPath



module FullCrackedFsproj =


    [<RequireQualifiedAccess>]
    module CrackedFsproj =
    
        /// copy ref dll from proj other options  e.g.
        /// arg-projectFile is D:\VsCode\Github\FCSWatch\tests\binary\datas\TestLib2.fsproj
        /// otherOptions contains :r:D:\VsCode\Github\FCSWatch\tests\binary\datas\TestLib2\obj\Debug\netstandard2.0\TestLib2.dll
        let copyFileFromRefDllToBin projectFile (destCrackedFsproj: CrackedFsproj) =
            destCrackedFsproj.AsList
            |> List.iter (SingleTargetCrackedFsproj.copyFileFromRefDllToBin projectFile)

        let copyObjToBin (crackedFsproj: CrackedFsproj) =
            crackedFsproj.AsList |> List.iter SingleTargetCrackedFsproj.copyObjToBin

    type FullCrackedFsproj =
        { Value: CrackedFsproj
          Refs: FullCrackedFsproj list }


    [<RequireQualifiedAccess>]
    module FullCrackedFsproj =
        let easyGetAllProjPaths (entryProjPath: string) =
            assert (entryProjPath.EndsWith ".fsproj")
            let values = new HashSet<string>()
            let add projectFile = values.Add projectFile |> ignore
            let rec loop (projectFile: string) =
                let normarlizedPath = Path.nomarlizeToUnixCompatible projectFile
                add normarlizedPath

                let dir = Path.getDirectory projectFile
                let doc = new XmlDocument()
                doc.Load(normarlizedPath)

                for node in doc.GetElementsByTagName "ProjectReference" do
                    let includeAttr = node.Attributes.GetNamedItem ("Include")
                    let includeValue = includeAttr.Value

                    let path = dir </> includeValue

                    loop path

            loop entryProjPath
            Set.ofSeq values

       ///async works may fail due to fetch proj options is not thread safe
       /// retry many times will solve it
        let private fetchUnsafeDataAsync maxRetryCount taskInterval task prediate taskResultToTaskArgMapping allTaskArgs = async {
            let rec loop retryCount accum allTaskArgs =
                logger.Info "try fetch unsafe thread datas at %d time" retryCount

                if retryCount > maxRetryCount then failwith "exceed max retry times"

                if Seq.isEmpty allTaskArgs then accum
                else
                    let allTaskResults =
                        allTaskArgs
                        |> Seq.mapi (fun i project -> async {
                            /// Set time delay to reduce the mistake times
                            do! Async.Sleep (taskInterval * i)
                            return! task project
                        }
                        )
                        |> Async.Parallel
                        |> Async.RunSynchronously

                    let success,unsuccess = allTaskResults |> Array.partition prediate

                    let newAccum = Array.append accum success

                    loop (retryCount + 1) newAccum (unsuccess |> Array.map taskResultToTaskArgMapping)

            return loop 1 [||] allTaskArgs
        }

        let getAllCrackedFsprojs entryProjPath =
            let prediate (crackedFsproj: CrackedFsproj) =
                crackedFsproj.AsList
                |> List.exists (fun singleTargetCrackedFsproj ->
                    singleTargetCrackedFsproj.FSharpProjectOptions.OtherOptions.Length <> 0
                )
            let allProjects = easyGetAllProjPaths entryProjPath

            fetchUnsafeDataAsync
                100
                50
                CrackedFsproj.create
                prediate
                (fun crackedFsproj -> crackedFsproj.ProjPath) (Array.ofSeq allProjects)


        let rec mapValue mapping (fullCrackedFsproj: FullCrackedFsproj) =
            { fullCrackedFsproj 
                with 
                    Value = mapping fullCrackedFsproj.Value 
                    Refs = fullCrackedFsproj.Refs |> List.map (mapValue mapping) }

        /// entry level is 0
        let getLevel projectFile (entryFsproj: FullCrackedFsproj) =
            let rec loop level (fsproj: FullCrackedFsproj) =
                [
                    if fsproj.Value.ProjPath = projectFile
                    then yield! [level]
                    yield! fsproj.Refs |> List.collect (loop (level + 1))
                ]

            loop 0 entryFsproj
            |> List.max


        let getProjectRefersMap (entryFsproj: FullCrackedFsproj) =
            let cacheMutable = new Dictionary<string, CrackedFsproj list>()
            let rec loop (stack: CrackedFsproj list) (fsproj: FullCrackedFsproj) =
                match cacheMutable.TryGetValue fsproj.Value.ProjPath with
                | true,stack2 ->
                    let newStack = stack @ stack2
                    cacheMutable.[fsproj.Value.ProjPath] <- newStack
                | false,_ ->
                    cacheMutable.Add(fsproj.Value.ProjPath, stack)

                    let newStack = fsproj.Value :: stack

                    fsproj.Refs |> List.iter (loop newStack)

            loop [] entryFsproj
            cacheMutable
            |> Dictionary.toMap
            |> Map.map (fun proj crackedFsprojs ->
                crackedFsprojs
                |> List.distinctBy (fun crackedFsproj -> crackedFsproj.ProjPath)
            )


        /// <param name="entryFileOp">file end with *.fs;*.fsproj;*.fsx;*.fsi; If None then config.Otherflags must be not empty</param>

        let create (checker: FSharpChecker) (config: Config) (entryFileOp: string option) = async {
            let otherFlags = config.OtherFlags

            let ofScript (scriptFile: string) fsharpProjectOptions =
                assert (
                    (scriptFile.EndsWith ".fs" || scriptFile.EndsWith ".fsx" || scriptFile.EndsWith ".fsi")
                )

                let outputDll = 
                    let fileName = Path.GetFileNameWithoutExtension scriptFile
                    let dir = Path.getDirectory scriptFile
                    dir </> "bin/Debug/noframework" </> fileName + ".dll"
                    |> Path.nomarlizeToUnixCompatible

                let props = 
                    [ "TargetPath", outputDll
                      "TargetFramework", "noframework"]
                    |> Map.ofList

                let fsharpProjectOptions = config.BuildingFSharpProjectOptions fsharpProjectOptions

                let project = 
                    { Value = 
                        { FSharpProjectOptions = fsharpProjectOptions
                          ProjRefs = []
                          Props = props
                          ProjPath = fsharpProjectOptions.ProjectFileName }
                        |> List.singleton
                        |> CrackedFsproj.CrackedFsproj
                      Refs = [] }

                logger.Infots "End crack project"
                ( project, Map.ofList [fsharpProjectOptions.ProjectFileName, project.Value] )
                    
            logger.Infots "Begin crack project"
            match entryFileOp with 
            | Some file ->
                match file with 
                | projectFile when projectFile.EndsWith ".fsproj" ->

                    let projectMapsMutable = Dictionary<string,CrackedFsproj>()
                    let! allCrackedFsprojs = getAllCrackedFsprojs projectFile

                    let applyOtherFlagsToEntryProject (allCrackedFsprojs: seq<CrackedFsproj> ) =
                        allCrackedFsprojs |> Seq.map (fun crackedFsproj ->
                            if crackedFsproj.ProjPath = projectFile then 
                                CrackedFsproj.mapProjOptions (fun ops -> 
                                    { ops with 
                                        OtherOptions = [| yield! ops.OtherOptions; yield! otherFlags |] }
                                ) crackedFsproj
                            else crackedFsproj
                        )

                    allCrackedFsprojs 
                    |> CrackedFsproj.mapProjOtherOptionsObjRefOnly 
                    |> Seq.map CrackedFsproj.mapProjOtherOptionsDebuggable
                    |> Seq.map (CrackedFsproj.mapProjOptions config.BuildingFSharpProjectOptions)
                    |> applyOtherFlagsToEntryProject 
                    |> Seq.iter (fun crakedFsproj -> projectMapsMutable.Add (crakedFsproj.ProjPath,crakedFsproj))

                    let rec loop projectFile =
                        let projectInfo = projectMapsMutable.[projectFile]
                        { Value = projectInfo
                          Refs = projectInfo.ProjRefs |> List.map loop }

                    let project = loop file

                    let projectMap =
                        projectMapsMutable
                        |> Dictionary.toMap

                    logger.Infots "End crack project"
                    return (project, projectMap)

                | scriptFile when file.EndsWith ".fs" || file.EndsWith ".fsx" || file.EndsWith ".fsi" ->
                    let fsharpProjectOptions = ProjectCoreCracker.getProjectOptionsFromScript checker scriptFile
                    return ofScript scriptFile fsharpProjectOptions

                | _ -> return failwithf "no project file found, no compilation arguments given and no project file found in %s" config.WorkingDir

            | None ->
                let sourceFiles, otherFlags2 = otherFlags |> List.partition (fun arg -> arg.EndsWith(".fs") || arg.EndsWith(".fsi") || arg.EndsWith(".fsx"))
                let otherFlags2 = [| yield! otherFlags; yield! otherFlags2 |]
                let sourceFiles = sourceFiles |> List.map Path.nomarlizeToUnixCompatible

                match sourceFiles with 
                | [ script ] when not config.Eval ->
                    let text = FileSystem.readFile script config
                    let fsharpProjectOptions, errors = checker.GetProjectOptionsFromScript(script, text, otherFlags=otherFlags2) |> Async.RunSynchronously
                    if errors.Length > 0 then 
                        failwithf "ERRORS: %A" errors
                        
                    return ofScript script fsharpProjectOptions 

                | _ when config.Eval -> 
                    let fsproj = Path.GetTempFileName() |> Path.changeExtension ".fsproj"
                    let fsharpProjectOptions = checker.GetProjectOptionsFromCommandLineArgs(fsproj, otherFlags2)

                    let fsharpProjectOptions = 
                        { fsharpProjectOptions with SourceFiles = Array.ofList sourceFiles }
                        |> config.BuildingFSharpProjectOptions

                    let props = 
                        let outputDll = 
                            let fileName = Path.GetFileNameWithoutExtension fsproj
                            let dir = Path.getDirectory fsproj
                            dir </> "bin/Debug/noframework" </> fileName + ".dll"
                            |> Path.nomarlizeToUnixCompatible

                        [ "TargetPath", outputDll
                          "TargetFramework", "noframework"]
                        |> Map.ofList

                    let project = 
                        { Value = 
                            { FSharpProjectOptions = fsharpProjectOptions
                              ProjRefs = []
                              Props = props
                              ProjPath = fsharpProjectOptions.ProjectFileName }
                            |> List.singleton
                            |> CrackedFsproj.CrackedFsproj
                          Refs = [] }

                    logger.Infots "End crack project"

                    return (project, Map.ofList [fsharpProjectOptions.ProjectFileName, project.Value])
                | _ -> return failwithf "Unexpected fsharp options %A" otherFlags
        }

