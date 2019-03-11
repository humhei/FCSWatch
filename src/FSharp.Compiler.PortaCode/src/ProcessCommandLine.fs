// Copyright 2018 Fabulous contributors. See LICENSE.md for license.

// F# PortaCode command processing (e.g. used by Fabulous.Cli)

module FSharp.Compiler.PortaCode.ProcessCommandLine

open FSharp.Compiler.PortaCode.CodeModel
open FSharp.Compiler.PortaCode.Interpreter
open FSharp.Compiler.PortaCode.FromCompilerService
open System
open System.Collections.Generic
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Net
open System.Text


let checker = FSharpChecker.Create(keepAssemblyContents = true)

let ProcessCommandLine (argv: string[]) =
    let mutable fsproj = None
    let mutable eval = false
    let mutable livechecksonly = false
    let mutable watch = false
    let mutable useEditFiles = false
    let mutable writeinfo = false
    let mutable webhook = None
    let mutable otherFlags = []
    let defaultUrl = "http://localhost:9867/update"
    let fsharpArgs = 
        let mutable haveDashes = false

        [| for arg in argv do 
                let arg = arg.Trim()
                if arg.StartsWith("@") then 
                    for line in File.ReadAllLines(arg.[1..]) do 
                        let line = line.Trim()
                        if not (String.IsNullOrWhiteSpace(line)) then
                            yield line
                elif arg.EndsWith(".fsproj") then 
                    fsproj <- Some arg
                elif arg = "--" then haveDashes <- true
                elif arg.StartsWith "--define:" then otherFlags <- otherFlags @ [ arg ]
                elif arg = "--watch" then watch <- true
                elif arg = "--eval" then eval <- true
                elif arg = "--livechecksonly" then livechecksonly <- true
                elif arg = "--writeinfo" then writeinfo <- true
                elif arg = "--vshack" then useEditFiles <- true
                elif arg.StartsWith "--webhook:" then webhook  <- Some arg.["--webhook:".Length ..]
                elif arg = "--send" then webhook  <- Some defaultUrl
                elif arg = "--version" then 
                   printfn ""
                   printfn "*** NOTE: if sending the code to a device the versions of CodeModel.fs and Interpreter.fs on the device must match ***"
                   printfn ""
                   printfn "CLI tool assembly version: %A" (System.Reflection.Assembly.GetExecutingAssembly().GetName().Version)
                   printfn "CLI tool name: %s" (System.Reflection.Assembly.GetExecutingAssembly().GetName().Name)
                   printfn ""
                elif arg = "--help" then 
                   printfn "Command line tool for watching and interpreting F# projects"
                   printfn ""
                   printfn "Usage: <tool> arg .. arg [-- <other-args>]"
                   printfn "       <tool> @args.rsp  [-- <other-args>]"
                   printfn "       <tool> ... Project.fsproj ... [-- <other-args>]"
                   printfn ""
                   printfn "The default source is a single project file in the current directory."
                   printfn "The default output is a JSON dump of the PortaCode."
                   printfn ""
                   printfn "Arguments:"
                   printfn "   --watch           Watch the source files of the project for changes"
                   printfn "   --webhook:<url>   Send the JSON-encoded contents of the PortaCode to the webhook"
                   printfn "   --send            Equivalent to --webhook:%s" defaultUrl
                   printfn "   --eval            Evaluate the contents using the interpreter after each update"
                   printfn "   --livechecksonly  (Experimental) Only evaluate declarations with a LiveCheck attribute"
                   printfn "                     This uses on-demand execution semantics for top-level declarations"
                   printfn "   --writeinfo       (Experimental) Write an info file based on results of evaluation"
                   printfn "   --vshack          (Experimental) Watch for .fsharp/foo.fsx.edit files and use the contents of those"
                   printfn "   <other-args>      All other args are assumed to be extra F# command line arguments"
                   exit 1
                else yield arg  |]

    if fsharpArgs.Length = 0 && fsproj.IsNone then 
        match Seq.toList (Directory.EnumerateFiles(Environment.CurrentDirectory, "*.fsproj")) with 
        | [ ] -> 
            failwithf "no project file found, no compilation arguments given and no project file found in \"%s\"" Environment.CurrentDirectory 
        | [ file ] -> 
            printfn "fscd: using implicit project file '%s'" file
            fsproj <- Some file
        | file1 :: file2 :: _ -> 
            failwithf "multiple project files found, e.g. %s and %s" file1 file2 

    let editDirAndFile (fileName: string) =
        assert useEditFiles
        let infoDir = Path.Combine(Path.GetDirectoryName fileName,".fsharp")
        let editFile = Path.Combine(infoDir,Path.GetFileName fileName + ".edit")
        if not (Directory.Exists infoDir) then 
            Directory.CreateDirectory infoDir |> ignore
        infoDir, editFile

    let readFile (fileName: string) = 
        if useEditFiles && watch then 
            let infoDir, editFile = editDirAndFile fileName
            let preferEditFile =
                try 
                    Directory.Exists infoDir && File.Exists editFile && File.Exists fileName && File.GetLastWriteTime(editFile) > File.GetLastWriteTime(fileName)
                with _ -> 
                    false
            if preferEditFile then 
                printfn "*** preferring %s to %s ***" editFile fileName
                File.ReadAllText editFile
            else
                File.ReadAllText fileName
        else
            File.ReadAllText fileName

    let options = 
        match fsproj with 
        | Some fsprojFile -> 
            if fsharpArgs.Length > 1 then failwith "can't give both project file and compilation arguments"
            match FSharpDaemon.ProjectCracker.load (new System.Collections.Concurrent.ConcurrentDictionary<_,_>()) fsprojFile with 
            | Ok (options, sourceFiles, _log) -> 
                let options = { options with SourceFiles = Array.ofList sourceFiles }
                let sourceFilesSet = Set.ofList sourceFiles
                let options = { options with OtherOptions = options.OtherOptions |> Array.filter (fun s -> not (sourceFilesSet.Contains(s))) }
                Result.Ok options
            | Error err -> 
                failwithf "Couldn't parse project file: %A" err
            
        | None -> 
            let sourceFiles, otherFlags2 = fsharpArgs |> Array.partition (fun arg -> arg.EndsWith(".fs") || arg.EndsWith(".fsi") || arg.EndsWith(".fsx"))
            let otherFlags = [| yield! otherFlags; yield! otherFlags2 |]
            let sourceFiles = sourceFiles |> Array.map Path.GetFullPath 
            printfn "CurrentDirectory = %s" Environment.CurrentDirectory
        
            match sourceFiles with 
            | [| script |] when script.EndsWith(".fsx") ->
                let text = readFile script
                let options, errors = checker.GetProjectOptionsFromScript(script, text, otherFlags=otherFlags) |> Async.RunSynchronously
                if errors.Length > 0 then 
                    for error in errors do 
                        printfn "%s" (error.ToString())
                    Result.Error ()
                else                                
                    //let options = { options with SourceFiles = sourceFiles }
                    Result.Ok options
            | _ -> 
                let options = checker.GetProjectOptionsFromCommandLineArgs("tmp.fsproj", otherFlags)
                let options = { options with SourceFiles = sourceFiles }
                Result.Ok options

    match options with 
    | Result.Error () -> 
        printfn "fscd: error processing project options or script" 
        -1
    | Result.Ok options ->
    let options = { options with OtherOptions = Array.append options.OtherOptions (Array.ofList otherFlags) }
    //printfn "options = %A" options

    let rec checkFile count sourceFile =         
        try 
            let _, checkResults = checker.ParseAndCheckFileInProject(sourceFile, 0, readFile sourceFile, options) |> Async.RunSynchronously  
            match checkResults with 
            | FSharpCheckFileAnswer.Aborted -> 
                printfn "aborted"
                Result.Error None

            | FSharpCheckFileAnswer.Succeeded res -> 
                let mutable hasErrors = false
                for error in res.Errors do 
                    printfn "%s" (error.ToString())
                    if error.Severity = FSharpErrorSeverity.Error then 
                        hasErrors <- true

                if hasErrors then 
                    Result.Error res.ImplementationFile
                else
                    Result.Ok res.ImplementationFile 
        with 
        | :? System.IO.IOException when count = 0 -> 
            System.Threading.Thread.Sleep 500
            checkFile 1 sourceFile
        | exn -> 
            printfn "%s" (exn.ToString())
            Result.Error None

    let keepRanges = eval
    let convFile (i: FSharpImplementationFileContents) =         
        //(i.QualifiedName, i.FileName
        i.FileName, { Code = Convert(keepRanges).ConvertDecls i.Declarations }

    let checkFiles files =             
        let rec loop rest acc = 
            match rest with 
            | file :: rest -> 
                match checkFile 0 (Path.GetFullPath(file)) with 

                // Note, if livechecks are on, we continue on regardless of errors
                | Result.Error iopt when not livechecksonly -> 
                    printfn "fscd: ERRORS for %s" file
                    Result.Error ()

                | Result.Error iopt 
                | Result.Ok iopt -> 
                    printfn "fscd: COMPILED %s" file
                    match iopt with 
                    | None -> Result.Error ()
                    | Some i -> 
                        printfn "fscd: GOT PortaCode for %s" file
                        loop rest (i :: acc)
            | [] -> Result.Ok (List.rev acc)
        loop (List.ofArray files) []

    let jsonFiles (impls: FSharpImplementationFileContents[]) =         
        let data = Array.map convFile impls
        let json = Newtonsoft.Json.JsonConvert.SerializeObject(data)
        json

    let sendToWebHook (hook: string) fileContents = 
        try 
            let json = jsonFiles (Array.ofList fileContents)
            printfn "fscd: GOT JSON, length = %d" json.Length
            use webClient = new WebClient(Encoding = Encoding.UTF8)
            printfn "fscd: SENDING TO WEBHOOK... " // : <<<%s>>>... --> %s" json.[0 .. min (json.Length - 1) 100] hook
            let resp = webClient.UploadString (hook,"Put",json)
            printfn "fscd: RESP FROM WEBHOOK: %s" resp
        with err -> 
            printfn "fscd: ERROR SENDING TO WEBHOOK: %A" (err.ToString())

    let emitInfoFile (sourceFile: string) lines = 
        let infoDir = Path.Combine(Path.GetDirectoryName(sourceFile), ".fsharp")
        let infoFile = Path.Combine(infoDir, Path.GetFileName(sourceFile) + ".info")
        let lockFile = Path.Combine(infoDir, Path.GetFileName(sourceFile) + ".info.lock")
        printfn "writing info file %s..." infoFile 
        try 
            File.WriteAllLines(infoFile, lines)
        finally
            try if Directory.Exists infoDir && File.Exists lockFile then File.Delete lockFile with _ -> ()

    let clearInfoFile sourceFile = 
        emitInfoFile sourceFile [| |]

    /// Format values resulting from live checking using the interpreter
    let rec formatValue (value: obj) = 
        match value with 
        | null -> "<null>"
        | :? string as s -> sprintf "%A" s
        | value -> 
        let ty = value.GetType()
        if ty.Name = "DT`1" then 
            // TODO: this is a hack for TensorFlow.FSharp, consider how to generalize it
            value.ToString()
        elif Reflection.FSharpType.IsTuple(ty) then 
            let vs = Reflection.FSharpValue.GetTupleFields(value)
            "(" + String.concat "," (Array.map formatValue vs) + ")"
        elif Reflection.FSharpType.IsFunction(ty) then 
            "<func>"
        elif ty.IsArray then 
            let value = (value :?> Array)
            if ty.GetArrayRank() = 1 then 
                "[| " + String.concat "; " [| for i in 0 .. min 10 (value.GetLength(0) - 1) -> formatValue (value.GetValue(i)) |] + " |]"
            else
                sprintf "array rank %d" value.Rank 
        elif Reflection.FSharpType.IsRecord(ty) then 
            let fs = Reflection.FSharpType.GetRecordFields(ty)
            let vs = Reflection.FSharpValue.GetRecordFields(value)
            "{ " + String.concat "; " [| for (f,v) in Array.zip fs vs -> f.Name + "=" + formatValue v |] + " }"
        elif Reflection.FSharpType.IsUnion(ty) then 
            let uc, vs = Reflection.FSharpValue.GetUnionFields(value, ty)
            uc.Name + "(" + String.concat ", " [| for v in vs -> formatValue v |] + ")"
        elif value :? System.Collections.IEnumerable then 
            "<seq>"
        else 
            value.ToString() //"unknown value"

    let MAXTOOLTIP = 100
    /// Write an info file containing extra information to make available to F# tooling.
    /// This is currently experimental and only experimental additions to F# tooling
    /// watch and consume this information.
    let writeInfoFile (tooltips: (DRange * (string * obj) list * bool)[]) sourceFile errors = 

        let lines = 
            let ranges =  HashSet<DRange>(HashIdentity.Structural)
            let havePreferred = tooltips |> Array.choose (fun (m,_,prefer) -> if prefer then Some m else None) |> Set.ofArray
            [| for (range, lines, prefer) in tooltips do
                    

                    // Only emit one line for each range. If live checks are performed twice only
                    // the first is currently shown.  
                    //
                    // We have a hack here to prefer some entries over others.  FCS returns non-compiler-generated
                    // locals for curried functions like 
                    //     a |> ... |> foo1 
                    // or
                    //     a |> ... |> foo2 x
                    //
                    // which become 
                    //     a |> ... |> (fun input -> foo input)
                    //     a |> ... |> (fun input -> foo2 x input
                    // but here a use is reported for "input" over the range of the application expression "foo1" or "foo2 x"
                    // So we prefer the actual call over these for these ranges.
                    //
                    // TODO: report this FCS problem and fix it.
                    if not (ranges.Contains(range))  && (prefer || not (havePreferred.Contains range)) then 
                        ranges.Add(range) |> ignore

                        // Format multiple lines of text into a single line in the output file
                        let valuesText = 
                            [ for (action, value) in lines do 
                                  let action = (if action = "" then "" else action + " ")
                                  let valueText = formatValue value
                                  let valueText = valueText.Replace("\n", " ").Replace("\r", " ").Replace("\t", " ")
                                  let valueText = 
                                      if valueText.Length > MAXTOOLTIP then 
                                          valueText.[0 .. MAXTOOLTIP-1] + "..."
                                      else   
                                          valueText
                                  yield action + valueText ]
                            |> String.concat "~   " // special new-line character known by experimental VS tooling + indent
                    
                        let sep = (if lines.Length = 1 then " " else "~   ")
                        let line = sprintf "ToolTip\t%d\t%d\t%d\t%d\tLiveCheck:%s%s" range.StartLine range.StartColumn range.EndLine range.EndColumn sep valuesText
                        yield line

               for (exn:exn, rangeStack) in errors do 
                    if List.length rangeStack > 0 then 
                        let range = List.last rangeStack 
                        let message = "LiveCheck failed: " + exn.Message.Replace("\t"," ").Replace("\r","   ").Replace("\n","   ") 
                        printfn "%s" message
                        let line = sprintf "Error\t%d\t%d\t%d\t%d\terror\t%s\t304" range.StartLine range.StartColumn range.EndLine range.EndColumn message
                        yield line |]

        emitInfoFile sourceFile lines

    /// Evaluate the declarations using the interpreter
    let evaluateDecls fileContents = 
        let assemblyTable = 
            dict [| for r in options.OtherOptions do 
                        if r.StartsWith("-r:") && not (r.Contains(".NETFramework")) then 
                            let assemName = r.[3..]
                            printfn "Script: pre-loading referenced assembly %s " assemName
                            match System.Reflection.Assembly.LoadFrom(assemName) with 
                            | null -> 
                                printfn "Script: failed to pre-load referenced assembly %s " assemName
                            | asm -> 
                                let name = asm.GetName()
                                yield (name.Name, asm) |]

        let assemblyResolver (nm: Reflection.AssemblyName) =  
            match assemblyTable.TryGetValue(nm.Name) with
            | true, res -> res
            | _ -> Reflection.Assembly.Load(nm)
                                        
        let tooltips = ResizeArray()
        let sink =
            if writeinfo then 
                { new Sink with 
                     member __.CallAndReturn(mref, mdef, _typeArgs, args, res) = 
                         let lines = 
                            [ for (p, arg) in Seq.zip mdef.Parameters args do 
                                  yield (sprintf "%s:" p.Name, arg)
                              if mdef.IsValue then 
                                  yield ("value:", res.Value)
                              else
                                  yield ("return:", res.Value) ]
                         mdef.Range |> Option.iter (fun r -> 
                             tooltips.Add(r, lines, true))
                         mref.Range |> Option.iter (fun r -> 
                             tooltips.Add(r, lines, true))

                     member __.BindValue(vdef, value) = 
                         if not vdef.IsCompilerGenerated then 
                             vdef.Range |> Option.iter (fun r -> tooltips.Add ((r, [("", value.Value)], false)))

                     member __.BindLocal(vdef, value) = 
                         if not vdef.IsCompilerGenerated then 
                             vdef.Range |> Option.iter (fun r -> tooltips.Add ((r, [("", value.Value)], false)))

                     member __.UseLocal(vref, value) = 
                         vref.Range |> Option.iter (fun r -> tooltips.Add ((r, [("", value.Value)], false)))
                }
                |> Some
            else  
                None

        let ctxt = EvalContext(assemblyResolver, ?sink=sink)
        let fileConvContents = [| for i in fileContents -> convFile i |]

        for (_, contents) in fileConvContents do 
            ctxt.AddDecls(contents.Code)

        for (sourceFile, ds) in fileConvContents do 
            printfn "evaluating decls.... " 
            let errors = ctxt.TryEvalDecls (envEmpty, ds.Code, evalLiveChecksOnly=livechecksonly)

            if writeinfo then 
                writeInfoFile (tooltips.ToArray()) sourceFile errors
            else
                for (exn, _range) in errors do
                   raise exn

            printfn "...evaluated decls" 

    let changed why _ =
        try 
            printfn "fscd: CHANGE DETECTED (%s), COMPILING...." why

            match checkFiles options.SourceFiles with 
            | Result.Error () -> ()

            | Result.Ok allFileContents -> 

            match webhook with 
            | Some hook -> sendToWebHook hook allFileContents
            | None -> 

            if eval then 
                printfn "fscd: CHANGE DETECTED, RE-EVALUATING ALL INPUTS...." 
                evaluateDecls allFileContents 

            // The default is to dump
            if not eval && webhook.IsNone then 
                let fileConvContents = jsonFiles (Array.ofList allFileContents)

                printfn "%A" fileConvContents

        with err when watch -> 
            printfn "fscd: exception: %A" (err.ToString())

    for o in options.OtherOptions do 
        printfn "compiling, option %s" o

    if watch then 
        // Send an immediate changed() event
        if webhook.IsNone then 
            printfn "Sending initial changes... " 
            for sourceFile in options.SourceFiles do
                changed "initial" ()

        let mkWatcher (path, fileName) = 
            let watcher = new FileSystemWatcher(path, fileName)
            watcher.NotifyFilter <- NotifyFilters.Attributes ||| NotifyFilters.CreationTime ||| NotifyFilters.FileName ||| NotifyFilters.LastAccess ||| NotifyFilters.LastWrite ||| NotifyFilters.Size ||| NotifyFilters.Security;
            watcher.Changed.Add (changed "Changed")
            watcher.Created.Add (changed "Created")
            watcher.Deleted.Add (changed "Deleted")
            watcher.Renamed.Add (changed "Renamed")
            watcher

        let watchers = 
            [ for sourceFile in options.SourceFiles do
                let path = Path.GetDirectoryName(sourceFile)
                let fileName = Path.GetFileName(sourceFile)
                printfn "fscd: WATCHING %s in %s" fileName path 
                yield mkWatcher (path, fileName)
                if useEditFiles then 
                    let infoDir, editFile = editDirAndFile fileName
                    printfn "fscd: WATCHING %s in %s" editFile infoDir 
                    yield mkWatcher (infoDir, Path.GetFileName editFile) ]

        for watcher in watchers do
            watcher.EnableRaisingEvents <- true

        printfn "Waiting for changes... press any key to exit" 
        System.Console.ReadLine() |> ignore
        for watcher in watchers do
            watcher.EnableRaisingEvents <- false

    else
        changed "once" ()
    0

