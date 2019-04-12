// Copyright 2018 Fabulous contributors. See LICENSE.md for license.

// F# Compiler Daemon sample
//
// Sample use, assumes app has a reference to ELmish.XamrinForms.LiveUpdate:
//
// cd Fabulous\Samples\CounterApp\CounterApp
//   adb -d forward  tcp:9867 tcp:9867
// dotnet run --project ..\..\..\Fabulous.Cli\Fabulous.Cli.fsproj -- --eval @out.args
// dotnet run --project ..\..\..\Fabulous.Cli\Fabulous.Cli.fsproj -- --watch --webhook:http://localhost:9867/update @out.args

module FsLive.Driver

open FcsWatch.Porta
open FcsWatch.Core
open System.IO
open System



#if !TEST
[<EntryPoint>]
#endif
let main (argv: string[]) =

    let mutable fsproj = None
    let mutable eval = false
    let mutable livechecksonly = false
    let mutable watch = false
    let mutable useEditFiles = false
    let mutable loggerLevel = Logger.Level.Minimal
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
                elif arg.StartsWith "--loggerlevel:" then 
                    loggerLevel  <- 
                        match arg.["--loggerlevel:".Length ..] with 
                        | "0" -> Logger.Level.Quiet
                        | "1" -> Logger.Level.Minimal
                        | "2" -> Logger.Level.Normal
                        | s -> failwithf "invalid logger level number %s" s 

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
                    printfn "   --loggerlevel:<0|1|2>     0: Quiet; 1: Minimal; 2: Normal; Default is Minimal"
                    printfn "   --writeinfo       (Experimental) Write an info file based on results of evaluation"
                    printfn "   --vshack          (Experimental) Watch for .fsharp/foo.fsx.edit files and use the contents of those"
                    printfn "   <other-args>      All other args are assumed to be extra F# command line arguments"
                    exit 1
                else yield arg  |]


    let config: PortaConfig =
        { Eval = eval 
          LiveCheckOnly = livechecksonly 
          LoggerLevel = loggerLevel 
          Fsproj = fsproj 
          UseEditFiles = useEditFiles
          WriteInfo = writeinfo 
          NoBuild = true 
          WorkingDir = Directory.GetCurrentDirectory() 
          Webhook = webhook 
          Watch = watch 
          OtherFlags = Array.append fsharpArgs (Array.ofList otherFlags) }

    try 
        System.Environment.SetEnvironmentVariable("LIVECHECK", "1")
        runFcsWatcher config
        0
    with e -> 
        printfn "Error: %s" (e.ToString())
        1
