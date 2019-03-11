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

open FSharp.Compiler.PortaCode.ProcessCommandLine


#if !TEST
[<EntryPoint>]
#endif
let main (argv: string[]) =
    try 
        System.Environment.SetEnvironmentVariable("LIVECHECK", "1")
        ProcessCommandLine argv

    with e -> 
        printfn "Error: %s" (e.ToString())
        1
