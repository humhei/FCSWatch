# FSharp.Compiler.PortaCode
The PortaCode F# code format and corresponding interpreter. 

* Used by Fabulous and others.

* Currently distributed by source inclusion, no nuget package yet

* FsLive.Cli is a live programming "watch my project" command line tool which is experimental

* Wet paint, API will change

It's used for the "LiveUpdate" feature of Fabulous, to interpret the Elmish model/view/update application code on-device.

It's also used for the experimental "LiveCheck" feature of
It's not actually necessary on Android since full mono JIT is available. On iOS it appears necessary.

The interpreter may also be useful for other live checking tools, because you get escape the whole complication of actual IL generation, Reflection emit and reflection invoke, and no actual classes etc are generated. We can also adapt the interpreter over time to do things like report extra information back to the host.

### Code format

The input code format for the interpreter (PortaCode) is derived from FSharp.Compiler.Service expressions, the code is in this repo.


### Interpretation

The semantics of interpretation can differ from the semantics of .NET F# code. Perf is not good but in many live check scenarios you're sitting on a base set of DLLs which are regular .NET code and are efficiently invoked.

Library calls are implemented by reflection invoke.  It's the same interpreter we use on-device for Fabulous.

### Command line arguments

    --webhook url      send JSON serialized PortaCode to webhook
    --eval             evaluate contents using the interpreter
    --livechecksonly   see below, only evaluate LiveChecks and their dependencies
    --writeinfo        write info files for IDE tooling (experimental)
    --vshack           (experimental)

Fabulous uses a `--webhook` argument in combination with `--watch` to send a serialized version of the code to a web request on each change.

### LiveChecks

* A LiveCheck is a declaration like this: https://github.com/fsprojects/TensorFlow.FSharp/blob/master/examples/NeuralStyleTransfer-dsl.fsx#L109 …

* The attribute indicates the intent that that specific piece of code (and anything it depends on) should be run at development time. This is _not_ done by the IDE directly but by some other tool you have to start.

* An example tool is the "fslive.exe" tool (soon to be a global command) from this repo here https://github.com/fsprojects/FSharp.Compiler.PortaCode/blob/master/src/ProcessCommandLine.fs#L46.  Like FsAutoComplete this watches for project changes and then recompiles using FCS and looks for LiveCheck attributes.  It then interprets those evaluations using reflection and collects information about the execution.  For example, it detects errors and detects when variables have been bound to particular values during interpretation.  

* This may be reconfigured to be an [F# Analyzer](https://medium.com/lambda-factory/introducing-f-analyzers-772487889429).  The tool currently emits a ".fsharp/file.fsx.info" file containing extra information about the file "file.fsx" - extra error messages and extra tooltips.  The FCS modification is to notice the existence of this file and incorporate the added information into Intellisense results. It keeps the checker tool totally decoupled from the IDE tooling.  We could use a different protocol later, also it could be extended to include more information.
 

