module FcsWatch.FakeHelper
open System.Threading
open Atrous.Core.Utils.FakeHelper
open Microsoft.FSharp.Compiler.SourceCodeServices
open Types

let runWatcher (checker: FSharpChecker) projectFile root = 
    let manualSet = new ManualResetEventSlim(false)
    dotnet root "build" []
    let watcher = 
        let buildConfig = fun config -> {config with WorkingDir = root}
        new FcsWatcher(buildConfig,FSharpChecker.Create(),projectFile)
    /// Modify fs files in TestLib2
    manualSet.Wait()