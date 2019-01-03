module FcsWatch.FakeHelper
open System.Threading
open Atrous.Core.Utils.FakeHelper
open Microsoft.FSharp.Compiler.SourceCodeServices
open Types

let runFcsWatcher (checker: FSharpChecker) projectFile = 
    let manualSet = new ManualResetEventSlim(false)
    dotnet root "build" []
    let watcher = 
        new FcsWatcher(id,checker,projectFile)
    /// Modify fs files in TestLib2
    manualSet.Wait()