module FcsWatch.FakeHelper
open System.Threading
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.FcsWatcher

let runFcsWatcher (checker: FSharpChecker) projectFile = 
    let manualSet = new ManualResetEventSlim(false)
    let watcher = fcsWatcher id checker projectFile 
    manualSet.Wait()

let runFcsWatcherWithConfig config (checker: FSharpChecker) projectFile = 
    let manualSet = new ManualResetEventSlim(false)
    let watcher = fcsWatcher config checker projectFile 
    manualSet.Wait()