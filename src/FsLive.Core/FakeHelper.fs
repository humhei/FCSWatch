module FsLive.Core.FakeHelper
open System.Threading
open Microsoft.FSharp.Compiler.SourceCodeServices
open FsLive.Core.FsLive

let runFsLive developmentTarget (checker: FSharpChecker) projectFile = 
    let manualSet = new ManualResetEventSlim(false)
    let watcher = fsLive id developmentTarget checker projectFile 
    manualSet.Wait()

let runFsLiveWith buidingConfig developmentTarget (checker: FSharpChecker) projectFile = 
    let manualSet = new ManualResetEventSlim(false)
    let watcher = fsLive buidingConfig developmentTarget checker projectFile 
    manualSet.Wait()