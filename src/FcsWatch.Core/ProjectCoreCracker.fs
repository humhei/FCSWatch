namespace FcsWatch.Core 
open System.Xml.Linq
open Ionide.ProjInfo.Types
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text



/// Adapted from https://github.com/fsharp/FsAutoComplete/blob/45bf4a7255f8856b0164f722a82a17108ae64981/src/FsAutoComplete.Core/ProjectCoreCracker.fs
module ProjectCoreCracker =

    open System
    open System.IO
    open System.Xml
    open Ionide.ProjInfo


    let getProjectOptionsFromScript (checker: RemotableFSharpChecker) (scriptPath: string): FSharpProjectOptions = 
        let r = 
            let sourceText = 
                File.ReadAllText(scriptPath)

            checker.GetProjectOptionsFromScript_Serializable(scriptPath, sourceText)
            |> Async.RunSynchronously

        fst r

