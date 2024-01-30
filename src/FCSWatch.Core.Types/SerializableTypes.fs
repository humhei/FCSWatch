namespace FcsWatch.Core
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open FSharp.Compiler.CodeAnalysis

type SerializableRange =    
    { FileName:string  
      Start: Position
      End: Position
      Line: int }

[<RequireQualifiedAccess>]
module SerializableRange =
    let ofRange (range: range) =
        { FileName = range.FileName 
          Start = range.Start
          End = range.End
          Line = range.EndLine }

type SerializableFSharpDiagnostic =
    { Range: SerializableRange
      Message: string 
      Severity: FSharpDiagnosticSeverity
      Subcategory: string 
      ErrorNum: int 
      NumberPrefix: string 
}
with 
    member x.Start = x.Range.Start
    member x.End = x.Range.End
    member x.FileName = x.Range.FileName

    override m.ToString() =
        let fileName = m.FileName
        let s = m.Start
        let e = m.End
        let severity = 
            match m.Severity with
            | FSharpDiagnosticSeverity.Warning -> "warning"
            | FSharpDiagnosticSeverity.Error -> "error"
            | FSharpDiagnosticSeverity.Info -> "info"
            | FSharpDiagnosticSeverity.Hidden -> "hidden"
        sprintf "%s (%d,%d)-(%d,%d) %s %s %s" fileName s.Line (s.Column + 1) e.Line (e.Column + 1) m.Subcategory severity m.Message


[<RequireQualifiedAccess>]
module SerializableFSharpDiagnostic =
    let ofFSharpDiagnostic(info: FSharpDiagnostic) =
        { Range = SerializableRange.ofRange info.Range 
          Message = info.Message
          Severity = info.Severity
          Subcategory = info.Subcategory
          ErrorNum = info.ErrorNumber
          NumberPrefix = info.ErrorNumberPrefix }


type SerializableFSharpCheckFileResults =
    { Diagnostics: SerializableFSharpDiagnostic [] }


[<RequireQualifiedAccess>]
module SerializableFSharpCheckFileResults =
    let ofFSharpCheckFileResults(info: FSharpCheckFileResults) =
        { Diagnostics = Array.map SerializableFSharpDiagnostic.ofFSharpDiagnostic info.Diagnostics  }

[<RequireQualifiedAccess>]
type SerializableFSharpCheckFileAnswer =
    | Succeeded of SerializableFSharpCheckFileResults
    | Aborted

[<RequireQualifiedAccess>]
module SerializableFSharpCheckFileAnswer =
    let ofFSharpCheckFileAnswer(answer: FSharpCheckFileAnswer) =
        match answer with 
        | FSharpCheckFileAnswer.Succeeded results -> 
            SerializableFSharpCheckFileResults.ofFSharpCheckFileResults results
            |> SerializableFSharpCheckFileAnswer.Succeeded

        | FSharpCheckFileAnswer.Aborted -> SerializableFSharpCheckFileAnswer.Aborted
