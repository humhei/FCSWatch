namespace FcsWatch.Porta
open Fake.Core
open FcsWatch.Porta.CodeModel
open FSharp.Compiler.CodeAnalysis
open FcsWatch.Porta.FromCompilerService
open System.Net
open System.Text
open System
open Interpreter
open System.Collections.Generic
open System.IO
open FSharp.Compiler.Symbols

[<AutoOpen>]
module internal FromProcessCommandLine =
    let convFile eval (i: FSharpImplementationFileContents) =         
        //(i.QualifiedName, i.FileName
        i.FileName, { Code = Convert(eval).ConvertDecls i.Declarations }

    let jsonFiles eval (impls: FSharpImplementationFileContents[]) =         
        let data = Array.map (convFile eval) impls
        let json = Newtonsoft.Json.JsonConvert.SerializeObject(data)
        json


    let sendToWebHook eval (hook: string) fileContents = 
        try 
            let json = jsonFiles eval (Array.ofList fileContents)
            printfn "fscd: GOT JSON, length = %d" json.Length
            use webClient = new WebClient(Encoding = Encoding.UTF8)
            printfn "fscd: SENDING TO WEBHOOK... " // : <<<%s>>>... --> %s" json.[0 .. min (json.Length - 1) 100] hook
            let resp = webClient.UploadString (hook,"Put",json)
            printfn "fscd: RESP FROM WEBHOOK: %s" resp
        with err -> 
            printfn "fscd: ERROR SENDING TO WEBHOOK: %A" (err.ToString())


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

    let emitInfoFile (sourceFile: string) lines = 
        let infoDir = Path.Combine(Path.GetDirectoryName(sourceFile), ".fsharp")
        let infoFile = Path.Combine(infoDir, Path.GetFileName(sourceFile) + ".info")
        let lockFile = Path.Combine(infoDir, Path.GetFileName(sourceFile) + ".info.lock")
        printfn "writing info file %s..." infoFile 
        try 
            File.WriteAllLines(infoFile, lines)
        finally
            try if Directory.Exists infoDir && File.Exists lockFile then File.Delete lockFile with _ -> ()

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
    let evaluateDecls eval writeInfo liveCheckOnly fileContents (options: FSharpProjectOptions) = 

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
            if writeInfo then 
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
        let fileConvContents = [| for i in fileContents -> convFile eval i |]

        for (_, contents) in fileConvContents do 
            ctxt.AddDecls(contents.Code)

        for (sourceFile, ds) in fileConvContents do 
            logger.ImportantLG "evaluating decls.... " 
            let errors = ctxt.TryEvalDecls (envEmpty, ds.Code, evalLiveChecksOnly=liveCheckOnly)

            if writeInfo then 
                writeInfoFile (tooltips.ToArray()) sourceFile errors
            else
                for (exn, _range) in errors do
                    raise exn

            logger.ImportantLG "...evaluated decls" 

