module FcsWatch.Tests.Types
open System.Xml

[<RequireQualifiedAccess>]
module Fsproj =
    let private equalIgnoreCaseAndEdgeSpace (text1: string) (text2: string) = 
        match text1.Trim().CompareTo (text2.Trim()) with 
        | 0 -> true
        | _ -> false

    let addFileToProject file (projectFile: string) =
        let doc = new XmlDocument()
        doc.Load(projectFile)
        let compiledFiles = doc.GetElementsByTagName "Compile"
        let compiledFileList =                         
            [ for compiledFile in compiledFiles do 
                yield compiledFile
            ]
        match List.tryFind (fun (compiledFile: XmlNode) -> 
            equalIgnoreCaseAndEdgeSpace file compiledFile.Attributes.["Include"].Value) compiledFileList with 
        | Some _ -> ()
        | None ->
            let firstCompiledFile = compiledFiles.[0]
            let addedCompiledFile = firstCompiledFile.Clone()
            addedCompiledFile.Attributes.["Include"].Value <- file

            firstCompiledFile.ParentNode.AppendChild addedCompiledFile
            |> ignore

        doc.Save(projectFile)  
    let removeFileFromProject file (projectFile: string) =
        let doc = new XmlDocument()
        doc.Load(projectFile)
        let compiledFiles = doc.GetElementsByTagName "Compile"
        let compiledFileList =                         
            [ for compiledFile in compiledFiles do 
                yield compiledFile
            ]
        match List.tryFind (fun (compiledFile: XmlNode) -> 
            equalIgnoreCaseAndEdgeSpace file compiledFile.Attributes.["Include"].Value) compiledFileList with 
        | Some xmlNode -> 
            xmlNode.ParentNode.RemoveChild xmlNode |> ignore
        | None -> ()
        doc.Save(projectFile)  
    