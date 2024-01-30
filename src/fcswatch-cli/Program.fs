// Learn more about F# at http://fsharp.org
module FcsWatch.Cli.Main
open FcsWatch.Binary
open FcsWatch.Cli.Share
open System.Threading.Tasks
open Ionide.ProjInfo


let splitArgs args =
    let arguArgs =
        args
        |> Array.takeWhile(fun x -> x <> "--")
    let additionalArgs =
        args
        |> Array.skipWhile(fun x -> x <> "--")
        |> (fun a -> if Array.tryHead a = Some "--" then Array.tail a else a)
    (arguArgs, additionalArgs)

[<EntryPoint>]
let main argv =
    //let projectFile = @"D:\VsCode\Github\FCSWatch\tests\datas\TestProject\TestProject.fsproj"
    //let cwd =
    //    System.IO.Path.GetDirectoryName projectFile
    //    |> System.IO.DirectoryInfo
    
    //let toolsPath = Ionide.ProjInfo.Init.init cwd None
    //let loaderError =
    //    ProjectLoader.loadProject projectFile BinaryLogGeneration.Off []
    //printfn "Hello from F#"
    
    
    try
        let exited = TaskCompletionSource<unit>()
        System.Console.CancelKeyPress.Add(fun _ ->
            exited.TrySetResult ()
            |> ignore
        )

        System.Runtime.Loader.AssemblyLoadContext.Default.add_Unloading(fun _ ->
            exited.TrySetResult ()
            |> ignore
        )
        let arguArgs, additionalArgs = splitArgs argv

        let results = parser.Parse arguArgs

        let processResult = processParseResults additionalArgs results

        runFcsWatcher exited.Task processResult.Config processResult.ProjectFile
        |> Async.RunSynchronously

        0

    with :? Argu.ArguParseException as e ->
        stdout.WriteLine e.Message

        LanguagePrimitives.EnumToValue e.ErrorCode
