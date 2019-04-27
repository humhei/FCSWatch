// Learn more about F# at http://fsharp.org

open System
open System.Collections.Concurrent
open System.Threading
open TestLib2
open System.IO

/// https://github.com/humhei/FCSWatch/issues/23

//let private getEmbeddedStringFromFile (resourceName:string) =
//    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
//    printfn "Assembly: %A" assembly
//    let rns = assembly.GetManifestResourceNames()
//    printfn "Resource Names: %A" rns
//    let rn = assembly.GetManifestResourceNames() |> Seq.find (fun n -> n.EndsWith resourceName)
//    use s = assembly.GetManifestResourceStream rn
//    use sr = new StreamReader(s)
//    sr.ReadToEnd()

//let private resource = getEmbeddedStringFromFile "test.txt"

[<EntryPoint>]
let main _ =

    //printfn "Hell world from resource %s" resource

    printfn "HEssssss222  9108 985966911 911000999                        %s" (Say.fromLib2)

    /// simulate server
    Console.ReadLine()
    |> ignore

    0 // return an integer exit code
