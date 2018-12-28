#if FAKE
#r "paket: groupref build //"
#endif
#if !FAKE
#r "netstandard" // windows
#endif
#load "./.fake/build.fsx/intellisense.fsx"
open Paket
open Fake.Core
open Atrous.Core.Utils.FakeHelper
open Fake.IO
let path1 = @"D:\VsCode\Github\FCSWatch\TestProject"
let path2 = @"D:\VsCode\Github\FCSWatch\TestProject\bin\Debug\netcoreapp3.0\Ss.dll"
let dependencies = Dependencies.Locate(__SOURCE_DIRECTORY__)
let relative = Path.toRelativeFrom path1 path2
let objRelative = 
    if relative.StartsWith ".\\bin" then  ".\\obj" + relative.Substring 5
    else failwithf "is not a valid bin relativePath %s" relative
Target.create "Default" (fun _ ->
    createSln()
    let dependencies = Dependencies.Locate(__SOURCE_DIRECTORY__)
    dependencies.Update(false)
    dotnet "./" "restore" []
)
Target.runOrDefault "Default"