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
open System.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators

Target.create "Default" (fun _ ->
    createSln()
    let dependencies = Dependencies.Locate(__SOURCE_DIRECTORY__)
    dependencies.Update(true)
    dotnet "./" "restore" []
)
Target.runOrDefault "Default"