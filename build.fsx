#if FAKE
#r "paket: groupref build //"
#endif
#if !FAKE
#r "Facades/netstandard"
#r "netstandard"
#endif
#load "./.fake/build.fsx/intellisense.fsx"
open Fake.Core
open FPublisher.Roles
open FPublisher
open FPublisher.Nuget
open FPublisher.Git
open Fake.DotNet
open Fake.Core.TargetOperators
open FPublisher.FakeHelper.CommandHelper
open Fake.IO

let buildServer =
    BuildServer.create
        { BuildServer.Config.DefaultValue
            with
                LocalNugetServer = Some NugetServer.DefaultBaGetLocal
                LoggerLevel = Logger.Level.Normal }


Target.create "Workspace.CreateDefaultSln" <| fun _ ->
    Workspace.createDefaultSln false buildServer.Collaborator.Workspace

Target.create "NonGit.Build" <| fun _ ->
    BuildServer.run (!^ (NonGit.Msg.Build None))  buildServer

Target.create "NonGit.Test" <| fun _ ->
    BuildServer.run (!^ NonGit.Msg.Test) buildServer

Target.create "Forker.PublishToLocalNugetServer" <| fun _ ->
    BuildServer.run (!^ (Forker.Msg.PublishToLocalNugetServer)) buildServer


Target.create "Collaborator.NextRelease" <| fun _ ->
    BuildServer.run (!^ Collaborator.Msg.NextRelease) buildServer

Target.create "RunCI" <| fun _ ->
    let paket = Path.getFullName ".paket/paket.exe"
    let mono = Mono.monoPath.Value
    printfn "paket path is %s" paket
    exec mono "./" [paket ;"install"]
    BuildServer.run (BuildServer.Msg.RunCI) buildServer

Target.create "Default" ignore

"NonGit.Test"
    ==> "Default"

Target.runOrDefault "Default"