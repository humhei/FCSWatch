module ExcelDna

open Fake.IO
open System.IO
open Fake.IO.FileSystemOperators

open System.Diagnostics
open System.Runtime.InteropServices
open Microsoft.Office.Interop.Excel
open Fake.Core
open FcsWatch.Core.Types
open FcsWatch.Core
open FcsWatch.Binary


[<AutoOpen>]
module User32 =

    [<DllImport("user32")>]
    extern int GetWindowThreadProcessId(int hwnd, int& lpdwprocessid )

    let getPidFromHwnd(hwnd:int) :int =
        let mutable pid = 0
        GetWindowThreadProcessId(hwnd, &pid) |> ignore
        pid


let plugin developmentTarget (projectPath: string) =
    if not (File.exists projectPath) then failwithf "Cannot find file %s" projectPath

    let projectName = Path.GetFileNameWithoutExtension projectPath
    let projectDir = Path.getDirectory projectPath

    let addInNameStarter = projectName
    let app =
        let appInbox =
            Process.GetProcesses()
            |> Array.tryFind (fun proc -> proc.ProcessName = "EXCEL")
            |> function
                | Some proc -> Marshal.GetActiveObject("Excel.Application")
                | None ->
                    failwithf "Please manual open excel, and add plugin %s" projectName
            :?> Application


        // let app = new ApplicationClass()
        let workbooks = appInbox.Workbooks

        let workBookOp = 
            let xlPath = projectDir </> "datas/book1.xlsx"
            if File.exists xlPath
            then 
                workbooks.Open(Filename = xlPath, Editable = true)
                |> Some
            else 
                printfn "It's possible place file %s to test" xlPath
                None

        appInbox


    // let app = new ApplicationClass()
    let procId = getPidFromHwnd app.Hwnd

    let addIn =
        seq {
            for addIn in app.AddIns do
                yield addIn
        } |> Seq.find (fun addIn -> addIn.Name.StartsWith addInNameStarter)
    addIn.Installed <- false
    app.Visible <- true

    //DotNet.build (fun ops -> {ops with Configuration = DotNet.BuildConfiguration.Debug }) projectFile

    addIn.Installed <- true

    let installPlugin() =
        //let r = DotNet.exec (fun ops -> { ops with WorkingDirectory = projectDir}) "msbuild" "/t:ExcelDnaBuild;ExcelDnaPack"
        //assert (r.OK)
        try 
            addIn.Installed <- true
        with ex -> printfn "%A" ex
        Trace.trace "installed plugin"

    let unInstall() =
        try 
            addIn.Installed <- false
        with ex -> printfn "%A" ex
        Trace.trace "unInstalled plugin"

    let calculate() =
        Trace.trace "calculate worksheet"
        let ws = app.ActiveSheet :?> Worksheet
        ws.Calculate()

    let pluginDebugInfo: PluginDebugInfo = 
        { DebuggerAttachTimeDelay = 2000
          Pid = procId 
          VscodeLaunchConfigurationName = "Attach Excel" } 

    match developmentTarget with 
    | DevelopmentTarget.AutoReload _ ->
        let plugin : AutoReload.Plugin= 
            { Load = installPlugin
              Unload = unInstall
              Calculate = calculate
              PluginDebugInfo = Some pluginDebugInfo
            }

        DevelopmentTarget.autoReloadPlugin plugin
    | DevelopmentTarget.Debuggable _ ->
        let plugin : DebuggingServer.Plugin = 
            { Load = installPlugin
              Unload = unInstall
              Calculate = calculate
              PluginDebugInfo = pluginDebugInfo } 
        DevelopmentTarget.debuggablePlugin plugin
       


