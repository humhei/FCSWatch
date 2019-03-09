
## Play around
vscode only

### MiniSample
For a quick play around
Try https://github.com/humhei/FcsWatchMiniSample

### From source code interaction test

* git clone https://github.com/humhei/FCSWatch.git
`cd tests/FcsWatch.InteractionTests/`
* dotnet run
* modify fs files in any of TestProject,TestLib2,TestLib1
* Set breakpoint in any of TestProject,TestLib2,TestLib1
* F5 Debug `Launch TestProject`
* modify fs files in any of TestProject,TestLib2,TestLib1
* (Optional) add new fs file in any of TestProject,TestLib2,TestLib1
* Relaunch Debugger

## Get started

1. Install [fake5](https://fake.build/fake-gettingstarted.html)
2. Replace build.fsx with below codes
```fsharp
#r "paket:
nuget FcsWatch
nuget Fake.Core.Target //"

#load "./.fake/build.fsx/intellisense.fsx"

// start build
open Fake.Core
open Fake.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.FakeHelper

let root = Path.getDirectory "./"
Target.create "FcsWatch" (fun _ ->
    let projectFile = Path.getFullName "YourProject/YourProject.fsproj"
    DotNet.build id projectFile
    let checker = FSharpChecker.Create()
    /// or runFcsWatcherWith for more customizations
    runFcsWatcher checker projectFile
)

Target.create "Default" ignore

Target.runOrDefault "Default"
```
3. `fake build -t "FcsWatch"`
4. `Change fs files in YourProject` and save it


### Launch debugging in vscode
You can also launch debugging when running in watch mode
```
    {
        "name": "launch TestProject",
        "type": "coreclr",
        "request": "launch",
        "preLaunchTask": "emitCompilerTmp",
        "program": "${workspaceFolder}/YourProject/bin/Debug/targetFramwork/YourProject.exe",
    },
    /// send a http request to copy dlls in obj to bin
    {
        "label": "emitCompilerTmp",
        "command": "curl",
        "args": [
            "--config",
            ".fake/fcswatch/port.cache"
        ],
        "presentation": {
            "reveal": "silent"
        }
    },
  },
```

When you are debugging files,watch mode still take effect


### Plugin mode
e.g.: excelDna sample
vscode launch.json setting
```
    {
      "name": "Attach Excel",
      "type": "clr",
      "request": "attach",
      "preLaunchTask": "emitCompilerTmp",
      "processId": 14876
    },
```

build.fsx setting
```fsharp
    /// trigger when file changed was detected
    /// and (re)load debugger (after emit cache)
    let installPlugin() =
        dotnet projectDir "msbuild" ["/t:ExcelDnaBuild;ExcelDnaPack"]
        addIn.Installed <- true
        Trace.trace "installed plugin"

    /// trigger when file changed was detected
    /// and (re)load debugger (before emit cache)
    let unInstall() =
        addIn.Installed <- false
        Trace.trace "unInstalled plugin"

    /// trigger when (re)load debugger (after installPlugin())
    let calculate() =
        Trace.trace "calculate worksheet"
        worksheet.Calculate()

    let plugin =
        { Load = installPlugin
          Unload = unInstall
          Calculate = calculate
          /// Thread sleed to wait debugger attached.
          /// Trigger when file changed was not detected
          /// and reload debugger
          DebuggerAttachTimeDelay = 1000 }

    runFcsWatcherWith (fun config ->
        { config with
            Logger = Logger.Normal
            DevelopmentTarget = DevelopmentTarget.Plugin plugin
        }
    ) checker projectFile

```


### Why?
why not use dotnet watch:
1. dotnet watch reference all dlls every time (which will take at least 3000ms?) (while fcs hold dlls in runtime cache)
2. not easy to debug when you are using dotnet watch


![](https://github.com/humhei/Resources/blob/Resources/TestfsFCSWatchVisualStud.gif)
