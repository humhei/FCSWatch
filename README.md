# Run standard fsharp codes in watch mode

Stable | Prerelease
--- | ---
[![NuGet Badge](https://buildstats.info/nuget/FCSWatch)](https://www.nuget.org/packages/FCSWatch/) | [![NuGet Badge](https://buildstats.info/nuget/FCSWatch?includePreReleases=true)](https://www.nuget.org/packages/FCSWatch/)


MacOS/Linux | Windows
--- | ---
[![CircleCI](https://circleci.com/gh/humhei/FCSWatch.svg?style=svg)](https://circleci.com/gh/humhei/FCSWatch) | [![Build status](https://ci.appveyor.com/api/projects/status/0qnls95ohaytucsi?svg=true)](https://ci.appveyor.com/project/ts2fable-imports/FCSWatch)
[![Build History](https://buildstats.info/circleci/chart/humhei/FCSWatch)](https://circleci.com/gh/humhei/FCSWatch) | [![Build History](https://buildstats.info/appveyor/chart/ts2fable-imports/FCSWatch)](https://ci.appveyor.com/project/ts2fable-imports/FCSWatch)


---

## Play around
vscode only

### MiniSample
For a quick play around
Try https://github.com/humhei/FcsWatchMiniSample

### From source code interaction test

* git clone https://github.com/humhei/FCSWatch.git
* fake build
* cd tests/FcsWatch.InteractionTests/`
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
source https://api.nuget.org/v3/index.json
nuget Fake.Core.Target = 5.12.0
nuget FcsWatch //"
#load "./.fake/build.fsx/intellisense.fsx"

// start build
open Fake.Core
open Fake.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.FakeHelper
open Fake.DotNet

Target.create "FcsWatch" (fun _ ->

    let projectFile = Path.getFullName "./FcsWatchMiniSample/FcsWatchMiniSample.fsproj"
    printfn "%A" projectFile
    DotNet.build (fun ops ->
      { ops with
          Configuration = DotNet.BuildConfiguration.Debug }
    ) projectFile
    let checker = FSharpChecker.Create()
    runFcsWatcher checker projectFile
)

Target.runOrDefault "FcsWatch"

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
          DebuggerAttachTimeDelay = 1000 }debug build 
