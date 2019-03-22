# Run standard fsharp codes in watch mode

Stable | Prerelease
--- | ---
[![NuGet Badge](https://buildstats.info/nuget/FCSWatch)](https://www.nuget.org/packages/FCSWatch/) | [![NuGet Badge](https://buildstats.info/nuget/FCSWatch?includePreReleases=true)](https://www.nuget.org/packages/FCSWatch/)


MacOS/Linux | Windows
--- | ---
[![CircleCI](https://circleci.com/gh/humhei/FCSWatch.svg?style=svg)](https://circleci.com/gh/humhei/FCSWatch) | [![Build status](https://ci.appveyor.com/api/projects/status/0qnls95ohaytucsi?svg=true)](https://ci.appveyor.com/project/ts2fable-imports/FCSWatch)
[![Build History](https://buildstats.info/circleci/chart/humhei/FCSWatch)](https://circleci.com/gh/humhei/FCSWatch) | [![Build History](https://buildstats.info/appveyor/chart/ts2fable-imports/FCSWatch)](https://ci.appveyor.com/project/ts2fable-imports/FCSWatch)


---


- [Run standard fsharp codes in watch mode](#run-standard-fsharp-codes-in-watch-mode)
  * [Get started](#get-started)
    + [From Cli](#from-cli)
    + [From Fake](#from-fake)
  * [Debug in vscode(only when AutoReload is false)](#debug-in-vscode-only-when-autoreload-is-false-)
    + [Play around](#play-around)
      - [MiniSample](#minisample)
      - [From source code interaction test](#from-source-code-interaction-test)
      - [Launch debugging in vscode](#launch-debugging-in-vscode)
  * [Plugin mode](#plugin-mode)
  * [Why?](#why-)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>



## Get started
### From Cli

dotnet tool install --global fcswatch-cli

```
USAGE: fcswatch.exe [--help] [--working-dir <string>] [--project-file <string>] [--auto-reload]
                    [--logger-level <minimal|normal|quiet>] [--no-build]

OPTIONS:

    --working-dir <string>
                          Specfic working directory, default is current directory
    --project-file <string>
                          Entry project file, default is exact fsproj file in working dir
    --auto-reload         AutoReload Or Debuggable in vscode
    --logger-level <minimal|normal|quiet>
                          Quiet; Minimal; Normal
    --no-build            --no-build
    --help                display this list of options.
```

### From Fake
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
    /// replace it to your entry project file
    let projectFile = Path.getFullName "./FcsWatchMiniSample/FcsWatchMiniSample.fsproj"
    DotNet.build (fun ops ->
      { ops with
          Configuration = DotNet.BuildConfiguration.Debug }
    ) projectFile
    let checker = FSharpChecker.Create()

    let config =
        { Config.DefaultValue with
            AutoReload = true
            /// LoggerLevel = Logger.Level.Normal }

    runFcsWatcherWith config checker projectFile
)

Target.runOrDefault "FcsWatch"

```
3. `fake build -t "FcsWatch"`
4. `Change fs files in YourProject` and save it

## Debug in vscode(only when AutoReload is false)

### Play around

#### MiniSample
https://github.com/humhei/FcsWatchMiniSample

#### From source code interaction test

* git clone https://github.com/humhei/FCSWatch.git
* .paket/paket.exe install
* cd tests/FcsWatch.InteractionTests/`
* dotnet run
* modify fs files in any of TestProject,TestLib2,TestLib1
* Set breakpoint in any of TestProject,TestLib2,TestLib1
* F5 Debug `Launch TestProject`
* modify fs files in any of TestProject,TestLib2,TestLib1
* (Optional) add new fs file in any of TestProject,TestLib2,TestLib1
* Relaunch Debugger

#### Launch debugging in vscode
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


## Plugin mode
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

## Why?
why not use dotnet watch:
1. dotnet watch reference all dlls every time (which will take at least 3000ms?) (while fcs hold dlls in runtime cache)
2. not easy to debug when you are using dotnet watch


![](https://github.com/humhei/Resources/blob/Resources/TestfsFCSWatchVisualStud.gif)
