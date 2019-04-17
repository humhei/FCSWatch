# Run standard fsharp codes in watch mode

Stable | Prerelease
--- | ---
[![NuGet Badge](https://buildstats.info/nuget/FCSWatch.Binary)](https://www.nuget.org/packages/FCSWatch.Binary/) | [![NuGet Badge](https://buildstats.info/nuget/FCSWatch.Binary?includePreReleases=true)](https://www.nuget.org/packages/FCSWatch.Binary/)


MacOS/Linux | Windows
--- | ---
[![CircleCI](https://circleci.com/gh/humhei/FCSWatch.svg?style=svg)](https://circleci.com/gh/humhei/FCSWatch) | [![Build status](https://ci.appveyor.com/api/projects/status/0qnls95ohaytucsi?svg=true)](https://ci.appveyor.com/project/ts2fable-imports/FCSWatch)
[![Build History](https://buildstats.info/circleci/chart/humhei/FCSWatch)](https://circleci.com/gh/humhei/FCSWatch) | [![Build History](https://buildstats.info/appveyor/chart/ts2fable-imports/FCSWatch)](https://ci.appveyor.com/project/ts2fable-imports/FCSWatch)


---


## Get started
### From Cli
`dotnet tool install --global fcswatch-cli`

`fcswatch --project-file yourProjectFile`

FcsWatch will load your project in autoReload mode by defalut

```
USAGE: fcswatch.exe [--help] [--working-dir <string>] [--project-file <string>] [--debuggable]
                    [--logger-level <minimal|normal|quiet>] [--no-build]

OPTIONS:

    --working-dir <string>
                          Specfic working directory, default is current directory
    --project-file <string>
                          Entry project file, default is exact fsproj file in working dir
    --debuggable          Enable debuggable in vscode, This will disable auto Reload
    --logger-level <minimal|normal|quiet>
                          Default is Minimal
    --no-build            --no-build
    --webhook <string>    send a web hook when program (re)run
    --send                Equivalent to --webhook http://localhost:9867/update
    --help                display this list of options.
```

### From Fake
1. Install [fake5](https://fake.build/fake-gettingstarted.html)
2. Replace build.fsx with below codes
```fsharp
#r "paket:
source https://api.nuget.org/v3/index.json
nuget Fake.Core.Target = 5.12.0
nuget FcsWatch.Binary //"
#load "./.fake/build.fsx/intellisense.fsx"

// start build
open Fake.Core
open Fake.IO
open FSharp.Compiler.SourceCodeServices
open FcsWatch.Binary

Target.create "FcsWatch" (fun _ ->
    /// replace it to your entry project file
    let projectFile = Path.getFullName "./FcsWatchMiniSample/FcsWatchMiniSample.fsproj"
    runFcsWatcher Config.DefaultValue projectFile
)

Target.runOrDefault "FcsWatch"

```
3. `fake build -t "FcsWatch"`
4. `Change fs files in YourProject` and save it


## Build From project
* .paket/paket.exe install
* dotnet build FCSWatch.sln

## File structure
### FcsWatch.Core
The core library (Include a lots of common logic
e.g `project cracker`, `file watcher`, mailbox group for concurrrent, and so on )

### FcsWatch.Binary (Ref FcsWatch.Core)
 It compile fsharp codes to .dll and .pdb (binary)
**No** webhoook currrently supported(No reason to send a dll and pdb)
Because it stop the whole application and replace the `.dll` and `.pdb` and then rerun application (This will make application debuggable and cross-projects compiling)

### FcsWatch-Porta (Ref FcsWatch.Core)
It is  ported from [FSharp.Compiler.PortaCode](https://github.com/fsprojects/FSharp.Compiler.PortaCode)
It sends a webhook to the host program
And then, the host program can replace its logic


## Debug in vscode(only when AutoReload is false)

### Play around
#### From source code interaction test
* git clone https://github.com/humhei/FCSWatch.git
* fcswatch --project-file "fullPath to \FCSWatch\tests\datas\TestProject\TestProject.fsproj" --debuggable
* modify fs files in any of TestProject,TestLib2,TestLib1
* Set breakpoint in any of TestProject,TestLib2,TestLib1
* F5 Debug `Launch TestProject`
* modify fs files in any of TestProject,TestLib2,TestLib1
* (Optional) add new fs file in any of TestProject,TestLib2,TestLib1
* Relaunch Debugger


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


## Plugin mode
e.g.: excelDna sample
vscode launch.json setting
```
    {
      "name": "Attach Excel",
      "type": "clr",
      "request": "attach",
      "preLaunchTask": "emitCompilerTmp",
      /// should be write automatically by script
      "processId": 14876
    },
```

build.fsx setting
```fsharp
    open FcsWatch.Binary

    let app =
        Process.GetProcesses()
        |> Array.tryFind (fun proc -> proc.ProcessName = "EXCEL")
        |> function
            | Some proc -> Marshal.GetActiveObject("Excel.Application")
            | None ->
                failwithf "Please manual open excel" projectName
        :?> Application

    let procId = User32.getPidFromHwnd app.Hwnd

    /// trigger when file changed was detected
    /// and (re)load debugger (after emit cache)
    let installPlugin() =
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


    let pluginDebugInfo: PluginDebugInfo =
        {
          /// Thread sleed to wait debugger attached.
          /// Trigger when file changed was not detected
          /// and reload debugger
          DebuggerAttachTimeDelay = 2000
          // pid write to .vscode/launch.json
          Pid = procId
          VscodeLaunchConfigurationName = "Attach Excel" }

    let plugin : DebuggingServer.Plugin =
        { Load = install
          Unload = unInstall
          Calculate = calculate
          TimeDelayAfterUninstallPlugin = 500
          PluginDebugInfo = pluginDebugInfo }

    let config =
        {Config.DefaultValue with
            DevelopmentTarget = DevelopmentTarget.autoReloadPlugin plugin }

    runFcsWatcher config projectFile

```

## Why?
why not use dotnet watch:
1. dotnet watch reference all dlls every time (which will take at least 3000ms?) (while fcs hold dlls in runtime cache)
2. not easy to debug when you are using dotnet watch


![](https://github.com/humhei/Resources/blob/Resources/TestfsFCSWatchVisualStud.gif)
