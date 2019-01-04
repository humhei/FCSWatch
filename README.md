# FCSWatch
Run standard fsharp codes in watch mode

## MiniSample 
For a quick play around 
Try https://github.com/humhei/FcsWatchMiniSample 
    
## Get started

1. Install [fake5](https://fake.build/fake-gettingstarted.html)
2. Replace build.fsx with below codes
```fsharp
#r "paket:
nuget Atrous.Core.Utils
nuget Fake.Core.Target
nuget FcsWatch //"

#load "./.fake/build.fsx/intellisense.fsx"

// start build
open Fake.Core
open Fake.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FcsWatch.FakeHelper
open Atrous.Core.Utils.FakeHelper

let root = Path.getDirectory "./"
Target.create "FcsWatch" (fun _ ->  
    let projectFile = Path.getFullName "YourProject/YourProject.fsproj"
    dotnet root "build" ["projectFile"]
    let checker = FSharpChecker.Create()
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
    {
      "label": "emitCompilerTmp",
      "command": "curl",
      "args": ["http://localhost:8050/emitCompilerTmp"],
      "presentation": {
          "reveal": "silent"
      }
  },
```

When you are debugging files,watch mode still take effect



### Why?
why not use dotnet watch:
1. dotnet watch reference all dlls every time (which will take at least 3000ms?) (while fcs hold dlls in runtime cache)
2. not easy to debug when you are using dotnet watch


![](https://github.com/humhei/Resources/blob/Resources/TestfsFCSWatchVisualStud.gif)

