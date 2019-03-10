# Run standard fsharp codes in watch mode

Stable | Prerelease
--- | ---
[![NuGet Badge](https://buildstats.info/nuget/FCSWatch)](https://www.nuget.org/packages/FCSWatch/) | [![NuGet Badge](https://buildstats.info/nuget/FCSWatch?includePreReleases=true)](https://www.nuget.org/packages/FCSWatch/)


MacOS/Linux | Windows
--- | ---
[![CircleCI](https://circleci.com/gh/humhei/FCSWatch.svg?style=svg)](https://circleci.com/gh/humhei/FCSWatch) | [![Build status](https://ci.appveyor.com/api/projects/status/0qnls95ohaytucsi?svg=true)](https://ci.appveyor.com/project/ts2fable-imports/FCSWatch)
[![Build History](https://buildstats.info/circleci/chart/humhei/FCSWatch)](https://circleci.com/gh/humhei/FCSWatch) | [![Build History](https://buildstats.info/appveyor/chart/ts2fable-imports/FCSWatch)](https://ci.appveyor.com/project/ts2fable-imports/FCSWatch)


---

# This repository has two parts
## [PortaCode: Compile fsharp source files to FSharp expressions](https://github.com/humhei/FCSWatch/tree/master3/src/FcsWatch/docs/Porta.md)
Mostly used by fabulous
### Features:
1. Serialiable
3. Compile time is rapid
2. Not Debuggable

## [Binary: Compile fsharp source files to (.dll and .pdb)](https://github.com/humhei/FCSWatch/tree/master3/src/FcsWatch/docs/Binary.md)
### Features:
1. Debuggable
2. Slowly Compile time
3. May only be used in vscode ?
