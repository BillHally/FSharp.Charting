

#nowarn "211"
#nowarn "40"

// When compiling, a reference to this is needed

#r "FSharp.Compiler.Interactive.Settings.dll"

// On windows we just reference the DLLs. On Mono we must reference them in ../gtk-sharp-2.0 relative
// to the Mono installation.
//
// On Mono OSX/Linux we could just use
//
//#r "../gtk-sharp-2.0/gtk-sharp.dll"
//#r "../gtk-sharp-2.0/gdk-sharp.dll"
//#r "../gtk-sharp-2.0/atk-sharp.dll"
//#r "../gtk-sharp-2.0/glib-sharp.dll"
//
// and on .NET on Windows
//
//#r "gtk-sharp.dll"
//#r "gdk-sharp.dll"
//#r "atk-sharp.dll"
//#r "glib-sharp.dll"


// In F# 2.0, 3.0 and 3.1, the resolution of #r paths in #load'd scripts is NOT relative to the directory where the script
// lives. However, the resolution of #I paths is, and the #I paths have local file scope.
//
// This means that using #I __SOURCE_DIRECTORY__ is sufficient to enable local resolution of #r and #I paths within an included script file.

#I __SOURCE_DIRECTORY__
#I "bin/debug"

#I "../packages/OxyPlot.Core"
#I "../packages/OxyPlot.Wpf"

#r "lib/portable-net4+sl4+wp71+win8/OxyPlot.dll"
#r "lib/net40/OxyPlot.Wpf.dll"

#I "./bin/Debug"
#r "FSharp.Charting.Wpf.dll"

open System

// Add an auto-display for things of type "GenericChart"

open FSharp.Charting
module FsiAutoShow = 
    fsi.AddPrinter(fun (ch:FSharp.Charting.ChartTypes.GenericChart) -> ch.ShowChart() |> ignore; "(Chart)")

//FSharp.Charting.Chart.Bar [0..10]
