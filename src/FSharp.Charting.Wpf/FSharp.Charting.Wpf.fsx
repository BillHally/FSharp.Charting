//#nowarn "211"
//#nowarn "40"

// When compiling, a reference to this is needed

//#r "FSharp.Compiler.Interactive.Settings.dll"

#I __SOURCE_DIRECTORY__
#I "bin/debug"

#r "../../packages/OxyPlot.Core/lib/net45/OxyPlot.dll"
#r "../../packages/OxyPlot.Wpf/lib/net45/OxyPlot.Wpf.dll"

//#I "./bin/Debug"
//#r "FSharp.Charting.Wpf.dll"
#load "Scripts/load-references-release.fsx"
#load "./FSharp.Charting.Wpf.fs"

open System

// Add an auto-display for things of type "GenericChart"

open FSharp.Charting
module FsiAutoShow = 
    fsi.AddPrinter(fun (ch : FSharp.Charting.ChartTypes.GenericChart) -> ch.ShowChart() |> ignore; "(Chart)")

//FSharp.Charting.Chart.Bar [0..10]
