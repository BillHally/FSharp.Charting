﻿#r "PresentationFramework"
#r "WindowsBase"
#r "PresentationCore"

#load "Scripts/load-references-release.fsx"
#r "bin/Release/FSharp.Charting.wpf.dll"

open FSharp.Charting

type A =
    {
        Name  : string
        Value : int
    }

    override x.ToString() = sprintf "Name: %s Value: %d" x.Name x.Value

let data0 = [|100..200|] |> Array.map (fun x -> { Name = sprintf "Name%3d" x; Value = x})
let data1 = data0 |> Array.map (fun x -> { x with Value =   - x.Value })
let data2 = data0 |> Array.map (fun x -> { x with Value = 2 * x.Value })
let data3 = data2 |> Array.map (fun x -> { x with Value =   - x.Value })

let points0 = data0 |> Array.map (fun x -> x.Value)
let points1 = data1 |> Array.map (fun x -> x.Value)
let points2 = data2 |> Array.map (fun x -> x.Value)
let points3 = data3 |> Array.map (fun x -> x.Value)

[|
    Chart.Point (points0, Labels = (data0 |> Array.map (fun x -> x :> obj)), Title="Label example 1")
    Chart.Point (points1, Labels = (data1 |> Array.map (fun x -> x :> obj)), Title="Label example 2")
    Chart.Point (points2, Labels = (data2 |> Array.map (fun x -> x :> obj)), Title="Label example 3")
    Chart.Point (points3, Labels = (data3 |> Array.map (fun x -> x :> obj)), Title="Label example 4")
|]
//|> Chart.ShowAll (IsMaximized=true)

open OxyPlot
open OxyPlot.Annotations
open OxyPlot.Axes
open OxyPlot.Series

let createABoxplot () =

    let data =
        [
            BoxPlotItem(0.0, 740.0, 850.0, 945.0, 980.0, 1070.0, Outliers = [| 650.0               |])
            BoxPlotItem(1.0, 750.0, 805.0, 845.0, 890.0,  970.0, Outliers = [|                     |])
            BoxPlotItem(2.0, 845.0, 847.0, 855.0, 880.0,  910.0, Outliers = [| 640.0; 950.0; 970.0 |])
            BoxPlotItem(3.0, 720.0, 760.0, 820.0, 870.0,  910.0, Outliers = [|                     |])
            BoxPlotItem(4.0, 730.0, 805.0, 807.0, 870.0,  950.0, Outliers = [|                     |])
        ]
    
    [
        Chart.BoxPlot(data.[0..2], "Results", Labels = [| "1"; "2"; "3"; "4"; "5" |], Color = OxyColors.AliceBlue)
        Chart.BoxPlot(data.[3..4], Color = OxyColors.Red)
    ]
    |> Chart.Combine
    |> Chart.WithTitle("The results")
    |> Chart.WithXAxis(Title = "Experiment No.")

createABoxplot ()
|> Chart.Show (IsMaximized = true)
