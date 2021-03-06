﻿#r "PresentationFramework"
#r "WindowsBase"
#r "PresentationCore"

#load "Scripts/load-project-release.fsx"

open FSharp.Charting
open FSharp.Charting.Controls

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
|> Chart.Explore (IsMaximized=true)
