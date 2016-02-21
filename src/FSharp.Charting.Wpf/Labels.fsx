#r "PresentationFramework"
#r "WindowsBase"
#r "PresentationCore"

#r @".\bin\Debug\OxyPlot.dll"
#r @".\bin\Debug\OxyPlot.Wpf.dll"
#r @".\bin\Debug\FSharp.Charting.Wpf.dll"
#r @"..\FSharp.Charting.Controls\bin\Debug\WpfControls.dll"
#r @"..\FSharp.Charting.Controls\bin\Debug\FSharp.Charting.Controls.dll"

open FSharp.Charting
open FSharp.Charting.Controls

type A =
    {
        Name  : string
        Value : int
    }

    override x.ToString() = sprintf "Name: %s Value: %d" x.Name x.Value

let data0 = [|100..200|] |> Array.map (fun x -> { Name = sprintf "Name%3d" x; Value = x})
let data1 = data0 |> Array.map (fun x -> { x with Value = - x.Value })

let points0 = data0 |> Array.map (fun x -> x.Value)
let points1 = data1 |> Array.map (fun x -> x.Value)

[|
    Chart.Point (points0, Labels = (data0 |> Array.map (fun x -> x :> obj)), Title="Label example 1")
    Chart.Point (points1, Labels = (data1 |> Array.map (fun x -> x :> obj)), Title="Label example 2")
|]
|> Chart.Explore (IsMaximized=true)
