#r @"src\bin\Debug\OxyPlot.dll"
#r @"src\bin\Debug\OxyPlot.Wpf.dll"
#r @"src\bin\Debug\FSharp.Charting.Wpf.dll"
//#r @"src\bin\Debug\WpfControls.dll"

#r "PresentationFramework"

open System
open FSharp.Charting
open OxyPlot

let buildChart f color = let points = [|0.0..(Math.PI / 100.0)..(2.0 * Math.PI) |] |> Array.map (fun x -> x, f x) in Chart.Line (points, Color=color)

[|
    buildChart sin OxyColors.Red  |> Chart.WithTitle "Sin"
    buildChart cos OxyColors.Blue |> Chart.WithTitle "Cos"
    [| buildChart sin OxyColors.Green; buildChart cos OxyColors.Purple |] |> Chart.Combine |> Chart.WithTitle "Combined"
|]                     
|> Chart.ShowAll (IsMaximized=true, IsModal=true)
