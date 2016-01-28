#r "PresentationFramework"
#r @".\bin\Release\OxyPlot.dll"
#r @".\bin\Release\OxyPlot.Wpf.dll"
#r @".\bin\Release\FSharp.Charting.Wpf.dll"

open FSharp.Charting

type A =
    {
        Name  : string
        Value : int
    }

    override x.ToString() = sprintf "Name: %s Value: %d" x.Name x.Value

let data = [|100..200|] |> Array.map (fun x -> { Name = sprintf "Name%3d" x; Value = x})

let points = data |> Array.map (fun x -> x.Value)

Chart.Point (points, Labels = (data |> Array.map (fun x -> x :> obj))) |> Chart.Show (IsMaximized=true)
