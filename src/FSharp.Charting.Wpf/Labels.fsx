#r "PresentationFramework"
#r "WindowsBase"
#r "PresentationCore"

#load "Scripts/load-references-release.fsx"
#load "FSharp.Charting.Wpf.fs"
//#r "bin/Release/FSharp.Charting.Wpf.dll"
open System
open System.Linq

open MathNet.Numerics.Distributions
open MathNet.Numerics.Random
open MathNet.Numerics.Statistics

open OxyPlot

open FSharp.Charting

#if MEH
module ChartConfiguration =
    type Axis =
        {
            AxisTitle : string

            Minimum : float
            Maximum : float
        }

    type Data =
        | Ints        of  int              seq
        | Doubles     of  double           seq
        | DoublePairs of (double * double) seq

    type Series =
        {
            SeriesName : string
            Data       : Data
            Labels     : obj seq
        }

    type ChartConfiguration =
        {
            Title : string

            XAxis : Axis
            YAxis : Axis

            Series : Series list
        }

open ChartConfiguration

module Chart =
    let private defaultAxis =
        {
            AxisTitle = ""
            Minimum = Double.NaN
            Maximum = Double.NaN
        }

    let Default =
        {
            Title  = ""
            Series = []
            XAxis  = defaultAxis
            YAxis  = defaultAxis
        }

    let withTitle  title  x = { x with Title  = title  }
    let withData   data   x = { x with Data   = data   }
    let withLabels labels x = { x with Labels = labels }

    let point (x : Series) =
        match x.Data with
        | Ints        xs -> Chart.Point(xs, Name = x.SeriesName, Labels = x.Labels)
        | Doubles     xs -> Chart.Point(xs, Name = x.SeriesName, Labels = x.Labels)
        | DoublePairs xs -> Chart.Point(xs, Name = x.SeriesName, Labels = x.Labels)

    let toChart x =
        x.Series
        |> Seq.map point
        |> Chart.Combine
        |> Chart.WithTitle x.Title

module Series =
    let Default =
        {
            SeriesName = ""
            Data = Doubles []
            Labels = []
        }

    let withData   data   x = { x with Data       = data   }
    let withNane   name   x = { x with SeriesName = name   }
    let withLabels labels x = { x with Labels     = labels }

    let toChart x = { Chart.Default with Title = x.SeriesName; Series = [ x ] }

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

//[|
//    Chart.Point (points0, Labels = (data0 |> Array.map (fun x -> x :> obj)), Title="Label example 1")
//    Chart.Point (points1, Labels = (data1 |> Array.map (fun x -> x :> obj)), Title="Label example 2")
//    Chart.Point (points2, Labels = (data2 |> Array.map (fun x -> x :> obj)), Title="Label example 3")
//    Chart.Point (points3, Labels = (data3 |> Array.map (fun x -> x :> obj)), Title="Label example 4")
//|]
//|> Chart.Explore (IsMaximized=true)

if false then
    [
        { SeriesName = "Label example 1"; Data = Ints points0; Labels = (data0 |> Array.map (fun x -> x :> obj)) }
        { SeriesName = "Label example 2"; Data = Ints points1; Labels = (data1 |> Array.map (fun x -> x :> obj)) }
        { SeriesName = "Label example 3"; Data = Ints points2; Labels = (data2 |> Array.map (fun x -> x :> obj)) }
        { SeriesName = "Label example 4"; Data = Ints points3; Labels = (data3 |> Array.map (fun x -> x :> obj)) }
    ]
    |> List.map (Series.toChart >> Chart.toChart)
    |> Chart.ShowAll (IsMaximized=true)


/////////////////////
open System
open OxyPlot

let scalePixelValue x = x / (float UInt16.MaxValue)

let log' x = log (x + 1.0)

let normalize a b =
    let a = scalePixelValue a
    let b = scalePixelValue b
    log' (abs (1.0 - abs (a / b)))

let getPoints other =
    [0..100..(int UInt16.MaxValue)]
    |> List.map
        (
            fun x ->
                let x = float x
                let other = float other
                let normalizedRatio = normalize x other
                (x, normalizedRatio), sprintf "x: %f\r\nOther value: %f\r\nNormalized ratio: %f" x other normalizedRatio
        )

if false then    
    [0.0..1000.0..65535.0]
        |> List.map
            (
                fun x ->
                    let points, labels = getPoints x |> List.unzip

                    Chart.Point(points, Name = sprintf "%d" (int x), Labels = (labels |> Seq.map (fun x -> x :> obj)), MarkerType = MarkerType.Circle, MarkerSize = 1.0)
            )
        |> Chart.Combine
        |> Chart.Show (IsMaximized = true)

if false then
    [|
        Chart.Point (points0, Labels = (data0 |> Array.map (fun x -> x :> obj)), Title="Label example 1")
        Chart.Point (points1, Labels = (data1 |> Array.map (fun x -> x :> obj)), Title="Label example 2")
        Chart.Point (points2, Labels = (data2 |> Array.map (fun x -> x :> obj)), Title="Label example 3")
        Chart.Point (points3, Labels = (data3 |> Array.map (fun x -> x :> obj)), Title="Label example 4")
    |]
    |> Chart.ShowAll (IsMaximized=true)

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

if false then
    createABoxplot ()
    |> Chart.Show (IsMaximized = true)

let createABoxplotFromData () =

    let blueData =
        [
            "A", Normal.WithMeanVariance(50.0, 20.0).Samples().Take(10000).ToArray()
            "B", Normal.WithMeanVariance(75.0, 10.0).Samples().Take(10000).ToArray()
        ]

    let greenData =
        [
            "A", Normal.WithMeanVariance(50.0, 20.0).Samples().Take(10000).ToArray()
            "B", Normal.WithMeanVariance(75.0, 10.0).Samples().Take(10000).ToArray()
        ]

    [
        Chart.BoxPlotFromData(blueData,  "Blue",        Color = OxyColors.AliceBlue,     ShowUnusualValues = true, XOffset = -0.4, BoxWidth = 0.15)
        Chart.BoxPlotFromData(greenData, "Green",       Color = OxyColors.Green,         ShowUnusualValues = true, XOffset = -0.2, BoxWidth = 0.15)
        Chart.BoxPlotFromData(blueData,  "Blue (alt)",  Color = OxyColors.BlueViolet,    ShowUnusualValues = true, XOffset =  0.2, BoxWidth = 0.15, Percentile=30, WhiskerPercentile = 1)
        Chart.BoxPlotFromData(greenData, "Green (alt)", Color = OxyColors.LightSeaGreen, ShowUnusualValues = true, XOffset =  0.4, BoxWidth = 0.15, Percentile=30, WhiskerPercentile = 1)
    ]
    |> Chart.Combine
    |> Chart.WithTitle("The results")
    |> Chart.WithXAxis(Title = "Experiment No.")

if false then
    createABoxplotFromData ()
    |> Chart.Show (IsMaximized = true)

if false then
    let blueData =
        [
            1.0, Normal.WithMeanVariance(50.0, 20.0).Samples().Take(10000).ToArray()
            2.5, Normal.WithMeanVariance(75.0, 10.0).Samples().Take(10000).ToArray()
        ]

    let greenData =
        [
            1.0, Normal.WithMeanVariance(50.0, 20.0).Samples().Take(10000).ToArray()
            2.5, Normal.WithMeanVariance(75.0, 10.0).Samples().Take(10000).ToArray()
        ]

    [
        Chart.BoxPlotFromData(blueData,  "Blue",        Color = OxyColors.AliceBlue,     ShowUnusualValues = true, XOffset = -0.4, BoxWidth = 0.15)
        Chart.BoxPlotFromData(greenData, "Green",       Color = OxyColors.Green,         ShowUnusualValues = true, XOffset = -0.2, BoxWidth = 0.15)
        Chart.BoxPlotFromData(blueData,  "Blue (alt)",  Color = OxyColors.BlueViolet,    ShowUnusualValues = true, XOffset =  0.2, BoxWidth = 0.15, Percentile=30, WhiskerPercentile = 1)
        Chart.BoxPlotFromData(greenData, "Green (alt)", Color = OxyColors.LightSeaGreen, ShowUnusualValues = true, XOffset =  0.4, BoxWidth = 0.15, Percentile=30, WhiskerPercentile = 1)
    ]
    |> Chart.Combine
    |> Chart.WithTitle("The results")
    |> Chart.WithXAxis(Title = "Experiment No.")
    |> Chart.Show (IsMaximized = true)
#endif

if true then
    let data =
        [|
            for x in 0..1000..65535 do
                yield (x, Normal.WithMeanVariance(float x, 100000000.0).Samples().Take(10000) |> Seq.map int |> Array.ofSeq)
        |]

    Chart.BoxPlotFromData(data, "Blue", Color = OxyColors.AliceBlue, ShowUnusualValues = true, BoxWidth = 65535.0 / ((float data.Length)) * 0.8, WhiskerPercentile = 1, MaxOutliers = 100, ShowAverage = true)
    |> Chart.WithTitle("The results")
    |> Chart.WithXAxis(Title = "Pixel value", LabelAngle = -90.0)
    |> Chart.WithYAxis(Title = "Something",   LabelAngle = +15.7)
    |> Chart.Show (IsMaximized = true)
