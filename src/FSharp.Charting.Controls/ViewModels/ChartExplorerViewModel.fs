namespace FSharp.Charting.Controls.ViewModels

open System
open System.Collections.Generic
open System.Reactive

open FSharp.Control.Reactive

open FSharp.Charting
open FSharp.Charting.ChartTypes

open FSharp.ViewModule

open WpfControls
open OxyPlot

type Search =
    | NoSearch
    | PlotSearch of PlotModel

    override this.ToString() =
        match this with
        | NoSearch -> ""
        | PlotSearch x -> x.Title

type PlotSuggestionProvider(plots : seq<PlotModel>) =
    
    interface ISuggestionProvider with
        member __.GetSuggestions filter =
            plots
            |> Seq.filter (fun x -> x.Title.ToLower().Contains(filter.ToLower()))
            |> Seq.map PlotSearch
            :> _

type ChartExplorerEvent =
    | FirstChartEvent
    | PreviousChartEvent
    | NextChartEvent
    | LastChartEvent

type ChartExplorerViewModel(charts : seq<GenericChart>) as this =
    inherit EventViewModelBase<ChartExplorerEvent>()

    let charts = charts |> Array.ofSeq
    let plots = charts |> Array.map (fun x -> x.Model)

    let suggestions = PlotSuggestionProvider plots

    let selectedPlot = plots |> Seq.tryHead

    let selectedPlot   = this.Factory.Backing(<@ this.SelectedPlot   @>, selectedPlot)
    let selectedSearch = this.Factory.Backing(<@ this.SelectedSearch @>, NoSearch)

    let ui = System.Reactive.Concurrency.DispatcherScheduler.Current

    let firstChartCommand    = this.Factory.EventValueCommand FirstChartEvent
    let previousChartCommand = this.Factory.EventValueCommand PreviousChartEvent
    let nextChartCommand     = this.Factory.EventValueCommand NextChartEvent
    let lastChartCommand     = this.Factory.EventValueCommand LastChartEvent

    //let plots = charts

    let axisMinimums =
        let map = Dictionary<IPlotModel, _ []>() // Can't use F# Map because PlotView, GenericChart etc. don't implement IComparable
                
        plots |> Array.iter
            (
                fun x ->
                    //x.Update(false) // This will force the creation of the default axes if necessary
                    map.[x] <- x.Axes |> Seq.map (fun a -> a.Minimum) |> Array.ofSeq
            )

        map

    let getPlot n =
        match n with
        | n when n < 0 -> 0
        | n when n >= plots.Length -> plots.Length - 1
        | n -> n
        |> (
            function
            | n when n >= 0 && n < plots.Length -> Some plots.[n]
            | _ -> None
            )

    let getCurrentPlotIndex, decrementPlotIndex, incrementPlotIndex, setFirstPlotIndex, setLastPlotIndex, setCurrentPlotIndex =
        let index = ref 0

        (fun () -> !index),
        (fun () -> if !index > 0 then decr index),
        (fun () -> if !index < plots.Length - 1 then incr index),
        (fun () -> index := 0),
        (fun () -> index := plots.Length - 1),
        (fun n -> if n >= 0 && n < plots.Length then index := n)

    let plots = plots |> Array.map Some

    let updateDisplay() =
        let currentIndex = getCurrentPlotIndex()
        //chartIndex.Text <- sprintf "%d of %d" (currentIndex + 1) plots.Length
        selectedPlot.Value <- plots.[currentIndex]
                
    let decrementPlotIndex  = decrementPlotIndex  >> updateDisplay
    let incrementPlotIndex  = incrementPlotIndex  >> updateDisplay
    let setFirstPlotIndex   = setFirstPlotIndex   >> updateDisplay
    let setLastPlotIndex    = setLastPlotIndex    >> updateDisplay
    let setCurrentPlotIndex = setCurrentPlotIndex >> updateDisplay 

    do
        selectedSearch
        |> Observable.observeOn ui
        |> Observable.add
            (
                fun x ->
                    printfn "%A" x
                    match x with
                    | NoSearch -> ()
                    | PlotSearch x -> selectedPlot.Value <- Some x
            )

        this.EventStream
        |> Observable.add
            (
            function
            | FirstChartEvent    -> setFirstPlotIndex  ()
            | PreviousChartEvent -> decrementPlotIndex ()
            | NextChartEvent     -> incrementPlotIndex ()
            | LastChartEvent     -> setLastPlotIndex   ()
            )

    member __.Plots = plots
    member __.Suggestions = suggestions

    member __.SelectedPlot   with get () = selectedPlot.Value   and set v = selectedPlot.Value   <- v
    member __.SelectedSearch with get () = selectedSearch.Value and set v = selectedSearch.Value <- v

    member __.FirstChartCommand    = firstChartCommand
    member __.PreviousChartCommand = previousChartCommand
    member __.NextChartCommand     = nextChartCommand
    member __.LastChartCommand     = lastChartCommand
