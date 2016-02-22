namespace FSharp.Charting.Controls.ViewModels

open System
open System.Reactive

open FSharp.Control.Reactive

open FSharp.Charting
open FSharp.ViewModule

open WpfControls.Editors
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

type ChartExplorerViewModel(plots : seq<ChartTypes.GenericChart>) as this =
    inherit ViewModelBase()

    let plots = plots |> Seq.map (fun x -> x.Model)

    let suggestions = PlotSuggestionProvider plots

    let plots = plots |> Seq.map Some

    let selectedPlot = plots |> Seq.choose id |> Seq.tryHead

    let selectedPlot   = this.Factory.Backing(<@ this.SelectedPlot   @>, selectedPlot)
    let selectedSearch = this.Factory.Backing(<@ this.SelectedSearch @>, NoSearch)

    let ui = System.Reactive.Concurrency.DispatcherScheduler.Current

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


    member __.Plots = plots
    member __.Suggestions = suggestions

    member __.SelectedPlot   with get () = selectedPlot.Value   and set v = selectedPlot.Value   <- v
    member __.SelectedSearch with get () = selectedSearch.Value and set v = selectedSearch.Value <- v

