namespace FSharp.Charting.Controls

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Markup

open FsXaml

open FSharp.Charting
open FSharp.Charting.Controls.Views
open FSharp.Charting.Controls.ViewModels

type Window = XAML<"MainWindow.xaml">

[<Sealed>]
[<AbstractClass>]
type Chart =
    static member Explore (?IsMaximized : bool) =
        fun (plots : #seq<ChartTypes.GenericChart>) ->
            let window = Window().Root
            let viewModel = ChartExplorerViewModel plots
            window.DataContext <- viewModel
            window.ShowDialog() |> ignore
