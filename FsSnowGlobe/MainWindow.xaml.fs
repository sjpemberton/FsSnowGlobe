namespace ViewModels

open System
open System.Windows
open FSharp.ViewModule
open FSharp.ViewModule.Validation
open Particle
open FsXaml

type MainViewBase = XAML<"MainWindow.xaml">

type MainView() =
    inherit MainViewBase()

    let propagate (window:Window) (e:EventArgs) =
        let vm = window.DataContext :?> MainViewModel
        vm.RaiseWindowMove {X = window.Left; Y = window.Top}

    override this.OnInitialize () =
        this.LocationChanged.Add (propagate this)
