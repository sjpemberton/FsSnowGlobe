namespace ViewModels

open System
open System.Windows
open FSharp.ViewModule
open FSharp.ViewModule.Validation
open Particle
open FsXaml

type MainView = XAML<"MainWindow.xaml", true>

type MainViewController() =
    inherit WindowViewController<MainView>()

    let propagate (window:Window) (e:EventArgs) =
        let vm = window.DataContext :?> MainViewModel
        vm.RaiseWindowMove {X = window.Left; Y = window.Top}

    override this.OnLoaded view =
        view.Root.LocationChanged.Add (propagate view.Root)
