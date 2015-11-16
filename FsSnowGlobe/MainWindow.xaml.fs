namespace ViewModels

open System
open System.Windows
open FSharp.ViewModule
open FSharp.ViewModule.Validation
open FsXaml

type MainView = XAML<"MainWindow.xaml", true>

type MainViewModel() as this = 
    inherit ViewModelBase()

    let exitCommand =  this.Factory.CommandSync (fun () ->  Application.Current.Shutdown()) 
    let dragComand = this.Factory.CommandSync (fun () -> Application.Current.MainWindow.DragMove())

    member x.ExitCommand = exitCommand
    member x.DragCommand = dragComand