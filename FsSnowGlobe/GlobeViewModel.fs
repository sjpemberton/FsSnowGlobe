namespace ViewModels

open System
open System.Windows
open FSharp.ViewModule
open FSharp.ViewModule.Validation
open FsXaml

type GlobeView = XAML<"MainWindow.xaml", true>

type GlobeViewModel() as this = 
    inherit ViewModelBase()