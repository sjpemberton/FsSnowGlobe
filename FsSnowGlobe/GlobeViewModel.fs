namespace ViewModels

open System
open System.Windows
open FSharp.ViewModule
open FSharp.ViewModule.Validation
open FsXaml
open Events

type GlobeView = XAML<"MainWindow.xaml", true>

type GlobeViewModel() as this = 
    inherit EventViewModelBase<DragEvent>()

    let exitCommand =  this.Factory.CommandSync (fun () ->  Application.Current.Shutdown()) 
    let dragComand = this.Factory.CommandSync (fun () -> Application.Current.MainWindow.DragMove())
    let mouseCommand = this.Factory.EventValueCommand()
        
    let handleMove = function
        | PositionChanged(DragStatus.Dragging, currentPos, 0.0), PositionChanged(_, previousPos, 0.0) -> 
            () //Do Drag
        | _, _ -> 
            ()

    // Handle F# drag events
    do 
        this.EventStream
        |> Observable.pairwise //Grabbing pair of events to handle diff
        |> Observable.subscribe handleMove
        |> ignore
    
    member x.MouseCommand = mouseCommand
    member x.ExitCommand = exitCommand
    member x.DragCommand = dragComand