namespace Events

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Markup
open FsXaml

type Point = 
    { X : float
      Y : float }

type DragDirection = float

type DragStatus = 
    | Dragging
    | NotDragging

type DragEvent = 
    | StatusChanged of status : DragStatus
    | PositionChanged of status : DragStatus * position : Point * DragDirection

module EventConverters = 
    let dragConverter (args : MouseEventArgs) = 
        let dragging = 
            if args.LeftButton = MouseButtonState.Pressed then DragStatus.Dragging
            else DragStatus.NotDragging
        
        let pt = args.GetPosition(args.OriginalSource :?> IInputElement)
        PositionChanged(dragging, { X = pt.X; Y = pt.Y }, 0.0)
    
    let statusConverter (args : MouseButtonEventArgs) = 
        match args.LeftButton with
        | MouseButtonState.Pressed -> StatusChanged(Dragging)
        | _ -> StatusChanged(NotDragging)
    
    let Default = StatusChanged(NotDragging)

// The converters to convert from MouseEventArgs and MouseButtonEventArgs -> DragEvent
type MoveConverter() = 
    inherit EventArgsConverter<MouseEventArgs, DragEvent>(EventConverters.dragConverter, EventConverters.Default)

type ButtonCaptureConverter() = 
    inherit EventArgsConverter<MouseButtonEventArgs, DragEvent>(EventConverters.statusConverter, EventConverters.Default)