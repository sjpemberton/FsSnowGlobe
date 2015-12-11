namespace Events

open System
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Markup
open FsXaml
open Engine
open Particle

type DragDirection = float

type MouseEvent = 
    { Status : MouseStatus
      Pos : Point }

module EventConverters = 
    open System.Windows

    let forceConverter (args : MouseEventArgs) = 
        let status = 
            match args with
            | repulse when args.LeftButton = MouseButtonState.Pressed -> MouseStatus.LeftDown
            | attract when args.RightButton = MouseButtonState.Pressed -> MouseStatus.RightDown
            | _ -> MouseStatus.Released
        
        let pt = args.GetPosition(args.OriginalSource :?> IInputElement)
        { Status = status
          Pos = { X = pt.X; Y = pt.Y } }
    
    let DefaultMouseStatus = 
        { Status = Released
          Pos = { X = 0.0; Y = 0.0 } }

    let moveConverter (args : EventArgs) = 
        {X = 0.0; Y = 0.0}

// The converters to convert from MouseEventArgs and MouseButtonEventArgs -> DragEvent
type MouseConverter() = 
    inherit EventArgsConverter<MouseEventArgs, MouseEvent>(EventConverters.forceConverter, EventConverters.DefaultMouseStatus)


type MoveConverter() = 
    inherit EventArgsConverter<EventArgs, Point>(EventConverters.moveConverter, {X = 0.0; Y = 0.0})