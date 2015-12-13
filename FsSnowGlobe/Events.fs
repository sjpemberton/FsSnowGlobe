namespace Events

open System.Windows.Input
open FsXaml
open Engine
open Particle

module EventConverters = 
    open System.Windows

    let mouseConverter (args : MouseEventArgs) = 
        let status = 
            if args.LeftButton = MouseButtonState.Pressed then LeftDown
            elif args.RightButton = MouseButtonState.Pressed then RightDown
            else Released
        
        let pt = args.GetPosition(args.OriginalSource :?> IInputElement)
        MouseEvent(status, { X = pt.X; Y = pt.Y })

// The converter to convert from MouseEventArgs -> EngineEvent
type MouseConverter() = 
    inherit EventArgsConverter<MouseEventArgs, EngineEvent>(EventConverters.mouseConverter, MouseEvent(Released, { X = 0.0; Y = 0.0 }))