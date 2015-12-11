namespace ViewModels

open System
open System.Windows
open FSharp.ViewModule
open FSharp.ViewModule.Helpers
open FsXaml
open Events
open System.Windows.Shapes
open Particle
open Mist
open System.Windows.Threading
open System.Collections.ObjectModel
open Engine
open System.Windows.Input
open Microsoft.Win32
open System.Windows.Media.Imaging

type NotifyingPoint(x,y,scale,rotation,alpha, height,width) as this = 
    inherit ViewModelBase()

    let rotation = this.Factory.Backing(<@this.Rotation@>, rotation)
    let scale = this.Factory.Backing(<@this.Scale@>, scale)
    let alpha = this.Factory.Backing(<@this.Alpha@>, alpha)
    let visible = this.Factory.Backing(<@this.Visible@>, true)
    let x = this.Factory.Backing(<@this.X@>, x)
    let y = this.Factory.Backing(<@this.Y@>, y)

    member this.Rotation with get () = rotation.Value and set (v) = rotation.Value <- v
    member this.Scale with get () = scale.Value and set (v) = scale.Value <- v
    member this.Alpha with get () = alpha.Value and set (v) = alpha.Value <- v
    member this.Visible with get () = visible.Value and set (v) = visible.Value <- v
    member this.X with get () = x.Value and set (v) = x.Value <- v
    member this.Y with get () = y.Value and set (v) = y.Value <- v

type GlobeViewModel() as this = 
    inherit EventViewModelBase<MouseEvent>() //If not using events, change base

    let img = this.Factory.Backing(<@this.BackgroundImageSource@>, new BitmapImage())
    let fps = this.Factory.Backing(<@this.Fps@>, "FPS: 0")

    let frameTimer = new DispatcherTimer()

    let createBackgroundImg () =
        let dialog = new OpenFileDialog()
        dialog.Title <- "Select an image"
        dialog.Filter <- "Images|*.jpg;*.jpeg;*.png"
        let result = dialog.ShowDialog()
        if result.HasValue && result.Value then 
            img.Value <- new BitmapImage(new Uri(dialog.FileName))

    let loadImgCommand = this.Factory.CommandSync createBackgroundImg 
  
    let snow = Snow.snowAnimation
    let mist = Mist.mistAnimation
    let particlesPerSec = 10

    let mouseCommand = FunCommand((fun o ->
        let mEvent = o :?> MouseEvent
        snow.RaiseMouseEvent mEvent.Status mEvent.Pos), fun _ -> true) :> ICommand

    let particles = ObservableCollection<NotifyingPoint>() //Snow
    let mistParticles = ObservableCollection<NotifyingPoint>() //Mist

    let mutable elapsed = 0.0
    let mutable totalElapsed = 0.0
    let mutable lastTick = 0
    let mutable currentTick = 0
    let mutable frameCount = 0
    let mutable frameCountTime = 0.0
    let mutable frameRate = 0

    let updateFps () = 
        currentTick <- Environment.TickCount;
        elapsed <- float (currentTick - lastTick) / 1000.0;
        totalElapsed <- totalElapsed + elapsed
        lastTick <- currentTick;

        frameCount <- frameCount + 1
        frameCountTime <- frameCountTime + elapsed;
        match frameCountTime with
        | a when frameCountTime >= 1.0 ->
            frameCountTime <- frameCountTime - 1.0
            frameRate <- frameCount
            frameCount <- 0
            fps.Value <- "FPS: " + frameRate.ToString()
        | _ -> ()
        

    let updateParticleUI (collection: ObservableCollection<NotifyingPoint>) particles = 
        particles 
        |> List.iteri (fun i p-> 
            match i,p with
            | x,_ when i < collection.Count ->
                collection.[i].Visible <- true
                collection.[i].X <- p.Coords.X
                collection.[i].Y <- p.Coords.Y
                collection.[i].Rotation <- p.Rotation
                collection.[i].Alpha <- p.Alpha
            | _, dead when p.TimeToLive < 0.0 && i < this.Particles.Count -> 
                collection.[i].Visible <- false
            | _,_ ->
                collection.Add(new NotifyingPoint(p.Coords.X, p.Coords.Y,p.Scale,p.Rotation, p.Alpha, Math.Round(8.0 * p.Scale, 2) ,Math.Round(8.0 * p.Scale, 2))))

    let onFrame e = 
        updateFps ()
        let snowState = snow.Update elapsed
        let mistState = mist.Update elapsed 
        snowState.Particles |> updateParticleUI particles
        mistState.Particles |> updateParticleUI mistParticles
        |> ignore

    do //TODO test timer with Asyncs
        lastTick <- Environment.TickCount;
        frameTimer.Tick.Add(onFrame);
        frameTimer.Interval <- TimeSpan.FromSeconds(0.0016);
        frameTimer.Start();
    
    member x.MouseCommand = mouseCommand
    member x.LoadImage = loadImgCommand
    member x.RaiseWindowMove(p:Point) = snow.RaiseMoveEvent p
    member x.Particles: ObservableCollection<NotifyingPoint> = particles
    member x.MistParticles: ObservableCollection<NotifyingPoint> = mistParticles
    member x.Fps with get () = fps.Value and set (v) = fps.Value <- v
    member x.BackgroundImageSource with get () = img.Value 
