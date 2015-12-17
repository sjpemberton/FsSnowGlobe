namespace ViewModels

open System
open FSharp.ViewModule
open Particle
open System.Windows.Threading
open System.Collections.ObjectModel
open Engine
open System.Windows.Input
open Microsoft.Win32
open System.Windows.Media.Imaging

type ParticleViewModel(x,y,scale,rotation,alpha) as this = 
    inherit ViewModelBase()

    let rotation = this.Factory.Backing(<@this.Rotation@>, rotation)
    let scale = this.Factory.Backing(<@this.Scale@>, scale)
    let alpha = this.Factory.Backing(<@this.Alpha@>, alpha)
    let x = this.Factory.Backing(<@this.X@>, x)
    let y = this.Factory.Backing(<@this.Y@>, y)

    member this.Rotation with get () = rotation.Value and set (v) = rotation.Value <- v
    member this.Scale with get () = scale.Value and set (v) = scale.Value <- v
    member this.Alpha with get () = alpha.Value and set (v) = alpha.Value <- v
    member this.X with get () = x.Value and set (v) = x.Value <- v
    member this.Y with get () = y.Value and set (v) = y.Value <- v

type MainViewModel() as this = 
    inherit EventViewModelBase<EngineEvent>() //If not using events, change base

    let img = this.Factory.Backing(<@this.BackgroundImageSource@>, new BitmapImage(new Uri(new Uri(System.AppDomain.CurrentDomain.BaseDirectory), "..\\..\\fsharp2048.png")))
    let particles = ObservableCollection<ParticleViewModel>() //Snow
    let mistParticles = ObservableCollection<ParticleViewModel>() //Mist
    let frameTimer = new DispatcherTimer()

    let createBackgroundImg () =
        let dialog = new OpenFileDialog()
        dialog.Title <- "Select an image"
        dialog.Filter <- "Images|*.jpg;*.jpeg;*.png"
        let result = dialog.ShowDialog()
        if result.HasValue && result.Value then 
            img.Value <- new BitmapImage(new Uri(dialog.FileName))

    let loadImgCommand = this.Factory.CommandSync createBackgroundImg 
    let mouseCommand = FunCommand((fun o ->
        let mEvent = o :?> EngineEvent
        Snow.Animation.RaiseMouseEvent mEvent), fun _ -> true) :> ICommand
        
    // Mutable state for UI
    let updateParticleUI (collection: ObservableCollection<ParticleViewModel>) particles = 
        particles 
        |> List.rev
        |> List.iteri (fun i p-> 
            match i,p with
            | x,_ when i < collection.Count ->
                collection.[i].X <- p.Coords.X
                collection.[i].Y <- p.Coords.Y
                collection.[i].Rotation <- p.Rotation
                collection.[i].Alpha <- p.Alpha
            | _,_ ->
                collection.Add(new ParticleViewModel(p.Coords.X, p.Coords.Y,p.Scale,p.Rotation, p.Alpha)))

    let onTick (_, elapsed) =
        let snowState = Snow.Animation.Update elapsed
        let mistState = Mist.Animation.Update elapsed 
        snowState.Particles |> updateParticleUI particles
        mistState.Particles |> updateParticleUI mistParticles

    do //TODO test timer with Asyncs
        frameTimer.Tick
        |> Observable.scan (fun (previous, elapsed) _  -> 
            (float Environment.TickCount, (float Environment.TickCount - previous) / 1000.0)) (float Environment.TickCount, float Environment.TickCount)
        |> Observable.subscribe onTick
        |> ignore
        frameTimer.Interval <- TimeSpan.FromSeconds(1.0 / 60.0);
        frameTimer.Start();
    
    member x.MouseCommand = mouseCommand
    member x.LoadImage = loadImgCommand
    member x.RaiseWindowMove(v:Vector) = Snow.Animation.RaiseMoveEvent v
    member x.Particles: ObservableCollection<ParticleViewModel> = particles
    member x.MistParticles: ObservableCollection<ParticleViewModel> = mistParticles
    member x.BackgroundImageSource with get () = img.Value 
