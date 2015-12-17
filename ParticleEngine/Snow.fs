module Snow

open Engine
open Particle
open System

let emitter = BoxEmitter {X = -50.0; Y= -40.0} {X = 1280.0; Y=100.0}
let private rand = Random()

let CreateSnowFlake () = 
    let life = 4.0 * (rand.NextDouble() * 2.0)
    { Coords = emitter ()
      Img = "SnowFlake.png"
      Mass = (rand.NextDouble() / 1000.0) + 0.001
      Alpha = 1.0
      AlphaMod = 1.0
      Velocity = toCartesian {Radius = 30.0; Theta = rand.NextDouble() * Math.PI * 2.0}
      AngularVelocity = 1.0
      Acceleration = defaultVector
      TimeToLive = life
      Life = life
      Rotation = (Math.PI * 2.0 * rand.NextDouble()) * 180.0 / Math.PI
      Scale = rand.NextDouble() * 0.5 + 0.5 
      Locked = false} 

let floorCollider particle = 
    {particle with Locked = BoxCollider {X = 0.0; Y=670.0} {X = 1280.0; Y=720.0} particle}

let wallCollider particle = 
    let constrainCoord min max current : float = 
        match current with
        | _ when current <= min -> min
        | _ when current >= max -> max
        | _ -> current
    {particle with 
        Coords = { X = constrainCoord 10.0 1270.0 particle.Coords.X
                   Y = constrainCoord 10.0 710.0 particle.Coords.Y}}

let gravity p = {X = 0.0; Y = 9.81 / 60.0}
let wind p = {X = 0.5; Y = 0.0}

//Mutable forces for toggling in events
let mutable private mouseForce = fun _ -> {X = 0.0; Y = 0.0}
let mutable private moveForce = fun _ -> {X = 0.0; Y = 0.0}

let private tick delta state =
    let wind = fun _ -> { X = -(Math.Sin state.Elapsed * 0.5 + 0.5) / 10.0;
                          Y = Math.Sin(state.Elapsed * 0.5) / 10.0}
    { state with Forces = [gravity; wind; mouseForce; moveForce]}

let Animation = new Animation(10.0, 1000, CreateSnowFlake, tick, [gravity; wind], [floorCollider]) 

//Timer to reset when not dragging window
let private timer = new System.Timers.Timer(1000.0)
let resetMoveForce e =
    moveForce <- fun _ -> {X = 0.0; Y = 0.0}
    timer.Stop()
timer.Elapsed.Add resetMoveForce
let resetTimer () = 
    timer.Stop()
    timer.Start()

//Should add timer to turn off effects
let private applyMoveForce points  =
    resetTimer()
    let p1, p2 = points
    moveForce <- fun _ -> {X = (p1.X - p2.X) * 10.0; Y = (p1.Y - p2.Y) * 10.0}

let private applyMouseForce = function
    | MouseEvent(LeftDown, currentPos) ->
        mouseForce <- exertForce currentPos 200.0 (fun d -> 1.0 / (1.0 + d))
    | MouseEvent(RightDown, currentPos) ->
        mouseForce <- exertForce currentPos -200.0 (fun d -> 1.0 / (1.0 + d))
    |_ -> mouseForce <- fun _ -> {X = 0.0; Y = 0.0}

do
    Animation.MouseEvent
    |> Observable.subscribe applyMouseForce 
    |> ignore

    Animation.MoveEvent
    |> Observable.pairwise
    |> Observable.subscribe applyMoveForce
    |> ignore