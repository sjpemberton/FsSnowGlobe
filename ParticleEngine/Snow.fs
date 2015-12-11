module Snow

open Engine
open Particle
open System

let emitter = BoxEmitter {X = -50.0; Y= -40.0} {X = 1280.0; Y=100.0}
let rand = Random()

let initSnowFlake () = 
    let life = 3.0 * (rand.NextDouble() * 2.0);
    {CreateSnowFlake with TimeToLive = life
                          Life = life
                          Coords = emitter ()
                          Velocity = toCartesian {Radius = 30.0; Theta = rand.NextDouble() * Math.PI * 2.0}
                          Rotation = (Math.PI * 2.0 * rand.NextDouble()) * 180.0 / Math.PI
                          Scale = rand.NextDouble() * 0.5 + 0.5 }

let floorCollision particle = 
    {particle with Locked = BoxCollider {X = 0.0; Y=670.0} {X = 1280.0; Y=720.0} particle}

let wallCollisions particle = 
    let constrainCoord min max current : float = 
        match current with
        | _ when current <= min -> min
        | _ when current >= max -> max
        | _ -> current
    {particle with 
        Coords ={ X = constrainCoord 10.0 1270.0 particle.Coords.X
                  Y = constrainCoord 10.0 710.0 particle.Coords.Y}}

let mutable colliders = [floorCollision]
let gravity = (fun p -> {X = 0.0; Y = 9.81 * 5.0})
let wind = (fun p -> {X = 0.5; Y = 0.0}) //This will vary
let mutable mouseForce = (fun p -> {X = 0.0; Y = 0.0})
let mutable moveForce = (fun p -> {X = 0.0; Y = 0.0})
let forces = [gravity; wind]

let mutable counter = 0.0
let tick delta state =
    // TODO - iteration of the snow animation 
    // Alter wind force
    counter <- counter + delta;
    let wind = (fun p -> { X = -(((Math.Sin counter) * 0.5 + 0.5) **4.0) * 100.0;
                           Y = Math.Sin(counter * 100.0) * 20.0})
    { state with Forces = [gravity; wind; mouseForce; moveForce]
                 Colliders = colliders}

//Would be nice to get a hook into the state so we didn't have to capture the mouseForce var
let applyMouseForce fEvent = 
    match fEvent with
    | Force(LeftDown, currentPos, state) ->
        mouseForce <- (fun p ->
            repulse p currentPos 20000.0 (fun d -> 1.0 / (1.0 + d)))
    | Force(RightDown, currentPos, _) ->
        mouseForce <- (fun p ->
            repulse p currentPos -20000.0 (fun d -> 1.0 / (1.0 + d)))
    |_ ->
        mouseForce <- fun _ -> {X = 0.0; Y = 0.0}
        
//Should add timer to turn off effects
let applyMoveForce points =
    let p1 = fst points
    let p2 = snd points
    match (p1.X - p2.X), (p1.Y - p2.Y) with
    | X,Y when (X < -2.0 || X > 2.0) || (Y < -2.0 || Y > 2.0) -> 
        moveForce <- fun _ -> {X = (p1.X - p2.X) * 500.0; Y = (p1.Y - p2.Y) * 500.0} 
        //colliders <- [wallCollisions]
    | _,_ -> 
        moveForce <- fun _ -> {X = 0.0; Y = 0.0}
        //colliders <- [floorCollision]

//Make this a type>???
let snowAnimation = new Animation(10.0, 1000, initSnowFlake, tick, forces, colliders)

snowAnimation.MouseEvent
|> Observable.subscribe applyMouseForce 
|> ignore

snowAnimation.MoveEvent
|> Observable.pairwise 
|> Observable.subscribe applyMoveForce
|> ignore
