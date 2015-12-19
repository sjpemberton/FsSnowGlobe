module Engine

open Particle
open System

type State = 
    { Particles : list<Particle>
      Forces : list<Vector -> Vector>
      Colliders : list<Particle -> Particle> 
      Elapsed: float}

type MouseButtonStatus = 
    | LeftDown
    | RightDown
    | Released

type EngineEvent = 
    | MouseEvent of MouseButtonStatus * position : Vector

type Animation(spawnRate, maxParticles, particleEmitter, tick, forces, colliders) = 

    let mouseEvent = new Event<EngineEvent>()
    let moveEvent = new Event<Vector>()
    
    let mutable state = 
        { Particles = list<Particle>.Empty
          Forces = forces
          Colliders = colliders
          Elapsed = 0.0 }
    
    let replace test replacement list = 
        let rec search acc = 
            function 
            | [] -> None
            | h :: t -> 
                match test h with
                | true -> Some(List.rev t @ replacement :: acc)
                | false -> search (h :: acc) t
        search [] list
    
    let rec spawnParticles toSpawn (accu : Particle list) = 
        match toSpawn with
        | a when accu.Length >= maxParticles -> 
            let replaced = replace (fun p -> p.Locked || p.TimeToLive < 0.0) (particleEmitter()) accu //TODO - lock the particle that move off the screen to clean them up easier
            match replaced with //When max, find first dead and replace it (We could move to a known position or other pool but difficult to keep in step)
            | Some replaced -> 
                if toSpawn > 1.0 then spawnParticles (toSpawn - 1.0) replaced |> List.rev
                else replaced |> List.rev
            | _ -> accu
        | b when toSpawn > 0.0 -> particleEmitter() :: accu |> spawnParticles (toSpawn - 1.0)
        | _ -> accu
    
    let applyForce particle accel force =
        particle.Coords
        |> force
        |> sum accel

    //Apply forces to particles Coords then calculate accel based on F / M (Newton's 2nd law of motion)
    //A = F/M
    let calcAcceleration particle = 
        { particle with Acceleration = 
                            state.Forces
                            |> List.fold (applyForce particle) defaultVector
                            |> fun f -> { f with X = f.X / particle.Mass
                                                 Y = f.Y / particle.Mass }}    
    let applyColliders particle = 
        particle
        |> match state.Colliders with
           | [] -> id
           | list -> (list |> List.fold (>>) id)

    let updateAlpha p =
        match 1.0 - p.TimeToLive / p.Life with
        | lifeRatio when lifeRatio <= 0.25
            -> { p with Alpha = lifeRatio / 0.25 * p.AlphaMod }
        | lifeRatio when lifeRatio >= 0.75 
            -> { p with Alpha = (1.0 - lifeRatio) / 0.25 * p.AlphaMod }
        | _ -> { p with Alpha = p.AlphaMod }

    let updatePosition delta p = 
        match p.Locked with
        | true -> { p with TimeToLive = p.TimeToLive - delta }
        | false -> 
            { p with TimeToLive = p.TimeToLive - delta
                     Coords = { X = p.Velocity.X * delta
                                Y = p.Velocity.Y * delta }
                              |> sum p.Coords 
                     Velocity = { X = p.Acceleration.X * delta
                                  Y = p.Acceleration.Y * delta }
                                |> sum p.Velocity 
                     Rotation = p.Rotation + (p.AngularVelocity * delta) }
    
    //State holds the current state of the sim - forces, particles etc
    let tick delta state = 
        let updatedState = tick delta state // Tick updates forces
        { updatedState with Particles = 
                                updatedState.Particles
                                |> List.rev
                                |> spawnParticles (delta * spawnRate)
                                |> List.map (fun p -> 
                                       calcAcceleration p
                                       |> applyColliders
                                       |> updateAlpha
                                       |> updatePosition delta) }
    
    //Public API to control when the simulation is updated from elsewhere
    member this.Update(secs) = 
        state <- tick secs {state with Elapsed = state.Elapsed + secs}
        state

    member this.RaiseMouseEvent status = status |> mouseEvent.Trigger
    member this.RaiseMoveEvent pos = moveEvent.Trigger pos
    member this.MouseEvent = mouseEvent.Publish :> IObservable<EngineEvent>
    member this.MoveEvent = moveEvent.Publish :> IObservable<_>