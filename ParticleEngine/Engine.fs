module Engine

open Particle
open System

type State = 
    { Particles : list<Particle>
      Forces : list<Point -> Point>
      Colliders : list<Particle -> Particle> 
      Counter: float}

type MouseButtonStatus = 
    | LeftDown
    | RightDown
    | Released

type EngineEvent = 
    | MouseEvent of MouseButtonStatus * position : Point

type Animation(spawnRate, maxParticles, particleEmitter, tick, forces, colliders) as x = 

    let mouseEvent = new Event<EngineEvent>()
    let moveEvent = new Event<Point>()
    
    let mutable state = 
        { Particles = list<Particle>.Empty
          Forces = forces
          Colliders = colliders
          Counter = 0.0 }
    
    let replace test replaceWith list = 
        let rec search acc = 
            function 
            | [] -> None
            | h :: t -> 
                match test h with
                | true -> Some(List.rev t @ replaceWith :: acc)
                | false -> search (h :: acc) t
        search [] list
    
    let rec spawnParticles toSpawn (accu: Particle list) = 
        match toSpawn with
        | a when accu.Length >= maxParticles -> 
            let replaced = replace (fun p -> p.Locked || p.TimeToLive < 0.0) (particleEmitter()) accu //TODO - lock the particle that move off the screen to clean them up easier
            match replaced with //When max, find first dead and replace it (We could move to a know position or other pool but difficult to keep in step)
            | Some replaced -> 
                if toSpawn > 1.0 then spawnParticles (toSpawn - 1.0) replaced else replaced
            | _ -> accu |> List.rev
        | b when toSpawn > 0.0 -> 
            particleEmitter() :: accu |> spawnParticles (toSpawn - 1.0)
        | _ -> accu |> List.rev //Must flip back
    
    let applyForce particle accel force =
        particle.Coords
        |> force
        |> sumPoints accel

    //Apply forces to particles Coords then calculate accel based on F / M (Newton's 2nd law of motion)
    //A = F/M
    let calcAcceleration particle = 
        { particle with Acceleration = 
                            state.Forces
                            |> List.fold (applyForce particle) defaultPoint
                            |> fun f -> { f with X = f.X / particle.Mass
                                                 Y = f.Y / particle.Mass }}
    
    
    let applyColliders particle = 
        particle
        |> match state.Colliders with
           | [] -> id
           | list -> (list |> List.fold (>>) id)
    
    //State holds the current state of the sim - forces, particles etc
    let tick secs state = 
        let updatedState = tick secs state // Tick updates forces
        { updatedState with Particles = 
                                spawnParticles (secs * spawnRate) (updatedState.Particles |> List.rev) //Flip so oldest particles are processed first
                                |> List.map (fun p -> 
                                       calcAcceleration p
                                       |> applyColliders
                                       |> UpdateParticle secs) }
    
    //Public API to control when the simulation is updated from elsewhere
    member this.Update(secs) = 
        state <- tick secs state
        state

    member this.RaiseMouseEvent status = status |> mouseEvent.Trigger
    member this.RaiseMoveEvent pos = moveEvent.Trigger pos
    member this.MouseEvent = mouseEvent.Publish :> IObservable<EngineEvent>
    member this.MoveEvent = moveEvent.Publish :> IObservable<_>