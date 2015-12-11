module Engine

open Particle
open System

type State = 
    { Particles : list<Particle>
      Forces : list<Point -> Point>
      Colliders : list<Particle -> Particle> }

type MouseStatus = 
    | LeftDown
    | RightDown
    | Released

type ForceEvent = 
    | StatusChanged of MouseStatus
    | Force of MouseStatus * position : Point * State

let spawnRate = 5

type Animation(spawnRate, maxParticles, particleGen, tick, forces, colliders) = 
    let mouseEvent = new Event<ForceEvent>()
    let moveEvent = new Event<Point>()
    let particles = list<Particle>.Empty
    let mutable spawnCount = 0
    
    let mutable state = 
        { Particles = particles
          Forces = forces
          Colliders = colliders }
    
    let replace test replaceWith list = 
        let rec search acc = 
            function 
            | [] -> None
            | h :: t -> 
                match test h with
                | true -> Some(List.rev t @ replaceWith :: acc)
                | false -> search (h :: acc) t
        search [] list
    
    let spawnParticle() = 
        spawnCount <- spawnCount + 1 // NOT GREAT TO HAVE A SIDE EFFECT!
        particleGen()
    
    let rec spawnParticles toSpawn accu = 
        match spawnCount, toSpawn with
        | sc, _ when spawnCount >= maxParticles -> 
            let rep = replace (fun p -> p.Locked || p.TimeToLive < 0.0) (spawnParticle()) accu //TODO - lock the particle that move off the screen to clean them up easier
            match rep with //When max, find first dead and replace it (We could move to a know position or other pool but difficult to keep in step)
            | Some rep -> 
                if toSpawn > 1.0 then 
                    rep
                    |> spawnParticles (toSpawn - 1.0)
                else rep
            | _ -> accu |> List.rev
                    
        | _, b when toSpawn > 0.0 -> spawnParticle() :: accu |> spawnParticles (toSpawn - 1.0)
        | _ -> accu |> List.rev //Must flip back
    
    //Apply forces to particles Coords then calculate accel based on F / M (Newton's 2nd law of motion)
    let calcAcceleration particle = 
        let force = 
            state.Forces |> List.fold (fun a fc -> 
                                particle.Coords
                                |> fc
                                |> sumPoints a) defaultPoint
        { //A = F/M
          particle with Acceleration = 
                            { force with X = force.X / particle.Mass
                                         Y = force.Y / particle.Mass } }
    
    let compose = 
        function 
        | [] -> fun p -> p
        | list -> (list |> List.fold (>>) (fun f -> f))
    
    let applyColliders particle = particle |> compose state.Colliders
    
    //State holds the current state of the sim - forces, particles etc
    let tick secs state = 
        let updatedState = tick secs state // Tick updates forces
        { updatedState with Particles = 
                                spawnParticles (secs * spawnRate) (updatedState.Particles |> List.rev) //Flip so oldest particles are processed first
                                |> List.map (fun p -> 
                                       p
                                       |> calcAcceleration
                                       |> applyColliders
                                       |> UpdateParticle secs) }
    
    member this.Update(secs) = 
        state <- tick secs state
        state
    
    member this.RaiseMouseEvent status pos = Force(status, pos, state) |> mouseEvent.Trigger
    member this.RaiseMoveEvent pos = moveEvent.Trigger pos
    member this.MouseEvent = mouseEvent.Publish :> IObservable<ForceEvent>
    member this.MoveEvent = moveEvent.Publish :> IObservable<_>
    member this.GetState() = state