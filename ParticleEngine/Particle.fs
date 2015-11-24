namespace ParticleEngine

module particle = 
    type Point = 
        { X : float
          Y : float }
    
    type Bounds = 
        { Min : Point
          Max : Point }
    
    let sumPoints a b = 
        { X = a.X + b.X
          Y = a.Y + b.Y }

    let magnitude vector = sqrt (float vector.X ** 2.0 + float vector.Y ** 2.0)
    
    let defaultPoint = 
        { X = 0.0
          Y = 0.0 }
    
    type Particle = 
        { Coords : Point
          Mass : int
          Img : string
          Velocity : Point
          AngularVelocity: float
          Acceleration : Point
          TimeToLive : int 
          Rotation: float}
    
    let CreateSnowFlake = 
        { Coords = defaultPoint
          Mass = 1
          Img = ""
          Velocity = defaultPoint
          AngularVelocity = 1.0
          Acceleration = defaultPoint
          TimeToLive = 20
          Rotation = 0.0 }
    
    let UpdateParticle(particle : Particle) = 
        { particle with Coords = sumPoints particle.Coords particle.Velocity 
                        Velocity = sumPoints particle.Velocity particle.Acceleration
                        Rotation = particle.Rotation + particle.AngularVelocity }

    //type IEmitter = 
    //    abstract Generate : unit -> Point
    //
    //type BoxEmitter(width, height) = 
    //    interface IEmitter with
    //        member x.Generate() = 
    //            let rand = System.Random()
    //            { X = rand.Next(0, width)
    //              Y = rand.Next(0, height) }
    let Emit generator = generator
    let ApplyForce point force = point |> force
    
    let Wind = 
        ApplyForce defaultPoint (fun p -> 
            { X = 1.0
              Y = 0.0 })
    
    let repulse point epicentre strength decayF = 
        let diff = 
            { X = point.X - epicentre.X
              Y = point.Y - epicentre.Y }
        
        let distance = magnitude diff 
        let decay = decayF distance
        { X = diff.X / distance * decay * strength
          Y = diff.Y / distance * decay * strength }
    
    let Repulsion point = 
        ApplyForce point (fun p -> 
            repulse p { X = 0.0
                        Y = 0.0 } 2.1 (fun d -> 1.0 / (1.0 + d))) //Needs mouse coords
    
    let pointInBounds bounds point = 
        bounds.Min.X < point.X && bounds.Max.Y > point.Y && bounds.Min.Y < point.Y && bounds.Max.Y > point.Y
    
    let BoxEmitter w h = 
        let rand = System.Random()
        Emit(fun () -> 
            { X = float (rand.Next(0, w))
              Y = float (rand.Next(0, h)) })
    
    module Animation = 
        let spawnRate = 5

