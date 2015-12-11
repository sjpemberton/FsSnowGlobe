module Particle 

open System

    type Point = 
        { X : float
          Y : float }

    type Polar =
        { Radius: float
          Theta: float }

    //TODO use this?
    type Vector(x:float ,y:float) =
        member this.X = x
        member this.Y = y
        member this.magnitude = sqrt x ** 2.0 + y ** 2.0

        static member (+) (a:Vector, b:Vector) = 
            Vector(a.X + b.X, a.Y + b.Y)
        static member ToPolar (v:Vector, p:Point) =
            {Radius = v.magnitude
             Theta = System.Math.Atan (v.Y / v.X)}
        static member FromPolar(p:Polar) =
            Vector (p.Radius * Math.Cos(p.Theta), p.Radius * Math.Sin(p.Theta))

    
    type Bounds = 
        { Min : Point
          Max : Point }
    
    //Can I overload op?
    let sumPoints a b = 
        { X = a.X + b.X
          Y = a.Y + b.Y }

    let magnitude vector = sqrt (float vector.X ** 2.0 + float vector.Y ** 2.0)
    
    let toPolar point =
        {Radius = magnitude point;
         Theta = System.Math.Atan (point.Y / point.X)}

    let toCartesian polar =
        {X = polar.Radius * Math.Cos(polar.Theta)
         Y = polar.Radius * Math.Sin(polar.Theta)}

    let defaultPoint = 
        { X = 0.0
          Y = 0.0 }
    
    type Particle = 
        { Coords : Point
          Mass : float
          Img : string
          Alpha: float
          AlphaMod: float
          Velocity : Point
          AngularVelocity: float
          Acceleration : Point
          TimeToLive : float 
          Life: float
          Rotation: float
          Scale: float
          Locked: bool}
    
    let CreateSnowFlake = 
        { Coords = defaultPoint
          Mass = 1.0
          Img = ""
          Alpha = 1.0
          AlphaMod = 1.0
          Velocity = defaultPoint
          AngularVelocity = 1.0
          Acceleration = defaultPoint
          TimeToLive = 6.0
          Life = 6.0
          Rotation = 0.0
          Scale = 0.0 
          Locked = false}
    
    //Need to fade in/out?
    //Also take into account being locked (on floor)
    let UpdateParticle delta (particle : Particle) =
        let lifeRatio = 1.0 - particle.TimeToLive / particle.Life
        let np = match lifeRatio with
                 | fadeOut when fadeOut <= 0.25
                     -> { particle with Alpha = lifeRatio / 0.25 * particle.AlphaMod }
                 | constant when constant > 0.25 && constant < 0.75
                     -> { particle with Alpha = particle.AlphaMod }
                 | fadeIn when fadeIn >= 0.75 
                    -> { particle with Alpha = (1.0 - fadeIn) / 0.25 * particle.AlphaMod }

        match np.Locked with
        | true -> { np with TimeToLive = np.TimeToLive - delta}
        | false ->{ np with TimeToLive = np.TimeToLive - delta
                            Coords = sumPoints np.Coords {X = np.Velocity.X * delta; Y = np.Velocity.Y * delta} 
                            Velocity = sumPoints np.Velocity {X = np.Acceleration.X * delta; Y = np.Acceleration.Y * delta} 
                            Rotation = np.Rotation + (np.AngularVelocity * delta)}

    let Emit generator = generator
    let ApplyForce (force: Point -> Point) point = point |> force
    
    let Wind = 
        ApplyForce (fun p -> 
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
    
    let pointInBounds bounds point = 
        bounds.Min.X < point.X && bounds.Max.Y > point.Y && bounds.Min.Y < point.Y && bounds.Max.Y > point.Y
    
    let BoxCollider min max particle =
        pointInBounds {Min = min; Max = max} particle.Coords

    let BoxEmitter min max = 
        let rand = Random()
        Emit(fun () -> 
            { X = float (rand.Next(int min.X, int max.X))
              Y = float (rand.Next(int min.Y, int max.Y)) })
    


