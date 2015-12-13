module Particle
    
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
  
let private updateAlpha p =
    match 1.0 - p.TimeToLive / p.Life with
    | lifeRatio when lifeRatio <= 0.25
        -> { p with Alpha = lifeRatio / 0.25 * p.AlphaMod }
    | lifeRatio when lifeRatio >= 0.75 
        -> { p with Alpha = (1.0 - lifeRatio) / 0.25 * p.AlphaMod }
    | _ -> { p with Alpha = p.AlphaMod }

let private updatePosition delta p =
    match p.Locked with
    | true -> { p with TimeToLive = p.TimeToLive - delta}
    | false ->{ p with TimeToLive = p.TimeToLive - delta
                       Coords = sumPoints p.Coords {X = p.Velocity.X * delta; Y = p.Velocity.Y * delta} 
                       Velocity = sumPoints p.Velocity {X = p.Acceleration.X * delta; Y = p.Acceleration.Y * delta} 
                       Rotation = p.Rotation + (p.AngularVelocity * delta)}

let UpdateParticle delta (particle : Particle) =
    particle
    |> updateAlpha
    |> updatePosition delta

let repulse epicentre strength decayF point = 
    let diff = subtractPoints point epicentre
    let distance = magnitude diff 
    let decay = decayF distance
    { X = diff.X / distance * decay * strength
      Y = diff.Y / distance * decay * strength }
    
let pointInBounds bounds point = 
    bounds.Min.X < point.X && bounds.Max.Y > point.Y && bounds.Min.Y < point.Y && bounds.Max.Y > point.Y
    
let BoxCollider min max particle =
    pointInBounds {Min = min; Max = max} particle.Coords

let BoxEmitter min max = 
    let rand = System.Random()
    fun () -> { X = float (rand.Next(int min.X, int max.X))
                Y = float (rand.Next(int min.Y, int max.Y)) }
