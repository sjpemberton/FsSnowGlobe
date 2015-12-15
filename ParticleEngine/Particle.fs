module Particle
    
type Particle = 
    { Coords : Vector
      Mass : float
      Img : string
      Alpha: float
      AlphaMod: float
      Velocity : Vector
      AngularVelocity: float
      Acceleration : Vector
      TimeToLive : float 
      Life: float
      Rotation: float
      Scale: float
      Locked: bool}
  
let repulse epicentre strength decayF vec = 
    let diff = epicentre |> diff vec 
    let distance = magnitude diff 
    let decay = decayF distance
    { X = diff.X / distance * decay * strength
      Y = diff.Y / distance * decay * strength }
    
let pointInBounds (min,max) point = 
    min.X < point.X && max.Y > point.Y && min.Y < point.Y && max.Y > point.Y
    
let BoxCollider min max particle =
    pointInBounds (min,max) particle.Coords

let BoxEmitter min max = 
    let rand = System.Random()
    fun () -> { X = float (rand.Next(int min.X, int max.X))
                Y = float (rand.Next(int min.Y, int max.Y)) }
