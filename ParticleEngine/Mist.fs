module Mist

open Engine
open Particle
open System

let emitter = BoxEmitter {X = 0.0; Y= 50.0} {X = 1280.0; Y=100.0}
let rand = Random() //TODO - Can we share?

//Needs to rework alpha and life if possible
let CreateMistPatch () = 
    let life = 3.0 + rand.NextDouble()
    { Mass = 1.0
      Img = ""
      Alpha = 0.4
      AlphaMod = 0.4
      Acceleration = defaultPoint
      Locked = false
      TimeToLive = life
      Life = life
      Coords = emitter ()
      Velocity = toCartesian {Radius = 50.0; Theta = rand.NextDouble() * Math.PI * 2.0}
      Rotation = (Math.PI * 2.0 * rand.NextDouble()) * 180.0 / Math.PI
      AngularVelocity = (1.0 - rand.NextDouble() * 2.0) * 0.5
      Scale = rand.NextDouble() + 1.0 }

let private tick delta state = state 

let Animation = new Animation(0.05, 4, CreateMistPatch, tick, [], [])