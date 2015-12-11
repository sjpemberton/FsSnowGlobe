module Mist

open Engine
open Particle
open System

let emitter = BoxEmitter {X = 0.0; Y= 50.0} {X = 1280.0; Y=100.0}
let rand = Random()

let defaultMist = 
        { Coords = defaultPoint
          Mass = 1.0
          Img = ""
          Alpha = 0.4
          AlphaMod = 0.4
          Velocity = defaultPoint
          AngularVelocity = 1.0
          Acceleration = defaultPoint
          TimeToLive = 3.0
          Life = 3.0
          Rotation = 0.0
          Scale = 0.0 
          Locked = false}

//Needs an alpha mod
let CreateMistPatch () = 
    let life = 3.0 + rand.NextDouble()
    {defaultMist with TimeToLive = life
                      Life = life
                      Coords = emitter ()
                      Velocity = toCartesian {Radius = 50.0; Theta = rand.NextDouble() * Math.PI * 2.0}
                      Rotation = (Math.PI * 2.0 * rand.NextDouble()) * 180.0 / Math.PI
                      AngularVelocity = (1.0 - rand.NextDouble() * 2.0) * 0.5
                      Scale = rand.NextDouble() + 1.0 }



let mistAnimation = new Animation(0.05, 4, CreateMistPatch, (fun _ s -> s), [], []) 