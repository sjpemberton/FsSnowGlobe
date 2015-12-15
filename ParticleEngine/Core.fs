[<AutoOpenAttribute>]
module Core

open System

type Vector = { X : float; Y : float }
type Polar = { Radius : float; Theta : float }

let defaultVector = { X = 0.0; Y = 0.0 }

let sum a b = { X = a.X + b.X; Y = a.Y + b.Y }
let diff a b = { X = a.X - b.X; Y = a.Y - b.Y }

let magnitude vector = sqrt (vector.X ** 2.0 + vector.Y ** 2.0)

let toPolar vec = 
    { Radius = magnitude vec
      Theta = System.Math.Atan(vec.Y / vec.X) }

let toCartesian polar = 
    { X = polar.Radius * Math.Cos(polar.Theta)
      Y = polar.Radius * Math.Sin(polar.Theta) }