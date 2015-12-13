[<AutoOpenAttribute>]
module Core

open System

type Point = { X : float; Y : float }
type Polar = { Radius : float; Theta : float }
type Bounds = { Min : Point; Max : Point }

let defaultPoint = { X = 0.0; Y = 0.0 }

let sumPoints a b = 
    { X = a.X + b.X; Y = a.Y + b.Y }

let subtractPoints a b = 
    { X = a.X - b.X; Y = a.Y - b.Y }

let magnitude vector = sqrt vector.X ** 2.0 + vector.Y ** 2.0

let toPolar point = 
    { Radius = magnitude point
      Theta = System.Math.Atan(point.Y / point.X) }

let toCartesian polar = 
    { X = polar.Radius * Math.Cos(polar.Theta)
      Y = polar.Radius * Math.Sin(polar.Theta) }