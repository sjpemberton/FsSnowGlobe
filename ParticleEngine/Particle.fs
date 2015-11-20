namespace ParticleEngine

module particle =

    type Point = {X:int; Y:int}
    type Bounds = {Min:Point; Max:Point}

    //type IEmitter = 
    //    abstract Generate : unit -> Point
    //
    //type BoxEmitter(width, height) = 
    //    interface IEmitter with
    //        member x.Generate() = 
    //            let rand = System.Random()
    //            { X = rand.Next(0, width)
    //              Y = rand.Next(0, height) }
            

    let Emit generator =
        generator

    let collides bounds point =
        bounds.Min.X < point.X && bounds.Max.Y > point.Y
        && bounds.Min.Y < point.Y && bounds.Max.Y > point.Y
        
    let BoxEmitter w h = 
        let rand = System.Random()
        Emit (fun () -> { X = rand.Next(0, w)
                          Y = rand.Next(0, h) })

