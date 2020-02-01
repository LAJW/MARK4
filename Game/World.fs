module World

open FSharp.Data.UnitSystems.SI.UnitSymbols

let update (resourceManager : IResourceManager) (controller : Controller) (dt : float<s>) (this : World) =
    let newDude, dudeProjectile = this.Dude |> Dude.update resourceManager controller dt
    { this with
        Dude = newDude
        Enemies = this.Enemies |> List.map (Sentry.update this.Dude.Pos dt)
        Projectiles = 
            let projectiles = this.Projectiles |> List.map (Projectile.update dt)
            projectiles @ ( dudeProjectile |> Option.toList )
    }

let render (this : World) : Renderable list =
    Seq.concat([
        [ this.Dude |> Dude.render ] |> List.toSeq
        this.Items |> Seq.map (Item.render)
        this.Projectiles |> Seq.map (Projectile.render)
        this.Enemies |> Seq.map (Sentry.render)
    ])
    |> Seq.collect id
    |> Seq.toList

