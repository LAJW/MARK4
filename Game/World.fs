module World

open FSharp.Data.UnitSystems.SI.UnitSymbols

let update (controller : Controller) (dt : float<s>) (this : World) =
    { this with
        Dude = this.Dude |> Dude.update controller dt
        Enemies = this.Enemies |> List.map (Sentry.update dt)
        Projectiles = this.Projectiles |> List.map (Projectile.update dt)
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

