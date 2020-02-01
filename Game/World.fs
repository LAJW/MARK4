module World

open FSharp.Data.UnitSystems.SI.UnitSymbols

let update (resourceManager : IResourceManager) (controller : Controller) (dt : float<s>) (this : World) =
    let newDude, dudeProjectile = this.Dude |> Dude.update resourceManager controller dt
    let enemies, enemyProjectiles = 
        let results = this.Enemies |> List.map (Sentry.update resourceManager this.Dude.Pos dt)
        (results |> List.map fst, results |> List.collect snd)
    let projectiles, effects =
        let results = this.Projectiles |> List.map (Projectile.update this dt)
        (results |> List.choose fst, results |> List.map snd)
    { this with
        Dude = newDude
        Enemies = enemies
        Projectiles = projectiles @ enemyProjectiles @ ( dudeProjectile |> Option.toList )
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

