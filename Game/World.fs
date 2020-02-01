module World

open FSharp.Data.UnitSystems.SI.UnitSymbols

let removeDead (this : World) =
    { this with
        Dude = this.Dude |> Option.filter Dude.isAlive
        Enemies = this.Enemies |> List.filter Sentry.isAlive }

let update (resourceManager : IResourceManager) (controller : Controller) (dt : float<s>) (this : World) =
    let newDude, dudeProjectile =
        this.Dude
        |> Option.map (Dude.update resourceManager controller dt)
        |> Option.map (fun (dude, projectiles) -> Some dude, projectiles)
        |> Option.defaultWith (fun () -> None, None)
    let enemies, enemyProjectiles = 
        let results = this.Enemies |> List.map (Sentry.update resourceManager (this.Dude |> Option.map Dude.pos) dt)
        (results |> List.map fst, results |> List.collect snd)
    let projectiles, effects =
        let results = this.Projectiles |> List.map (Projectile.update this dt)
        (results |> List.choose fst, results |> List.choose snd)
    effects
    |> List.fold
        Effect.apply
        { this with
            Dude = newDude
            Enemies = enemies
            Projectiles = projectiles @ enemyProjectiles @ ( dudeProjectile |> Option.toList )
        }
    |> removeDead

let render (this : World) : Renderable list =
    Seq.concat([
        this.Dude |> Option.toList |> Seq.map Dude.render
        this.Items |> Seq.map Item.render
        this.Projectiles |> Seq.map Projectile.render
        this.Enemies |> Seq.map Sentry.render
    ])
    |> Seq.collect id
    |> Seq.toList

