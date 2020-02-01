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
    let projectiles, projectileEffects =
        let results = this.Projectiles |> List.map (Projectile.update this dt)
        (results |> List.choose fst, results |> List.choose snd)
    let items, itemEffects =
        match this.Dude with
        | Some dude ->
            let results = this.Items |> List.map (Item.update dude)
            (results |> List.choose fst, results |> List.choose snd)
        | None -> this.Items, []
    itemEffects @ projectileEffects
    |> List.fold
        Effect.apply
        { this with
            Dude = newDude
            Enemies = enemies
            Items = items
            Projectiles = projectiles @ enemyProjectiles @ ( dudeProjectile |> Option.toList )
        }
    |> removeDead

let renderBar (fraction) (offset : VecI) (color : Color) =
    let barMaxLength = 500
    let barLength = int (float barMaxLength * (fraction |> confine 0. 1.))
    let margin = 20
    let height = 20
    let foreground = Sprite({
        Target = SpriteTarget.Screen({
            Pos = (veci((barLength + margin) / 2, margin) + offset) |> VecI.toPair
            Size = SpriteScreenSize.Rectangle(barLength - 2, height - 2)
        })
        Rotation = 0.<rad>
        Color = color
        Layer = 1.f
        Texture = None
    })
    let background = Sprite({
        Target = SpriteTarget.Screen({
            Pos = (veci((barMaxLength + margin) / 2, margin) + offset) |> VecI.toPair
            Size = SpriteScreenSize.Rectangle(barMaxLength, height)
        })
        Rotation = 0.<rad>
        Color = Color.Gray
        Layer = 0.9f
        Texture = None
    })
    [ foreground; background ]


let render (this : World) : Renderable list =
    let healthFraction = (this.Dude |> Option.map Dude.health |> Option.defaultValue 0.<HP>) / 100.<HP>
    let radXFraction = (this.Dude |> Option.map (fun dude -> dude.RadResist / 10.<s>) |> Option.defaultValue 0.)
    Seq.concat([
        this.Dude |> Option.toList |> Seq.map Dude.render
        this.Items |> Seq.map Item.render
        this.Projectiles |> Seq.map Projectile.render
        this.Enemies |> Seq.map Sentry.render
    ])
    |> Seq.collect id
    |> Seq.append (renderBar healthFraction (veci(0, 0)) Color.LimeGreen)
    |> Seq.append (renderBar radXFraction (veci(0, 40)) Color.MonoGameOrange)
    |> Seq.toList

