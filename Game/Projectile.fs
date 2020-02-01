module Projectile
open FSharp.Data.UnitSystems.SI.UnitSymbols

let texture (resourceManager : IResourceManager) =
    resourceManager.Texture("Triangle")

let create (resourceManager : IResourceManager) =
    {
        Allied = false
        Pos = Vec.Zero
        Direction = Vec.Zero
        Speed = 0.<m/s>
        Lifespan = 0.<s>
        Texture = texture resourceManager
    }

let update (world : World) (dt : float<s>) (this : Projectile) : (Projectile option) * (Effect option) =
    let targets = 
        if this.Allied then
            world.Enemies
            |> Seq.map (fun enemy -> enemy.Pos)
            |> Seq.enumerate
            |> Seq.map(fun (index, pos) -> Id(index), pos)
        else List.toSeq[ (Id(-1), world.Dude.Pos) ]
    let collision =
        targets |> Seq.tryFind(fun (id, targetPos) -> targetPos |> Vec.inProximity this.Pos 55.<m>)
    match collision with
    | Some (id, target) -> 
        None, Some (Damage(id, 10.<HP>))
    | None ->
        let projectile = 
            Some ({
                this with
                    Pos = this.Pos + this.Direction * this.Speed * dt
                    Lifespan = this.Lifespan - dt
            })
            |> Option.filter (fun projectile -> projectile.Lifespan > 0.<s>)
        projectile, None

let render (this : Projectile) : Renderable list =
    [ Sprite({
        Target = SpriteTarget.World({
            Pos = this.Pos
            Size = SpriteWorldSize.Square(30.<m>)
        })
        Rotation = this.Direction |> Vec.atan
        Color = Color.Red
        Layer = 0.f
        Texture = Some (this.Texture)
    }) ]


