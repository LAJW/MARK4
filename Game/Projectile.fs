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

let update (dt : float<s>) (this : Projectile) =
    { this with
        Pos = this.Pos + this.Direction * this.Speed * dt
        Lifespan = this.Lifespan - dt
    }

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


