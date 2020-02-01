module Dude
open FSharp.Data.UnitSystems.SI.UnitSymbols

let speed = 500.<m/s>
let create(resourceManager : IResourceManager) : Dude =
    {
        Pos = Vec.Zero
        Health = 100.<HP>
        Rads = 0.<RAD>
        Texture = resourceManager.Texture("Triangle")
        Direction = Vec.Zero
    }

let update (controller : Controller) (dt : float<s>) (this : Dude) =
    { this with
        Pos = this.Pos + controller.PlayerMoveDirection * speed * dt
        Direction = Vec.normalize(controller.PlayerCrosshairPos - this.Pos)
    }

let render (this : Dude) : Renderable list =
    [ Sprite({
        Target = SpriteTarget.World({
            Pos = this.Pos
            Size = SpriteWorldSize.Square(100.<m>)
        })
        Rotation = this.Direction |> Vec.atan
        Color = Color.White
        Layer = 0.f
        Texture = Some (this.Texture)
    }) ]

