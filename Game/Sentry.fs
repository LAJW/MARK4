module Sentry
open FSharp.Data.UnitSystems.SI.UnitSymbols

let create (resourceManager : IResourceManager) =
    {
        Pos = Vec.Zero
        Texture = resourceManager.Texture("Triangle")
    }

let update (dt : float<s>) (this : Sentry) =
    this

let render (this : Sentry) : Renderable list =
    [ Sprite({
        Target = SpriteTarget.World({
            Pos = this.Pos
            Size = SpriteWorldSize.Square(100.<m>)
        })
        Rotation = 0.<rad>
        Color = Color.Orange
        Layer = 0.f
        Texture = Some (this.Texture)
    }) ]

