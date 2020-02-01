module Item
open FSharp.Data.UnitSystems.SI.UnitSymbols

let update (dude : Dude) (this : Item) : (Item option) * (Effect option) =
    if this.Pos |> Vec.inProximity dude.Pos 65.<m> then
        None, Some Restore
    else Some this, None

let render (this : Item) : Renderable list =
    [ Sprite({
        Target = SpriteTarget.World({
            Pos = this.Pos
            Size = SpriteWorldSize.Square(30.<m>)
        })
        Rotation = 0.<rad>
        Color = Color.LimeGreen
        Layer = 0.2f
        Texture = None
    }) ]


