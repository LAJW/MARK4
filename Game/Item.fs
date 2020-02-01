module Item
open FSharp.Data.UnitSystems.SI.UnitSymbols

let update (dude : Dude) (this : Item) : (Item option) * (Effect option) =
    if this.Pos |> Vec.inProximity dude.Pos 65.<m> then
        match this.Chem with
        | Stimpack -> None, Some Restore
        | RadX -> None, Some RadResist
    else Some this, None

let render (this : Item) : Renderable list =
    [ Sprite({
        Target = SpriteTarget.World({
            Pos = this.Pos
            Size = SpriteWorldSize.Square(30.<m>)
        })
        Rotation = 0.<rad>
        Color = 
            match this.Chem with
            | Stimpack -> Color.LimeGreen
            | RadX -> Color.Orange
        Layer = 0.2f
        Texture = None
    }) ]


