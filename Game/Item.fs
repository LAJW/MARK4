module Item
open FSharp.Data.UnitSystems.SI.UnitSymbols

let update (dude : Dude) (this : Item) : (Item option) * (Effect option) =
    if this.Pos |> Vec.inProximity dude.Pos 65.<m> then
        match this.Chem with
        | Stimpack -> None, Some Restore
        | RadX -> None, Some RadResist
        | MedX -> None, Some DamResist
        | ShipPart -> None, Some AddShipPart
        | Ship -> Some this, Some EnterShip
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
            | MedX -> Color.DeepSkyBlue
            | ShipPart -> Color.MediumPurple
            | Ship -> Color.DeepPink
        Layer = 0.2f
        Texture = None
    }) ]


