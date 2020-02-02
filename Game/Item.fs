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

let color (chem : Chem) = 
    match chem with
    | Stimpack -> Color.LimeGreen
    | RadX -> Color.MonoGameOrange
    | MedX -> Color.DeepSkyBlue
    | ShipPart -> Color.MediumPurple
    | Ship -> Color.DeepPink

let render (this : Item) : Renderable list =
    [ Sprite({
        Target = SpriteTarget.World({
            Pos = this.Pos
            Size = SpriteWorldSize.Square(30.<m>)
        })
        Rotation = 0.<rad>
        Color = color this.Chem
        Layer = 0.2f
        Texture = None
    }) ]


