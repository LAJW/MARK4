﻿[<AutoOpen>]
module DomainTypes
open FSharp.Data.UnitSystems.SI.UnitSymbols

type Color = Microsoft.Xna.Framework.Color
type Texture2D = Microsoft.Xna.Framework.Graphics.Texture2D

type SpriteWorldSize =
| Square of float<m>
| Rectangle of float<m> pair

type SpriteWorldTarget = {
    Pos : Vec<m>
    Size : SpriteWorldSize
}

type SpriteScreenSize =
| Square of int
| Rectangle of int pair

type SpriteScreenTarget = {
    Pos : int pair
    Size : SpriteScreenSize
}

type SpriteTarget =
| World of SpriteWorldTarget
| Screen of SpriteScreenTarget

type Sprite = {
    Target : SpriteTarget
    Rotation : float<rad>
    Color : Color
    Layer : float32
    Texture : Texture2D option
}

type Path = {
    Color : Color
    Points : Vec<m> list
    Layer : float32
}

type Renderable =
| Sprite of Sprite
| Path of Path
type Settings = {
    ScrollEdge : int
    CameraSpeed : float<m/s>
}

type Settings with
    static member Default = { ScrollEdge = 50; CameraSpeed = 400.<m/s> }

type Camera = {
    Offset : Vec<m>
    Scale : float
}
