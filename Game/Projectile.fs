module Projectile
open FSharp.Data.UnitSystems.SI.UnitSymbols

let update (dt : float<s>) (this : Projectile) =
    { this with
        Pos = this.Pos + this.Direction * this.Speed * dt
        Lifespan = this.Lifespan - dt
    }

let render (this : Projectile) : Renderable list =
    []

