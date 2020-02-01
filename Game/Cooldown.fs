module Cooldown
open FSharp.Data.UnitSystems.SI.UnitSymbols

let create (time : float<s>) : Cooldown = {
    Total = time
    Current = 0.<_>
}

let update (trigger : bool) (dt : float<s>) (supplier : unit -> 'T) (this : Cooldown) : Cooldown * ('T option) =
    if trigger then
        let remaining = this.Current - dt
        if remaining < 0.<s> then
            { this with Current = remaining + this.Total }, Some (supplier())
        else
            { this with Current = remaining }, None
    else
        { this with Current = max (this.Current - dt) 0.<s> }, None
        
