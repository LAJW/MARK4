module Sentry
open FSharp.Data.UnitSystems.SI.UnitSymbols
open StateMachine

let create (resourceManager : IResourceManager) =
    let ai : StateMachine<unit, (float<s> * Sentry), Command> = sm {
        let path = [
            vec(500.<m>, 300.<m>)
            vec(200.<m>, 300.<m>)
            vec(200.<m>, -300.<m>)
            vec(500.<m>, -300.<m>)
        ]
        while true do
            for dest in path do
                yield MoveTo(dest)
                do! waitFor(0.5<s>)
                do! waitUntil(fun sentry -> sentry.Pos |> Vec.inProximity dest 10.<m>)
    }
    {
        Pos = Vec.Zero
        Texture = resourceManager.Texture("Triangle")
        Ai = ai
        Command = None
    }

let speed = 300.<m/s>

let update (dt : float<s>) (this : Sentry) =
    let ai, commands = this.Ai |> StateMachine.step (dt, this)
    let maybeCommand = commands |> List.tryLast |> Option.orElse this.Command
    match maybeCommand with
    | Some command ->
        match command with
        | MoveTo dest ->
            let direction = Vec.normalize (dest - this.Pos)
            { this with
                Pos = 
                    let newPos = this.Pos + direction * speed * dt
                    let remaining = dest - this.Pos
                    if (Vec.length remaining) < speed * dt then
                        dest
                    else newPos
                Ai = ai
                Command = Some command
            }
    | None -> { this with Ai = ai }

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

