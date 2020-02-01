module Sentry
open FSharp.Data.UnitSystems.SI.UnitSymbols
open StateMachine

let create (resourceManager : IResourceManager) =
    let ai : StateMachine<unit, (float<s> * Sentry), Command> = sm {
        while true do
            yield MoveTo(vec(500.<m>, 300.<m>))
            do! waitFor(5.<s>)
            yield MoveTo(vec(200.<m>, 300.<m>))
            do! waitFor(3.<s>)
            yield MoveTo(vec(200.<m>, -300.<m>))
            do! waitFor(5.<s>)
            yield MoveTo(vec(500.<m>, -300.<m>))
            do! waitFor(3.<s>)
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

