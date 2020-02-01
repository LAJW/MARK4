module Sentry
open FSharp.Data.UnitSystems.SI.UnitSymbols
open StateMachine

let spotDistance = 200.<m>

let waitUntilAndReturn condition : StateMachine<'u, (float<s> * 'u), 'a>  =
    sm {
        let mutable result = Unchecked.defaultof<'u>
        do! waitUntil (fun state ->
            do result <- state
            condition state)
        return result
    }

let getState() = waitUntilAndReturn (fun _ -> true)

let rec stalkDude() : StateMachine<unit, (float<s> * (Sentry * Vec<m>)), Command> = sm {
    let! sentry, dudePos = getState()
    if sentry.Pos |> Vec.inProximity dudePos spotDistance then
        yield MoveTo dudePos
        return! stalkDude()
    return! waitOnce()
}

let create (resourceManager : IResourceManager) =
    let ai : StateMachine<unit, (float<s> * (Sentry * Vec<m>)), Command> = sm {
        while true do
            let! sentry, _ = getState()
            for dest in sentry.Path do
                do! waitForOrUntil (1.<s>) (fun (sentry, dudePos) ->
                    sentry.Pos |> Vec.inProximity dudePos spotDistance)
                yield MoveTo(dest)
                let! sentry, dudePos = waitUntilAndReturn(fun (sentry, dudePos) ->
                    (sentry.Pos |> Vec.inProximity dudePos spotDistance)
                    || (sentry.Pos |> Vec.inProximity dest 10.<m>))
                do! stalkDude()
    }
    {
        Pos = Vec.Zero
        Texture = resourceManager.Texture("Triangle")
        Ai = ai
        Command = None
        Path = []
    }

let speed = 300.<m/s>

let update (dudePos : Vec<m>) (dt : float<s>) (this : Sentry) =
    let ai, commands = this.Ai |> StateMachine.step (dt, (this, dudePos))
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

