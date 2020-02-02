module Sentry
open FSharp.Data.UnitSystems.SI.UnitSymbols
open StateMachine

let spotDistance = 300.<m>

let waitUntilAndReturn condition : StateMachine<'u, (float<s> * 'u), 'a>  =
    sm {
        // TODO: Make this work with the type system
        let mutable result = Unchecked.defaultof<'u>
        do! waitUntil (fun state ->
            do result <- state
            condition state)
        return result
    }

let getState() = waitUntilAndReturn (fun _ -> true)

let rec stalkDude() : StateMachine<unit, (float<s> * (Sentry * Vec<m> option)), Command> = sm {
    let! sentry, maybeDudePos = getState()
    match maybeDudePos with
    | Some dudePos -> 
        if sentry.Pos |> Vec.inProximity dudePos spotDistance then
            yield MoveTo dudePos
            yield Shoot dudePos
            return! stalkDude()
    | None -> ()
}

let create (resourceManager : IResourceManager) =
    let ai : StateMachine<unit, (float<s> * (Sentry * Vec<m> option)), Command> = sm {
        while true do
            let! sentry, _ = getState()
            for dest in sentry.Path do
                do! waitForOrUntil (1.<s>) (fun (sentry, dudePos) ->
                    match dudePos with
                    | Some dudePos -> sentry.Pos |> Vec.inProximity dudePos spotDistance
                    | None -> false)
                yield MoveTo dest
                let! sentry, dudePos = waitUntilAndReturn(fun (sentry, dudePos) ->
                    if sentry.Pos |> Vec.inProximity dest 10.<m> then
                        true
                    else
                        match dudePos with
                        | Some dudePos -> (sentry.Pos |> Vec.inProximity dudePos spotDistance)
                        | None -> false
                )
                do! stalkDude()
    }
    {
        Pos = Vec.Zero
        Texture = resourceManager.Texture("Triangle")
        Ai = ai
        Command = None
        Path = []
        Health = 20.<HP>
        Cooldown = Cooldown.create(1.<s>)
        Direction = Vec.Zero
    }

let speed = 300.<m/s>

let update (resourceManager : IResourceManager) (dudePos : Vec<m> option) (dt : float<s>) (this : Sentry) : Sentry * (Projectile list) =
    let createProjectile(direction) = {
        Projectile.create resourceManager with
            Allied = false
            Pos = this.Pos
            Direction = direction
            Speed = 1000.<m/s>
            Lifespan = 5.<s> }
    let cooldown, _ = this.Cooldown |> Cooldown.update false dt (fun () -> createProjectile Vec.Zero)
    let newAi, commands = this.Ai |> StateMachine.step (dt, (this, dudePos))
    if commands.IsEmpty then
        this.Command |> Option.toList
    else commands
    |> Seq.fold
        (fun (this, projectiles) command ->
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
                    Command = Some command
                    Direction = if direction = Vec.Zero then this.Direction else direction
                }, projectiles
            | Shoot dest ->
                let direction = Vec.normalize(dest - this.Pos)
                let cooldown, projectile = this.Cooldown |> Cooldown.update true dt (fun () -> createProjectile direction)
                { this with Cooldown = cooldown }, projectiles @ (Option.toList projectile)
        )
        ({ this with Ai = newAi; Cooldown = cooldown }, [])

let render (this : Sentry) : Renderable list =
    [ Sprite({
        Target = SpriteTarget.World({
            Pos = this.Pos
            Size = SpriteWorldSize.Square(100.<m>)
        })
        Rotation = this.Direction |> Vec.atan
        Color = Color.Orange
        Layer = 0.2f
        Texture = Some (this.Texture)
    }) ]

let isAlive (this : Sentry) = 
    this.Health > 0.<HP>

