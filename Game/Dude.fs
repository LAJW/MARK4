module Dude
open FSharp.Data.UnitSystems.SI.UnitSymbols

let speed = 500.<m/s>
let create(resourceManager : IResourceManager) : Dude =
    {
        Pos = Vec.Zero
        Health = 100.<HP>
        Rads = 0.<RAD>
        Texture = resourceManager.Texture("Triangle")
        Direction = Vec.Zero
        WeaponCooldown = Cooldown.create(0.5<s>)
    }

let update (resourceManager : IResourceManager) (controller : Controller) (dt : float<s>) (this : Dude) : Dude * (Projectile option) =
    let direction = Vec.normalize(controller.PlayerCrosshairPos - this.Pos)
    let cooldown, projectile =
        this.WeaponCooldown |> Cooldown.update controller.Shooting dt (fun () -> {
            Projectile.create resourceManager with
                Allied = true
                Pos = this.Pos
                Direction = direction
                Speed = 1000.<m/s>
                Lifespan = 5.<s>
        })
    let newThis = {
        this with
            Pos = this.Pos + controller.PlayerMoveDirection * speed * dt
            Direction = direction
            WeaponCooldown = cooldown
    }
    newThis, projectile

let render (this : Dude) : Renderable list =
    [ Sprite({
        Target = SpriteTarget.World({
            Pos = this.Pos
            Size = SpriteWorldSize.Square(100.<m>)
        })
        Rotation = this.Direction |> Vec.atan
        Color = Color.White
        Layer = 0.f
        Texture = Some (this.Texture)
    }) ]

let isAlive (this : Dude) = 
    this.Health > 0.<HP>

let pos (this : Dude) = this.Pos

let health (this : Dude) = this.Health
