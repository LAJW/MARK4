﻿module Game

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content
open Renderer
open ResourceManager
open Geometry

let resolution = veci(1500, 1000)

// Wrapper over Game1 to reduce mutability (XNA defers initialization to the
// Initialize() method which doesn't allow immutability). Initializing objects
// in the constructor throws strange exceptions
type Game2(content : ContentManager, graphicsDevice : GraphicsDevice) =
    let renderer = Renderer(graphicsDevice)
    let resourceManager = ResourceManager(content) :> IResourceManager
    let mutable camera : Camera = {
        Offset = Vec.Zero
        Scale = 1.
    }

    let concrete = resourceManager.Texture "concrete"

    let mutable world : World = {
        Dude = Dude.create(resourceManager) |> Some
        Items = [
            { Pos = vec(-0.<m>, -0.<m>)
              Chem = Ship }
            { Pos = vec(-1000.<m>, -1000.<m>)
              Chem = ShipPart }
            { Pos = vec(1000.<m>, -1000.<m>)
              Chem = ShipPart }
            { Pos = vec(0.<m>, 1200.<m>)
              Chem = ShipPart }

            { Pos = vec(-400.<m>, -400.<m>)
              Chem = Stimpack }
            { Pos = vec(-400.<m>, 400.<m>)
              Chem = RadX }
            { Pos = vec(0.<m>, 400.<m>)
              Chem = MedX }
        ]
        Enemies = [
            { Sentry.create (resourceManager) with
                Pos = vec(500.<m>, -300.<m>)
                Path = [
                    vec(500.<m>, 300.<m>)
                    vec(200.<m>, 300.<m>)
                    vec(200.<m>, -300.<m>)
                    vec(500.<m>, -300.<m>)
                ]
            }
        ]
        Projectiles = []
        CollectedParts = 0
    }

    member this.Update(gameTime : GameTime) : unit = 
        let dt = (float gameTime.ElapsedGameTime.Milliseconds) * (1.<s> / 1000.)
        let controller = Controller.create resolution camera (Mouse.GetState()) (Keyboard.GetState())
        do camera <-
            match world.Dude with
            | Some dude ->
                let diff = dude.Pos - camera.Offset
                let cameraPos = camera.Offset + diff * (2.9<1/s> * dt)
                { camera with Offset = cameraPos }
            | None -> camera
        do world <- world |> World.update resourceManager controller dt
        ()

    member this.Draw =
        let mousePos = Mouse.GetState() |> Controller.Detail.mouseStateToScreenPos
        let crosshairPos = Camera.screenToWorld(mousePos, resolution, camera)
        let cursor = Sprite({
            Target = SpriteTarget.World({
                Pos = crosshairPos
                Size = SpriteWorldSize.Square(20.<m>)
            })
            Rotation = 0.<rad>
            Color = Color.Green
            Layer = 1.f
            Texture = None
        })
        let background = Sprite({
            Target = SpriteTarget.World({
                Pos = Vec(0.<m>, 0.<m>)
                Size = SpriteWorldSize.Rectangle(6560.<m>, 4256.<m>)
            })
            Rotation = 0.<rad>
            Color = Color.Gray
            Layer = 0.f
            Texture = Some concrete
        })
        let indicator target (color : Color) = 
            let ray = camera.Offset, target
            let top = Camera.screenToWorld2 ((veci(0, 10), veci(1500, 10)), resolution, camera) 
            let bottom = Camera.screenToWorld2 ((veci(0, 990), veci(1500, 990)), resolution, camera) 
            let left = Camera.screenToWorld2 ((veci(10, 0), veci(10, 1000)), resolution, camera) 
            let right = Camera.screenToWorld2 ((veci(1490, 0), veci(1490, 1000)), resolution, camera) 
            [
                Segment.intersect ray top
                Segment.intersect ray bottom
                Segment.intersect ray left
                Segment.intersect ray right
            ]
            |> Seq.collect Option.toList
            |> Seq.tryMinBy Vec.length
            |> Option.map (fun pos -> 
                Sprite({
                    Target = SpriteTarget.World({
                        Pos = pos
                        Size = SpriteWorldSize.Square(15.<m>)
                    })
                    Rotation = 0.<rad>
                    Color = color
                    Layer = 0.8f
                    Texture = None
                })
            )
            |> Option.toList
        let indicators = world.Items |> List.collect(fun item ->
            let pos = item.Pos 
            let color = Item.color item.Chem
            indicator pos color)

        do renderer.Draw camera ([ cursor ] @ World.render world @ [ background ] @ indicators)

type Game1 () as this =
    inherit Game()
 
    let gdm =
        let gdm = new GraphicsDeviceManager(this)
        do gdm.PreferredBackBufferWidth <- 1500
        do gdm.PreferredBackBufferHeight <- 1000
        do gdm.ApplyChanges();
        gdm

    do this.Content.RootDirectory <- "Content"
    do this.IsMouseVisible <- true
    let mutable game = Unchecked.defaultof<Game2>

    override this.Initialize() =
        game <- Game2(this.Content, this.GraphicsDevice)
        do gdm.PreferredBackBufferWidth <- resolution.X
        do gdm.PreferredBackBufferHeight <- resolution.Y
        do gdm.ApplyChanges()

    override this.LoadContent() = ()
    override this.Update gameTime = game.Update(gameTime)
    override this.Draw _ = game.Draw

