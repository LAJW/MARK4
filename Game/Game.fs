module Game

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content
open Renderer
open ResourceManager

let resolution = veci(1500, 1000)

// Wrapper over Game1 to reduce mutability (XNA defers initialization to the
// Initialize() method which doesn't allow immutability). Initializing objects
// in the constructor throws strange exceptions
type Game2(content : ContentManager, graphicsDevice : GraphicsDevice) =
    let renderer = Renderer(graphicsDevice)
    let resourceManager = ResourceManager(content) :> IResourceManager
    let camera : Camera = {
        Offset = Vec.Zero
        Scale = 1.
    }

    let mutable world : World = {
        Dude = Dude.create(resourceManager)
        Items = []
        Enemies = []
        Projectiles = []
    }

    member this.Update(gameTime : GameTime) : unit = 
        let dt = (float gameTime.ElapsedGameTime.Milliseconds) * (1.<s> / 1000.)
        let controller = Controller.create resolution camera (Mouse.GetState()) (Keyboard.GetState())
        do world <- world |> World.update controller dt
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
        do renderer.Draw camera ([ cursor ] @ World.render world)

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

