module Game

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content
open Renderer
open ResourceManager

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

    let mutable state = 0.<rad>

    member this.Update(gameTime : GameTime) = 
        let dt = (float gameTime.ElapsedGameTime.Milliseconds) * (1.<s> / 1000.)
        let angularSpeed = Pi * 0.5</s>
        state <- (state + angularSpeed * dt) % (2. * Pi)


    member this.Draw =
        let waifu = Sprite({
            Target = SpriteTarget.World({
                Pos = vec(0.<m>, 0.<m>)
                Size = SpriteWorldSize.Square(500.<m>)
            })
            Rotation = state
            Color = Color.White
            Layer = 0.f
            Texture = Some (resourceManager.Texture("Waifu"))
        })
        do renderer.Draw camera [ waifu ]

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
        do gdm.PreferredBackBufferWidth <- 1500
        do gdm.PreferredBackBufferHeight <- 1000
        do gdm.ApplyChanges()

    override this.LoadContent() = ()
    override this.Update gameTime = game.Update(gameTime)
    override this.Draw _ = game.Draw

