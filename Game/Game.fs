module Game

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content
open Renderer
open ResourceManager

let resolution = veci(1500, 1000)

let mouseStateToScreenPos (mouseState : MouseState) =
    veci(mouseState.X, mouseState.Y)

let keyboardStateToDirection(keyboardState : KeyboardState) : Vec<1> =
    let up = keyboardState.IsKeyDown(Keys.W)
    let down = keyboardState.IsKeyDown(Keys.S) 
    let left = keyboardState.IsKeyDown(Keys.A) 
    let right = keyboardState.IsKeyDown(Keys.D)
    let x =
        if left && not right then -1
        else if not left && right then 1
        else 0
    let y =
        if up && not down then -1
        else if not up && down then 1
        else 0
    veci(x, y) |> VecI.toVec |> Vec.normalize

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

    let mutable waifuPos = vec(0.<m>, 0.<m>)

    member this.Update(gameTime : GameTime) = 
        let dt = (float gameTime.ElapsedGameTime.Milliseconds) * (1.<s> / 1000.)
        let direction = Keyboard.GetState() |> keyboardStateToDirection
        let speed = 500.<m/s>
        waifuPos <- waifuPos + direction * speed * dt
        ()

    member this.Draw =
        let mousePos = Mouse.GetState() |> mouseStateToScreenPos
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
        let waifu = Sprite({
            Target = SpriteTarget.World({
                Pos = waifuPos
                Size = SpriteWorldSize.Square(500.<m>)
            })
            Rotation = 0.<rad>
            Color = Color.White
            Layer = 0.f
            Texture = Some (resourceManager.Texture("Waifu"))
        })
        do renderer.Draw camera [ cursor; waifu ]

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

