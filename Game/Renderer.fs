module Renderer

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Texture2D with
    static member size(this : Texture2D) =
        vec(float this.Width, float this.Height)

type Vec<'u> with
    static member toXNA(v: Vec<_>) = Vector2(float32 v.x, float32 v.y)

let slice x = int (round x)

[<RequireQualifiedAccess>]
module Detail =
    let segmentToComponents (beginning : Vec<_>) (ending : Vec<_>) =
        let diff = ending - beginning
        let length = diff |> Vec.length |> float |> slice
        let rotation = atan2 diff.y diff.x
        length, rotation

    let posAndSize camera resolution sprite = 
        match sprite.Target with
        | World target ->
            let size =
                match target.Size with
                | SpriteWorldSize.Square size ->
                    vec(float size * camera.Scale, float size * camera.Scale)
                | SpriteWorldSize.Rectangle (width, height) ->
                    vec(float width * camera.Scale, float height * camera.Scale)
            let pos = Camera.worldToScreen(target.Pos, resolution, camera)
            VecI.toVec pos, size
        | Screen target ->
            let size =
                match target.Size with
                | SpriteScreenSize.Square size -> vec(float size, float size)
                | SpriteScreenSize.Rectangle (width, height) -> vec(float width, float height)
            let x, y = target.Pos
            vec(float x, float y), size

    let viewportSize (viewport : Viewport) = veci(viewport.Width, viewport.Height)

type Renderer(graphicsDevice : GraphicsDevice) = 
    let spriteBatch = new SpriteBatch(graphicsDevice)
    let dummyTexture =
        let tex = new Texture2D(graphicsDevice, 1, 1)
        tex.SetData [| Color.White |]
        tex

    let resolution = graphicsDevice.Viewport |> Detail.viewportSize
    let drawSprite (camera : Camera) (sprite : Sprite) =
        let pos, size = sprite |> Detail.posAndSize camera resolution
        let texture = sprite.Texture |> Option.defaultValue dummyTexture
        let origin = texture |> Texture2D.size |> (*) 0.5 |> Vec.toXNA
        let destination = new Rectangle(slice pos.x, slice pos.y, slice size.x, slice size.y)
        do spriteBatch.Draw(
            texture, destination, System.Nullable<Rectangle>(),
            sprite.Color, float32 sprite.Rotation, origin, SpriteEffects.None,
            sprite.Layer)
    
    let drawPath camera (path : Path) =
        do path.Points
        |> Seq.pairwise
        |> Seq.iter (fun segment -> 
            let first, second = segment
            let length, rotation = Detail.segmentToComponents first second
            let pos = Camera.worldToScreen(first, resolution, camera)
            let destination = new Rectangle(pos.X, pos.Y, (float length * camera.Scale) |> slice, 1);
            do spriteBatch.Draw(
                dummyTexture, destination, System.Nullable<Rectangle>(),
                path.Color, float32 rotation, Vector2(0.f, 0.5f), SpriteEffects.None,
                path.Layer))
        
    let draw (camera : Camera) (renderable : Renderable) =
        match renderable with
        | Path path -> drawPath camera path
        | Sprite sprite -> drawSprite camera sprite

    member this.Draw (camera : Camera) (rectangles : Renderable list) = 
        graphicsDevice.Clear(Color.SkyBlue)
        spriteBatch.Begin(SpriteSortMode.FrontToBack)
        rectangles |> List.iter (draw camera)
        spriteBatch.End()

