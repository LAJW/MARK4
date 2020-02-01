module Controller

open Microsoft.Xna.Framework.Input

[<RequireQualifiedAccess>]
module Detail = 
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

let create (resolution : VecI) (camera : Camera) (mouseState : MouseState) (keyboardState : KeyboardState) : Controller =
    let mouseScreenPos = mouseState |> Detail.mouseStateToScreenPos
    {
        PlayerCrosshairPos = Camera.screenToWorld(mouseScreenPos, resolution, camera)
        PlayerMoveDirection = keyboardState |> Detail.keyboardStateToDirection
        Shooting = mouseState.LeftButton = ButtonState.Pressed
    }


