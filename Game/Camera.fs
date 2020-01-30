module Camera

open FSharp.Data.UnitSystems.SI.UnitSymbols

module Detail =
    let scrollDirection edge value res =
        if value < edge then -1.
        elif value > res - edge then 1.
        else 0.

    // Default scrolling distance on Windows
    let defaultScrollDistance = 120

    let scaleMultiplier scrollDistance =
        let n = float scrollDistance / float defaultScrollDistance
        1.1 ** n

let screenToWorld
    (screenPos : VecI, 
     resolution : VecI,
     camera : Camera) : Vec<m> =
    let pos = VecI.toVec screenPos
    let res = VecI.toVec resolution
    (pos - (res * 0.5)) * (1. / camera.Scale) * 1.<m> + camera.Offset

/// Convert an area (pair of cooridnates) into a area in world space
let screenToWorld2 (area : VecI pair, resolution, camera) : Vec<m> pair =
    area |> Pair.map(fun pos -> screenToWorld(pos, resolution, camera))

let worldToScreen
    (pos : Vec<m>, 
     resolution : VecI,
     camera : Camera) : VecI =
    let res = VecI.toVec resolution
    let screenPos = (pos - camera.Offset) * camera.Scale * 1.<1/m> + (res * 0.5)
    veci(int screenPos.x, int screenPos.y)

type UpdateInfo = {
    settings : Settings
    dt : float<s>
    mousePos : VecI
    scroll : int
    resolution : VecI
    MapBounds : Vec<m> pair
}

let update (info : UpdateInfo) (camera : Camera) : Camera =
    let x, y = VecI.toPair info.mousePos
    let resX, resY = VecI.toPair info.resolution
    let scrollDirection = Detail.scrollDirection info.settings.ScrollEdge
    let cameraSpeed = info.settings.CameraSpeed 
    let cameraDirection =
        vec(scrollDirection x resX, scrollDirection y resY)
        |> Vec.normalize
    { Offset =
        camera.Offset + cameraDirection * cameraSpeed * info.dt * (1. / camera.Scale)
        |> Vec.confine info.MapBounds
      Scale = camera.Scale * Detail.scaleMultiplier info.scroll }
    
