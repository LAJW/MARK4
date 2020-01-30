namespace Test.Camera

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.VisualStudio.TestTools.UnitTesting
open Test.Utils

[<TestClass>]
type Transforms() =
    let resolution = veci(1920, 1080)

    [<TestMethod>]
    member this.Center1080p() =
        let pos = resolution / 2
        let camera = {
            Offset = Vec<m>.Zero
            Scale = 1.
        }
        let result = Camera.screenToWorld(pos, resolution, camera)
        Assert.AreEqual(vec(0.<m>, 0.<m>), result)
        Assert.AreEqual(pos, Camera.worldToScreen(result, resolution, camera))

    [<TestMethod>]
    member this.TopLeft1080p() =
        let pos = veci(0, 0)
        let camera = {
            Offset = Vec<m>.Zero
            Scale = 1.
        }
        let result = Camera.screenToWorld(pos, resolution, camera)
        Assert.AreEqual(vec(1920.<m>, 1080.<m>) * -0.5, result) 
        Assert.AreEqual(pos, Camera.worldToScreen(result, resolution, camera))

    [<TestMethod>]
    member this.BottomRight1080p() =
        let pos = resolution
        let camera = {
            Offset = Vec<m>.Zero
            Scale = 1.
        }
        let result = Camera.screenToWorld(pos, resolution, camera)
        Assert.AreEqual(vec(1920.<m>, 1080.<m>) * 0.5, result) 
        Assert.AreEqual(pos, Camera.worldToScreen(result, resolution, camera))

    [<TestMethod>]
    member this.Center1080pWithOffset() =
        let pos = resolution / 2
        let camera = {
            Offset = vec(100.<m>, 100.<m>)
            Scale = 1.
        }
        let result = Camera.screenToWorld(pos, resolution, camera)
        Assert.AreEqual(vec(100.<m>, 100.<m>), result)
        Assert.AreEqual(pos, Camera.worldToScreen(result, resolution, camera))

    [<TestMethod>]
    member this.TopLeft1080pWithOffset() =
        let pos = veci(0, 0)
        let camera = {
            Offset = vec(100.<m>, 100.<m>)
            Scale = 1.
        }
        let result = Camera.screenToWorld(pos, resolution, camera)
        Assert.AreEqual(vec(1920.<m>, 1080.<m>) * -0.5 + vec(100.<m>, 100.<m>), result) 
        Assert.AreEqual(pos, Camera.worldToScreen(result, resolution, camera))

    [<TestMethod>]
    member this.BottomRight1080pWithOffset() =
        let pos = resolution
        let camera = {
            Offset = vec(100.<m>, 100.<m>)
            Scale = 1.
        }
        let result = Camera.screenToWorld(pos, resolution, camera)
        Assert.AreEqual(vec(1920.<m>, 1080.<m>) * 0.5 + vec(100.<m>, 100.<m>), result) 
        Assert.AreEqual(pos, Camera.worldToScreen(result, resolution, camera))

    [<TestMethod>]
    member this.TopLeft1080pZoom2xWithOffset() =
        let pos = veci(0, 0)
        let camera = {
            Offset = vec(100.<m>, 100.<m>)
            Scale = 2.
        }
        let result = Camera.screenToWorld(pos, resolution, camera)
        Assert.AreEqual((vec(1920.<m>, 1080.<m>) * -0.5) * 0.5 + vec(100.<m>, 100.<m>), result) 
        Assert.AreEqual(pos, Camera.worldToScreen(result, resolution, camera))

[<TestClass>]
type Update() =
    static let resolution = veci(1920, 1080)
    static let defaultUpdateInfo : Camera.UpdateInfo = {
        settings = Settings.Default
        dt = 0.01<s>
        // neutral position to prevent scrolling
        mousePos = resolution / 2
        scroll = 0
        resolution = resolution
        MapBounds = vec(0.<m>, 0.<m>), vec(2000.<m>, 2000.<m>)
    }

    static member moveMouseTestData =
        let dt = defaultUpdateInfo.dt
        let cameraSpeed = Settings.Default.CameraSpeed
        dataArray [
            veci(400, 400), vec(0.<m>, 0.<m>) // Neutral
            veci(10, 400), vec(-dt * cameraSpeed, 0.<m>) // Left
            veci(400, 10), vec(0.<m>, -dt * cameraSpeed) // Top
            veci(1910, 400), vec(dt * cameraSpeed, 0.<m>) // Right
            veci(1910, 1070), vec(1., 1.) |> Vec.normalize |> ((*) (dt * cameraSpeed)) // Diagonally BD
            veci(10, 20), vec(1., 1.) |> Vec.normalize |> ((*) (-dt * cameraSpeed)) // Diagonally TL
        ]
    [<TestMethod>]
    [<DynamicData("moveMouseTestData")>]
    member this.MouseMove_ChangePosition((mousePos, expectedDiff)) =
        let camera = {
            Offset = vec(5.<m>, 10.<m>)
            Scale = 1.
        }
        camera
        |> Camera.update { defaultUpdateInfo with mousePos = mousePos }
        |> Assert.equal { camera with Offset = vec(5.<m>, 10.<m>) + expectedDiff }

    static member scrollTestData = dataArray [
        0, 1.0
        120, 1.1
        240, 1.1 ** 2.
        360, 1.1 ** 3.
        -120, 1.1 ** -1.
        -240, 1.1 ** -2.
        -360, 1.1 ** -3.
    ]
    [<TestMethod>]
    [<DynamicData("scrollTestData")>]
    member this.Scroll_ChangeScale((scroll, expectedScale)) =
        let camera = {
            Offset = vec(5.<m>, 10.<m>)
            Scale = 1.
        }
        let dt = defaultUpdateInfo.dt
        camera
        |> Camera.update { defaultUpdateInfo with mousePos = (veci (400, 400)); scroll = scroll }
        |> Assert.equal { camera with Scale = expectedScale }

    //        Position      Expected
    [<DataRow(5.,    10.,   5.,    10)>]
    [<DataRow(-150., 10.,   -100., 10)>]
    [<DataRow(5.,    -300., 5.,    -200)>]
    [<DataRow(500.,  10.,   300.,  10)>]
    [<DataRow(5.,    1000., 5.,    400.)>]
    [<DataRow(-150., -300., -100., -200)>]
    [<DataRow(500.,  1000., 300.,  400)>]
    [<DataTestMethod>]
    member this.ConfineToMapBounds(x, y, expectedX, expectedY) =
        let camera = {
            Offset = vec(x, y)
            Scale = 1. }
        camera
        |> Camera.update
            { defaultUpdateInfo with
                MapBounds = vec(-100.<m>, -200.<m>), vec(300.<m>, 400.<m>) }
        |> Assert.equal { camera with Offset = vec(expectedX, expectedY) * 1.<m> }

