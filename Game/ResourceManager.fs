module ResourceManager

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open QuantumConcepts.Formats.StereoLithography
open System.IO

type ResourceManager(content : Content.ContentManager) =
    interface IResourceManager with
        override this.Texture(name : string) =
            content.Load<Texture2D>(name)
        override this.Mesh(name : string) = 
            [| "../../Content"; name + ".stl" |] |> Path.Combine |> STLDocument.Open
