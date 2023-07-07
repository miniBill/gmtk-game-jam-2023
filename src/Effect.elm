module Effect exposing (Config, Effect(..), loadTextures, none, toCmd)

import Task
import WebGL.Texture as Texture exposing (Texture)


type alias Config msg =
    { loadedTexture : String -> Result Texture.Error Texture -> msg
    }


type Effect
    = Batch (List Effect)
    | LoadTexture String String


none : Effect
none =
    batch []


batch : List Effect -> Effect
batch =
    Batch


toCmd : Config msg -> Effect -> Cmd msg
toCmd config effect =
    case effect of
        Batch children ->
            Cmd.batch (List.map (toCmd config) children)

        LoadTexture key content ->
            Texture.loadWith Texture.nonPowerOfTwoOptions content
                |> Task.attempt (config.loadedTexture key)


loadTextures : List ( String, String ) -> Effect
loadTextures textures =
    textures
        |> List.map loadTexture
        |> batch


loadTexture : ( String, String ) -> Effect
loadTexture ( key, content ) =
    LoadTexture key content
