module Game exposing (Flags, Model, Msg, Position, init, loadedTexture, onAnimationFrame, subscriptions, time, update, view)

import Browser.Events
import Dict
import Effect exposing (Effect)
import Gamepad exposing (Gamepad)
import Gamepad.Simple exposing (FrameStuff)
import Html exposing (Html)
import PixelEngine exposing (Area)
import PixelEngine.Tile as Tile
import Time


type alias Flags =
    { now : Time.Posix
    , width : Float
    , height : Float
    }


type Msg
    = Tick FrameStuff
    | Resize Int Int


type alias Model =
    { hero : { position : Position }
    , now : Time.Posix
    , width : Float
    , height : Float
    }


type alias Position =
    { x : Int
    , y : Int
    }


view : Model -> Html Msg
view model =
    PixelEngine.toHtml
        { width = model.width
        , options = Nothing
        }
        [ viewHero model ]


viewHero : Model -> Area msg
viewHero { hero, now } =
    Tile.fromPosition ( 0, 0 )
        |> Tile.animated 0


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        Tick ({ gamepads } as frameStuff) ->
            -- let
            --     deltaT : Duration
            --     deltaT =
            --         frameStuff.dt
            --             -- Avoid jumps if focus is lost
            --             |> min 200
            --             |> Duration.milliseconds
            -- in
            ( { model
                | now = frameStuff.timestamp
              }
                |> applyGamepadInput gamepads
                |> resetBall gamepads
            , Effect.none
            )

        Resize w h ->
            ( { model
                | width = toFloat w
                , height = toFloat h
              }
            , Effect.none
            )


resetBall : List Gamepad -> Model -> Model
resetBall gamepads model =
    if
        List.any
            (\gamepad -> Gamepad.wasReleased gamepad Gamepad.B)
            gamepads
    then
        model

    else
        (\a -> a) model


applyGamepadInput : List Gamepad -> Model -> Model
applyGamepadInput gamepads model =
    gamepadToAccelleration gamepads model


gamepadToAccelleration : List Gamepad -> Model -> Model
gamepadToAccelleration gamepads model =
    if
        List.any
            (\gamepad -> Gamepad.isPressed gamepad Gamepad.LeftStickLeft)
            gamepads
    then
        model

    else
        (\a -> a) model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize Resize


init : Flags -> ( Model, Effect )
init flags =
    let
        model : Model
        model =
            { hero = { position = { x = 0, y = 0 } }
            , now = flags.now
            , width = flags.width
            , height = flags.height
            , textures = Dict.empty
            }
    in
    ( model
    , Effect.none
    )


time : Model -> Time.Posix
time { now } =
    now


onAnimationFrame : FrameStuff -> Msg
onAnimationFrame frameStuff =
    Tick frameStuff


loadedTexture : String -> Result Texture.Error Texture -> Msg
loadedTexture key content =
    LoadedTexture key content
