module Game exposing (Flags, Model, Msg, Position, init, onAnimationFrame, subscriptions, time, update, view)

import Browser.Events
import Color
import Dungeon.Heroes.Knight
import Effect exposing (Effect)
import Gamepad exposing (Gamepad)
import Gamepad.Simple exposing (FrameStuff)
import Html exposing (Html)
import PixelEngine
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Options as Options
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


tileSize : number
tileSize =
    16


gameWidth : number
gameWidth =
    10


gameHeight : number
gameHeight =
    10


view : Model -> Html Msg
view model =
    PixelEngine.toHtml
        { width = gameWidth * tileSize
        , options =
            Options.default
                |> Options.withScale (maxScale model)
                |> Just
        }
        [ PixelEngine.imageArea
            { height = gameHeight * tileSize
            , background = PixelEngine.colorBackground <| Color.blue
            }
            [ viewHero model ]
        ]


maxScale : Model -> Int
maxScale { width, height } =
    let
        exp : Float
        exp =
            min
                (logBase 2 (width / (gameWidth * tileSize)))
                (logBase 2 (height / (gameHeight * tileSize)))
    in
    2 ^ floor exp


idleFramesPerSecond : number
idleFramesPerSecond =
    4


viewHero : Model -> ( ( Float, Float ), Image msg )
viewHero model =
    viewAnimated model
        { spritesheet = Dungeon.Heroes.Knight.knightIdleSpritesheet
        , position = model.hero.position
        , key = "hero"
        }


viewAnimated :
    Model
    ->
        { position : Position
        , spritesheet : { widthInTiles : Int, tileset : Tile.Tileset }
        , key : String
        }
    -> ( ( Float, Float ), Image msg )
viewAnimated model { position, spritesheet, key } =
    ( ( toFloat position.x, toFloat position.y )
    , Image.fromTile
        (Tile.fromPosition ( 0, 0 )
            |> Tile.animated
                (modBy spritesheet.widthInTiles (Time.posixToMillis model.now // (1000 // idleFramesPerSecond)))
        )
        spritesheet.tileset
        |> Image.movable key
    )


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
            let
                _ =
                    Debug.log "resize" msg
            in
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
