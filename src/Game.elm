module Game exposing (Flags, Model, Msg, Position, init, onAnimationFrame, subscriptions, time, update, view)

import Browser.Events
import Color
import Dungeon.Heroes.Knight
import Effect exposing (Effect)
import Gamepad
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
    { hero :
        { position : Position
        , waitTime : Float
        }
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
    20


gameHeight : number
gameHeight =
    20


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
        borderWidth : number
        borderWidth =
            tileSize

        exp : Float
        exp =
            min
                (logBase 2 ((width - 2 * borderWidth) / (gameWidth * tileSize)))
                (logBase 2 ((height - 2 * borderWidth) / (gameHeight * tileSize)))
    in
    2 ^ floor exp


idleFramesPerSecond : number
idleFramesPerSecond =
    4


actionsPerSecond : number
actionsPerSecond =
    10


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
    ( ( toFloat <| tileSize * position.x, toFloat <| tileSize * position.y )
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
        Tick frameStuff ->
            let
                -- Avoid jumps if focus is lost
                fixed : FrameStuff
                fixed =
                    { frameStuff | dt = min 200 frameStuff.dt }
            in
            ( { model
                | now = fixed.timestamp
              }
                |> applyGamepadInput fixed
            , Effect.none
            )

        Resize w h ->
            ( { model
                | width = toFloat w
                , height = toFloat h
              }
            , Effect.none
            )


applyGamepadInput : FrameStuff -> Model -> Model
applyGamepadInput frameStuff model =
    let
        onPress : Gamepad.Digital -> number -> number
        onPress key value =
            if List.any (\gamepad -> Gamepad.isPressed gamepad key) frameStuff.gamepads then
                value

            else
                0

        hero : { position : Position, waitTime : Float }
        hero =
            model.hero

        ( newPosition, newWaitTime ) =
            if hero.waitTime > 0 then
                ( hero.position, max 0 <| hero.waitTime - frameStuff.dt )

            else
                let
                    position : Position
                    position =
                        hero.position

                    dx : number
                    dx =
                        onPress Gamepad.DpadLeft -1 + onPress Gamepad.DpadRight 1
                in
                if dx /= 0 then
                    ( { position
                        | x =
                            (position.x + dx)
                                |> clamp 0 (gameWidth - 1)
                      }
                    , 1000 / actionsPerSecond
                    )

                else
                    let
                        dy : number
                        dy =
                            onPress Gamepad.DpadUp -1 + onPress Gamepad.DpadDown 1
                    in
                    if dy /= 0 then
                        ( { position
                            | y =
                                (position.y + dy)
                                    |> clamp 0 (gameHeight - 1)
                          }
                        , 1000 / actionsPerSecond
                        )

                    else
                        ( position, hero.waitTime )
    in
    { model
        | hero = { hero | position = newPosition, waitTime = newWaitTime }
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize Resize


init : Flags -> ( Model, Effect )
init flags =
    let
        model : Model
        model =
            { hero =
                { position = { x = 0, y = 0 }
                , waitTime = 0
                }
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
