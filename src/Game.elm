module Game exposing (Flags, Hero, Model, Msg, Position, init, onAnimationFrame, subscriptions, time, update, view)

import Browser.Events
import Color
import Dungeon.Heroes.Knight
import Effect exposing (Effect)
import Fonts
import Gamepad exposing (Digital(..))
import Gamepad.Simple exposing (FrameStuff)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import PixelEngine
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Options as Options
import PixelEngine.Tile as Tile exposing (Tileset)
import String.Extra
import Time


type alias Flags =
    { now : Time.Posix
    , width : Float
    , height : Float
    }


type Msg
    = Tick FrameStuff
    | Resize Int Int
    | KeyDown Gamepad.Digital
    | KeyUp Gamepad.Digital


type alias Model =
    { hero : Hero
    , keyboardPressed : List Digital
    , now : Time.Posix
    , width : Float
    , height : Float
    , gameWidth : Int
    , gameHeight : Int
    }


type alias Hero =
    { position : Position
    , waitTime : Float
    , facingRight : Bool
    , moving : Bool
    }


type alias Position =
    { x : Int
    , y : Int
    }


tileSize : number
tileSize =
    16


textHeight : number
textHeight =
    2


actionsPerSecond : number
actionsPerSecond =
    10


textTileset : Tileset
textTileset =
    Fonts.berlin8x8.tileset


view : Model -> Html Msg
view model =
    PixelEngine.toHtml
        { width = toFloat model.gameWidth * tileSize
        , options =
            Options.default
                |> Options.withScale (maxScale model)
                |> Options.withAnimationFPS 10
                |> Just
        }
        [ PixelEngine.imageArea
            { height = toFloat model.gameHeight * tileSize
            , background = PixelEngine.colorBackground Color.blue
            }
            [ viewHero model ]
        , viewStatusMessage model
        ]


viewStatusMessage : Model -> PixelEngine.Area Msg
viewStatusMessage model =
    let
        charsPerLine : Int
        charsPerLine =
            model.gameWidth * tileSize // textTileset.spriteHeight - 2
    in
    statusMessage model
        |> String.Extra.softBreak charsPerLine
        |> List.take textHeight
        |> List.map (Tile.fromText ( 0, 0 ))
        |> List.indexedMap
            (\row tiles ->
                List.indexedMap
                    (\column tile ->
                        ( ( 1 + column
                          , 1 + row
                          )
                        , tile
                        )
                    )
                    tiles
            )
        |> List.concat
        |> PixelEngine.tiledArea
            { rows = textHeight * 2
            , tileset = textTileset
            , background = PixelEngine.colorBackground Color.white
            }


statusMessage : Model -> String
statusMessage _ =
    "To win the game, reverse the rolls!"


maxScale : Model -> Int
maxScale model =
    let
        borderWidth : number
        borderWidth =
            tileSize

        maxScaleWidth : Float
        maxScaleWidth =
            (model.width - 2 * borderWidth)
                / (toFloat model.gameWidth * tileSize)

        maxScaleHeight : Float
        maxScaleHeight =
            (model.height - 2 * borderWidth)
                / ((toFloat model.gameHeight * tileSize)
                    + ((textHeight + 2) * toFloat textTileset.spriteHeight)
                  )

        maxScaleMin : Float
        maxScaleMin =
            min maxScaleWidth maxScaleHeight
    in
    2 ^ floor (logBase 2 maxScaleMin)


viewHero : Model -> ( ( Float, Float ), Image msg )
viewHero model =
    let
        spritesheet : { tileset : Tileset, widthInTiles : Int }
        spritesheet =
            if model.hero.moving then
                if model.hero.facingRight then
                    Dungeon.Heroes.Knight.knightRunSpritesheet

                else
                    Dungeon.Heroes.Knight.knightRunSpritesheetFlipped

            else if model.hero.facingRight then
                Dungeon.Heroes.Knight.knightIdleSpritesheet

            else
                Dungeon.Heroes.Knight.knightIdleSpritesheetFlipped
    in
    viewAnimated
        { spritesheet = spritesheet
        , position = model.hero.position
        , key = "hero"
        }


viewAnimated :
    { position : Position
    , spritesheet : { widthInTiles : Int, tileset : Tile.Tileset }
    , key : String
    }
    -> ( ( Float, Float ), Image msg )
viewAnimated { position, spritesheet, key } =
    ( ( toFloat <| tileSize * position.x, toFloat <| tileSize * position.y )
    , Image.fromTile
        (Tile.fromPosition ( 0, 0 )
            |> Tile.animated spritesheet.widthInTiles
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

        KeyDown key ->
            ( { model | keyboardPressed = key :: model.keyboardPressed }
            , Effect.none
            )

        KeyUp key ->
            ( { model | keyboardPressed = List.filter ((/=) key) model.keyboardPressed }
            , Effect.none
            )


applyGamepadInput : FrameStuff -> Model -> Model
applyGamepadInput frameStuff model =
    let
        onPress : Gamepad.Digital -> number -> number
        onPress key value =
            if
                List.member key model.keyboardPressed
                    || List.any (\gamepad -> Gamepad.isPressed gamepad key) frameStuff.gamepads
            then
                value

            else
                0

        hero : Hero
        hero =
            model.hero

        newHero : Hero
        newHero =
            if hero.waitTime > 0 then
                { hero | waitTime = max 0 <| hero.waitTime - frameStuff.dt }

            else
                let
                    position : Position
                    position =
                        hero.position

                    dx : number
                    dx =
                        onPress Gamepad.DpadLeft -1 + onPress Gamepad.DpadRight 1

                    newX : Int
                    newX =
                        (position.x + dx)
                            |> clamp 0 (model.gameWidth - 1)

                    dy : number
                    dy =
                        onPress Gamepad.DpadUp -1 + onPress Gamepad.DpadDown 1

                    newY : Int
                    newY =
                        (position.y + dy)
                            |> clamp 0 (model.gameHeight - 1)
                in
                if newX /= position.x || newY /= position.y then
                    { hero
                        | position = { position | x = newX, y = newY }
                        , facingRight = dx > 0
                        , waitTime = 1000 / actionsPerSecond
                        , moving = True
                    }

                else
                    { hero | moving = False }
    in
    { model | hero = newHero }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        ]


keyDecoder : Decoder Gamepad.Digital
keyDecoder =
    Decode.andThen
        (\key ->
            case toDigital key of
                Just digital ->
                    Decode.succeed digital

                Nothing ->
                    Decode.fail "Ignored"
        )
        (Decode.field "key" Decode.string)


toDigital : String -> Maybe Gamepad.Digital
toDigital string =
    case string of
        "ArrowLeft" ->
            Just DpadLeft

        "ArrowRight" ->
            Just DpadRight

        "ArrowUp" ->
            Just DpadUp

        "ArrowDown" ->
            Just DpadDown

        "w" ->
            Just DpadUp

        "a" ->
            Just DpadLeft

        "s" ->
            Just DpadDown

        "d" ->
            Just DpadRight

        " " ->
            Just A

        _ ->
            Nothing


init : Flags -> ( Model, Effect )
init flags =
    let
        hero : Hero
        hero =
            { position = { x = 0, y = 0 }
            , waitTime = 0
            , facingRight = True
            , moving = False
            }

        maxGameCells : number
        maxGameCells =
            180

        gameWidth : Int
        gameWidth =
            floor <| sqrt (flags.width / flags.height * maxGameCells)

        model : Model
        model =
            { hero = hero
            , keyboardPressed = []
            , now = flags.now
            , width = flags.width
            , height = flags.height
            , gameWidth = gameWidth
            , gameHeight = maxGameCells // gameWidth
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
