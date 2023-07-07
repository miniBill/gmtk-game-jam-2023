module Game.Update exposing (Msg, init, onAnimationFrame, subscriptions, update)

import Browser.Events
import Game exposing (Flags, Hero, Model, Position)
import Gamepad exposing (Digital(..))
import Gamepad.Simple exposing (FrameStuff)
import Json.Decode as Decode exposing (Decoder)


type Msg
    = Tick FrameStuff
    | Resize Int Int
    | KeyDown Digital
    | KeyUp Digital


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick frameStuff ->
            let
                -- Avoid jumps if focus is lost
                fixed : FrameStuff
                fixed =
                    { frameStuff | dt = min 200 frameStuff.dt }
            in
            { model
                | now = fixed.timestamp
            }
                |> applyGamepadInput fixed

        Resize w h ->
            { model
                | width = toFloat w
                , height = toFloat h
            }

        KeyDown key ->
            { model | keyboardPressed = key :: model.keyboardPressed }

        KeyUp key ->
            { model | keyboardPressed = List.filter ((/=) key) model.keyboardPressed }


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


actionsPerSecond : number
actionsPerSecond =
    10


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        ]


keyDecoder : Decoder Digital
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


toDigital : String -> Maybe Digital
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


onAnimationFrame : FrameStuff -> Msg
onAnimationFrame frameStuff =
    Tick frameStuff


init : Flags -> Model
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
    in
    { hero = hero
    , keyboardPressed = []
    , now = flags.now
    , width = flags.width
    , height = flags.height
    , gameWidth = gameWidth
    , gameHeight = maxGameCells // gameWidth
    }
