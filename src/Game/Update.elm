module Game.Update exposing (Msg, controls, init, onAnimationFrame, subscriptions, time, update)

import Browser.Events
import EverySet
import Game.Types exposing (Flags, Hero, Model, Position)
import Gamepad exposing (Digital(..))
import Gamepad.Simple exposing (FrameStuff)
import Json.Decode as Decode exposing (Decoder)
import Set
import Time


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
            model
                |> updateTimestamp fixed
                |> updatePosition frameStuff
                |> updateAttacking frameStuff

        Resize w h ->
            { model
                | width = toFloat w
                , height = toFloat h
            }

        KeyDown key ->
            { model | keyboardPressed = EverySet.insert key model.keyboardPressed }

        KeyUp key ->
            { model | keyboardPressed = EverySet.remove key model.keyboardPressed }


updateAttacking : FrameStuff -> Model -> Model
updateAttacking _ model =
    model


updateTimestamp : FrameStuff -> Model -> Model
updateTimestamp frameStuff model =
    { model
        | now = frameStuff.timestamp
    }


updatePosition : FrameStuff -> Model -> Model
updatePosition frameStuff model =
    let
        hero : Hero
        hero =
            model.hero
    in
    if hero.waitTime > 0 then
        { model
            | hero =
                { hero | waitTime = max 0 <| hero.waitTime - frameStuff.dt }
        }

    else
        let
            onPress : Digital -> number
            onPress key =
                if isPressed key frameStuff model then
                    1

                else
                    0

            position : Position
            position =
                hero.position

            dx : number
            dx =
                onPress Gamepad.DpadRight - onPress Gamepad.DpadLeft

            newX : Int
            newX =
                (position.x + dx)
                    |> clamp 0 (model.gameWidth - 1)

            dy : number
            dy =
                onPress Gamepad.DpadDown - onPress Gamepad.DpadUp

            newY : Int
            newY =
                (position.y + dy)
                    |> clamp 0 (model.gameHeight - 1)

            newHero : Hero
            newHero =
                if newX == position.x && newY == position.y then
                    { hero | moving = False }

                else
                    let
                        free : Int -> Int -> Bool
                        free posX posY =
                            not (Set.member ( posX, posY ) model.walls)

                        newPosition : Position
                        newPosition =
                            if free newX newY then
                                { position | x = newX, y = newY }

                            else if free newX position.y then
                                { position | x = newX }

                            else if free position.x newY then
                                { position | y = newY }

                            else
                                position
                    in
                    if position == newPosition then
                        hero

                    else
                        { hero
                            | position = newPosition
                            , facingRight = newPosition.x - position.x > 0
                            , waitTime = 1000 / actionsPerSecond
                            , moving = True
                        }
        in
        { model | hero = newHero }


isPressed : Digital -> FrameStuff -> Model -> Bool
isPressed key frameStuff model =
    EverySet.member key model.keyboardPressed
        || List.any (\gamepad -> Gamepad.isPressed gamepad key) frameStuff.gamepads


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
            { position = { x = 1, y = 1 }
            , waitTime = 0
            , facingRight = True
            , moving = False
            , attacking = False
            }

        maxGameCells : number
        maxGameCells =
            360

        gameWidth : Int
        gameWidth =
            floor <| sqrt (flags.width / flags.height * maxGameCells)

        gameHeight : Int
        gameHeight =
            maxGameCells // gameWidth
    in
    { hero = hero
    , keyboardPressed = EverySet.empty
    , now = flags.now
    , width = flags.width
    , height = flags.height
    , gameWidth = gameWidth
    , gameHeight = gameHeight
    , walls =
        createWalls flags.now gameWidth gameHeight
            |> Set.fromList
    }


createWalls : Time.Posix -> Int -> Int -> List ( Int, Int )
createWalls now gameWidth gameHeight =
    let
        topLeft : ( Int, Int )
        topLeft =
            ( 0, 0 )

        topRight : ( Int, Int )
        topRight =
            ( gameWidth - 1, 0 )

        bottomLeft : ( Int, Int )
        bottomLeft =
            ( 0, gameHeight - 1 )

        bottomRight : ( Int, Int )
        bottomRight =
            ( gameWidth - 1, gameHeight - 1 )

        topWall : List ( Int, Int )
        topWall =
            wall topLeft topRight

        bottomWall : List ( Int, Int )
        bottomWall =
            wall bottomLeft bottomRight

        leftWall : List ( Int, Int )
        leftWall =
            wall topLeft bottomLeft

        rightWall : List ( Int, Int )
        rightWall =
            wall topRight bottomRight
    in
    topWall ++ bottomWall ++ leftWall ++ rightWall


wall : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
wall ( fromX, fromY ) ( toX, toY ) =
    let
        steps : Int
        steps =
            abs (toX - fromX) + abs (toY - fromY)

        dx : Int
        dx =
            (toX - fromX) // steps

        dy : Int
        dy =
            (toY - fromY) // steps
    in
    List.map
        (\i -> ( fromX + i * dx, fromY + i * dy ))
        (List.range 0 steps)


time : Model -> Time.Posix
time { now } =
    now


controls : List ( String, Gamepad.Digital )
controls =
    [ ( "Up", Gamepad.DpadUp )
    , ( "Down", Gamepad.DpadDown )
    , ( "Left", Gamepad.DpadLeft )
    , ( "Right", Gamepad.DpadRight )
    , ( "Flip", Gamepad.A )
    ]
