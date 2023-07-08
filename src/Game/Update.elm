module Game.Update exposing (Msg, controls, init, onAnimationFrame, subscriptions, time, update)

import Browser.Events
import EverySet
import Game.Types exposing (Flags, Hero, Model, Position)
import Gamepad exposing (Digital(..))
import Gamepad.Simple exposing (FrameStuff)
import Json.Decode as Decode exposing (Decoder)
import Random exposing (Generator)
import Random.Extra
import Set exposing (Set)
import Time


maxArea : Int
maxArea =
    16


minSideLength : Int
minSideLength =
    2


type alias Room =
    { topLeft : Position
    , bottomRight : Position
    }


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
                |> maybeReset frameStuff

        Resize w h ->
            { model
                | width = toFloat w
                , height = toFloat h
            }

        KeyDown key ->
            { model | keyboardPressed = EverySet.insert key model.keyboardPressed }

        KeyUp key ->
            { model | keyboardPressed = EverySet.remove key model.keyboardPressed }


maybeReset : FrameStuff -> Model -> Model
maybeReset frameStuff ({ hero } as model) =
    if isPressed A frameStuff model then
        { model
            | walls = createWalls model.now model.gameWidth model.gameHeight
            , hero = { hero | position = ( 1, 1 ) }
        }

    else
        model


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

            positionX : Int
            positionX =
                Tuple.first position

            positionY : Int
            positionY =
                Tuple.second position

            dx : number
            dx =
                onPress Gamepad.DpadRight - onPress Gamepad.DpadLeft

            newX : Int
            newX =
                (positionX + dx)
                    |> clamp 0 (model.gameWidth - 1)

            dy : number
            dy =
                onPress Gamepad.DpadDown - onPress Gamepad.DpadUp

            newY : Int
            newY =
                (positionY + dy)
                    |> clamp 0 (model.gameHeight - 1)

            newHero : Hero
            newHero =
                if newX == positionX && newY == positionY then
                    { hero | moving = False }

                else
                    let
                        free : Int -> Int -> Bool
                        free posX posY =
                            not (Set.member ( posX, posY ) model.walls)

                        newPosition : Position
                        newPosition =
                            if free newX newY then
                                ( newX, newY )

                            else if free newX positionY then
                                ( newX, positionY )

                            else if free positionX newY then
                                ( positionX, newY )

                            else
                                position
                    in
                    if position == newPosition then
                        hero

                    else
                        let
                            newPositionX : Int
                            newPositionX =
                                Tuple.first newPosition
                        in
                        { hero
                            | position = newPosition
                            , facingRight = newPositionX - positionX > 0
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
            { position = ( 1, 1 )
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
    , walls = createWalls flags.now gameWidth gameHeight
    }


createWalls : Time.Posix -> Int -> Int -> Set Position
createWalls now gameWidth gameHeight =
    let
        topLeft : Position
        topLeft =
            ( 0, 0 )

        topRight : Position
        topRight =
            ( gameWidth - 1, 0 )

        bottomLeft : Position
        bottomLeft =
            ( 0, gameHeight - 1 )

        bottomRight : Position
        bottomRight =
            ( gameWidth - 1, gameHeight - 1 )

        topWall : Set Position
        topWall =
            wall topLeft topRight

        bottomWall : Set Position
        bottomWall =
            wall bottomLeft bottomRight

        leftWall : Set Position
        leftWall =
            wall topLeft bottomLeft

        rightWall : Set Position
        rightWall =
            wall topRight bottomRight

        outerWalls : Set Position
        outerWalls =
            topWall
                |> Set.union bottomWall
                |> Set.union leftWall
                |> Set.union rightWall

        internalWalls : Set Position
        internalWalls =
            Random.step
                (wallGenerator topLeft bottomRight
                    |> Random.andThen (openDoors bottomRight)
                )
                (Random.initialSeed <| Time.posixToMillis now)
                |> Tuple.first
    in
    Set.union outerWalls internalWalls


openDoors : Position -> ( Set Position, List Room ) -> Generator (Set Position)
openDoors bottomRight ( walls, rooms ) =
    case rooms of
        [] ->
            Random.constant walls

        room :: tail ->
            openDoor bottomRight room walls
                |> Random.andThen
                    (\newWalls ->
                        openDoors bottomRight ( newWalls, tail )
                    )


openDoor : Position -> Room -> Set Position -> Generator (Set Position)
openDoor bottomRight room =
    let
        minX : Int
        minX =
            Tuple.first room.topLeft

        minY : Int
        minY =
            Tuple.second room.topLeft

        maxX : Int
        maxX =
            Tuple.first room.bottomRight

        maxY : Int
        maxY =
            Tuple.second room.bottomRight

        rightHole : Set Position -> Generator (Set Position)
        rightHole walls =
            if maxX /= Tuple.first bottomRight then
                List.range (minY + 1) (maxY - 1)
                    |> List.filter (\y -> not <| Set.member ( maxX + 1, y ) walls)
                    |> List.map (\y -> ( maxX, y ))
                    |> Random.Extra.sample
                    |> Random.map
                        (\maybeHole ->
                            case maybeHole of
                                Nothing ->
                                    walls

                                Just hole ->
                                    Set.remove hole walls
                        )

            else
                Random.constant walls

        bottomHole : Set Position -> Generator (Set Position)
        bottomHole walls =
            if maxY /= Tuple.second bottomRight then
                List.range (minX + 1) (maxX - 1)
                    |> List.filter (\x -> not <| Set.member ( x, maxY + 1 ) walls)
                    |> List.map (\x -> ( x, maxY ))
                    |> Random.Extra.sample
                    |> Random.map
                        (\maybeHole ->
                            case maybeHole of
                                Nothing ->
                                    walls

                                Just hole ->
                                    Set.remove hole walls
                        )

            else
                Random.constant walls
    in
    \walls ->
        walls
            |> rightHole
            |> Random.andThen bottomHole


wallGenerator :
    Position
    -> Position
    -> Generator ( Set Position, List Room )
wallGenerator (( minX, minY ) as topLeft) (( maxX, maxY ) as bottomRight) =
    let
        verticalSplit : Int -> ( Position, Position )
        verticalSplit splitY =
            ( ( minX, splitY ), ( maxX, splitY ) )

        horizontalSplit : Int -> ( Position, Position )
        horizontalSplit splitX =
            ( ( splitX, minY ), ( splitX, maxY ) )

        trySplit : Int -> Int -> (Int -> ( Position, Position )) -> Generator ( Set Position, List Room )
        trySplit minC maxC splitKind =
            List.range
                (minC + (minSideLength + 1))
                (maxC - (minSideLength + 1))
                |> Random.Extra.sample
                |> Random.andThen
                    (\maybeSplitC ->
                        case maybeSplitC of
                            Nothing ->
                                Random.constant ( Set.empty, [] )

                            Just splitC ->
                                let
                                    ( first, second ) =
                                        splitKind splitC
                                in
                                split first second
                    )

        split : Position -> Position -> Generator ( Set Position, List Room )
        split first second =
            Random.map3
                (\a ( bw, br ) ( cw, cr ) ->
                    ( Set.union a (Set.union bw cw)
                    , br ++ cr
                    )
                )
                (Random.constant (wall first second))
                (wallGenerator topLeft second)
                (wallGenerator first bottomRight)
    in
    if area topLeft bottomRight < maxArea then
        Random.constant
            ( Set.empty
            , [ { topLeft = topLeft
                , bottomRight = bottomRight
                }
              ]
            )

    else if maxY - minY > maxX - minX then
        trySplit minY maxY verticalSplit

    else
        trySplit minX maxX horizontalSplit


{-| Area of a rectangle, given points on the border.
-}
area : Position -> Position -> Int
area ( minX, minY ) ( maxX, maxY ) =
    abs (maxX - minX - 1) * abs (maxY - minY - 1)


wall : Position -> Position -> Set Position
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
        |> Set.fromList


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
