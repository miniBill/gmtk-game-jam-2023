module Game.Update exposing (Msg, controls, init, onAnimationFrame, subscriptions, time, update)

import Browser.Events
import Dict exposing (Dict)
import EverySet
import Game.Types exposing (Flags, Hero, Model, Position, Roll)
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
                |> updateReversing frameStuff
                |> maybeReset frameStuff
                |> moveToPrevious

        Resize w h ->
            { model
                | width = toFloat w
                , height = toFloat h
            }

        KeyDown key ->
            { model | keyboardPressed = EverySet.insert key model.keyboardPressed }

        KeyUp key ->
            { model | keyboardPressed = EverySet.remove key model.keyboardPressed }


moveToPrevious : Model -> Model
moveToPrevious model =
    { model
        | previous =
            { keyboardPressed = model.keyboardPressed
            , heroPosition = model.heroPosition
            }
    }


maybeReset : FrameStuff -> Model -> Model
maybeReset frameStuff model =
    if isPressed Back frameStuff model then
        regen model

    else
        model


regen : Model -> Model
regen model =
    let
        ( walls, rooms ) =
            generateWalls model.now model.gameWidth model.gameHeight
    in
    { model
        | walls = walls
        , rolls = createRolls model.now rooms
        , heroPosition = ( 1, 1 )
    }


updateReversing : FrameStuff -> Model -> Model
updateReversing _ model =
    let
        tryFlip : Position -> Dict Position Roll -> Dict Position Roll
        tryFlip position rolls =
            case
                Dict.get position rolls
            of
                Just roll ->
                    Dict.insert
                        position
                        { roll | reversed = True }
                        rolls

                _ ->
                    rolls

        diagonals : Maybe ( Position, Position )
        diagonals =
            let
                ( currX, currY ) =
                    model.heroPosition

                ( prevX, prevY ) =
                    model.previous.heroPosition

                diag1 : Position
                diag1 =
                    ( prevX, currY )

                diag2 : Position
                diag2 =
                    ( currX, prevY )
            in
            if
                (currX /= prevX)
                    && (currY /= prevY)
                    && (Set.member diag1 model.walls || Set.member diag2 model.walls)
            then
                Just ( diag1, diag2 )

            else
                Nothing

        normalFlip : Dict Position Roll
        normalFlip =
            case diagonals of
                Nothing ->
                    model.rolls
                        |> tryFlip model.heroPosition

                Just ( diag1, diag2 ) ->
                    model.rolls
                        |> tryFlip model.heroPosition
                        |> tryFlip diag1
                        |> tryFlip diag2

        newModel : Model
        newModel =
            { model
                | rolls =
                    normalFlip
            }
    in
    if
        List.all
            (\( _, { reversed } ) -> reversed)
            (Dict.toList newModel.rolls)
    then
        toLevel (model.level + 1) newModel

    else
        newModel


toLevel : Int -> Model -> Model
toLevel level model =
    let
        diff : Int
        diff =
            level - model.level
    in
    (if model.gameWidth > model.gameHeight then
        { model
            | level = level
            , gameWidth = model.gameWidth + diff
            , gameHeight =
                (toFloat (model.gameWidth + diff)
                    * model.height
                    / model.width
                )
                    |> floor
        }

     else
        { model
            | level = level
            , gameHeight = model.gameHeight + diff
            , gameWidth =
                (toFloat (model.gameHeight + diff)
                    * model.width
                    / model.height
                )
                    |> floor
        }
    )
        |> regen


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

            positionX : Int
            positionX =
                Tuple.first model.heroPosition

            positionY : Int
            positionY =
                Tuple.second model.heroPosition

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

            ( newHero, newPosition ) =
                if newX == positionX && newY == positionY then
                    ( { hero | moving = False }, model.heroPosition )

                else
                    let
                        free : Int -> Int -> Bool
                        free posX posY =
                            not (Set.member ( posX, posY ) model.walls)

                        newPos : Position
                        newPos =
                            if free newX newY then
                                ( newX, newY )

                            else if free newX positionY then
                                ( newX, positionY )

                            else if free positionX newY then
                                ( positionX, newY )

                            else
                                model.heroPosition
                    in
                    if model.heroPosition == newPos then
                        ( hero, model.heroPosition )

                    else
                        let
                            newPositionX : Int
                            newPositionX =
                                Tuple.first newPos
                        in
                        ( { hero
                            | facingRight = newPositionX - positionX > 0
                            , waitTime = 1000 / actionsPerSecond
                            , moving = True
                          }
                        , newPos
                        )
        in
        { model
            | hero = newHero
            , heroPosition = newPosition
        }


isPressed : Digital -> FrameStuff -> Model -> Bool
isPressed key frameStuff model =
    EverySet.member key model.keyboardPressed
        || List.any (\gamepad -> Gamepad.isPressed gamepad key) frameStuff.gamepads



-- wasReleased : Digital -> FrameStuff -> Model -> Bool
-- wasReleased key frameStuff model =
--     (EverySet.member key model.previous.keyboardPressed
--         && not (EverySet.member key model.keyboardPressed)
--     )
--         || List.any (\gamepad -> Gamepad.wasReleased gamepad key) frameStuff.gamepads


actionsPerSecond : number
actionsPerSecond =
    4


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
            { waitTime = 0
            , facingRight = True
            , moving = False
            , attacking = False
            }

        maxGameCells : number
        maxGameCells =
            100

        gameWidth : Int
        gameWidth =
            floor <| sqrt (flags.width / flags.height * maxGameCells)

        gameHeight : Int
        gameHeight =
            maxGameCells // gameWidth

        ( walls, rooms ) =
            generateWalls flags.now gameWidth gameHeight
    in
    { hero = hero
    , heroPosition = ( 1, 1 )
    , keyboardPressed = EverySet.empty
    , previous =
        { heroPosition = ( 1, 1 )
        , keyboardPressed = EverySet.empty
        }
    , now = flags.now
    , width = flags.width
    , height = flags.height
    , gameWidth = gameWidth
    , gameHeight = gameHeight
    , walls = walls
    , rolls = createRolls flags.now rooms
    , level = 1
    }


createRolls : Time.Posix -> List Room -> Dict Position Roll
createRolls now rooms =
    let
        randomRoll : Room -> Generator ( Position, Roll )
        randomRoll room =
            let
                ( minX, minY ) =
                    room.topLeft

                ( maxX, maxY ) =
                    room.bottomRight
            in
            Random.map2
                (\x y -> ( ( x, y ), { reversed = False } ))
                (Random.int (minX + 1) (maxX - 1))
                (Random.int (minY + 1) (maxY - 1))

        generator : Generator (Dict Position Roll)
        generator =
            rooms
                |> List.filter (\room -> room.topLeft /= ( 0, 0 ))
                |> List.map randomRoll
                |> Random.Extra.sequence
                |> Random.map
                    Dict.fromList
    in
    Random.step generator
        (Random.initialSeed <| Time.posixToMillis now)
        |> Tuple.first


generateWalls : Time.Posix -> Int -> Int -> ( Set Position, List Room )
generateWalls now gameWidth gameHeight =
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

        ( internalWalls, rooms ) =
            Random.step
                (wallGenerator topLeft bottomRight
                    |> Random.andThen (openDoors bottomRight)
                )
                (Random.initialSeed <| Time.posixToMillis now)
                |> Tuple.first
    in
    ( Set.union outerWalls internalWalls, rooms )


openDoors : Position -> ( Set Position, List Room ) -> Generator ( Set Position, List Room )
openDoors bottomRight ( walls, rooms ) =
    let
        go : ( Set Position, List Room ) -> Generator (Set Position)
        go ( currentWalls, queue ) =
            case queue of
                [] ->
                    Random.constant currentWalls

                room :: tail ->
                    openDoor bottomRight room currentWalls
                        |> Random.andThen
                            (\newWalls ->
                                go ( newWalls, tail )
                            )
    in
    go ( walls, rooms )
        |> Random.map (\newWalls -> ( newWalls, rooms ))


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
                                Random.constant
                                    ( Set.empty
                                    , [ { topLeft = topLeft
                                        , bottomRight = bottomRight
                                        }
                                      ]
                                    )

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


controls : List ( String, Digital )
controls =
    [ ( "Up", DpadUp )
    , ( "Down", DpadDown )
    , ( "Left", DpadLeft )
    , ( "Right", DpadRight )
    , ( "Flip", A )
    , ( "Reset", Back )
    ]
