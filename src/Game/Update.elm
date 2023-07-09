module Game.Update exposing (audio, controls, init, loadAudio, onAnimationFrame, subscriptions, update)

import Audio exposing (Audio, AudioCmd)
import AudioSources
import AudioSources.Effects
import AudioSources.Music
import Browser.Events
import Dict exposing (Dict)
import Duration
import EverySet
import Game.Types exposing (Behavior(..), Direction(..), Effect, Flags, Guard, Hero, InnerModel(..), Model, Msg(..), PlayingModel, Position, Roll, Room, actionsPerSecond, move)
import Gamepad exposing (Digital)
import Gamepad.Simple exposing (FrameStuff)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Quantity
import Random exposing (Generator)
import Random.Extra
import Random.List
import Set exposing (Set)
import Time


maxArea : Int
maxArea =
    16


minSideLength : Int
minSideLength =
    2


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.inner ) of
        ( MuteAll, _ ) ->
            { model | mainVolume = 1 - model.mainVolume }

        ( MuteMusic, _ ) ->
            { model | musicVolume = 1 - model.musicVolume }

        ( MuteEffects, _ ) ->
            { model | effectsVolume = 1 - model.effectsVolume }

        ( CleanQueue, _ ) ->
            { model
                | effects =
                    List.filter
                        (\( _, at ) -> deltaT model at < 5000)
                        model.effects
            }

        ( MenuHover, _ ) ->
            model
                |> queueEffect AudioSources.Effects.menuHover

        ( Start, Menu _ ) ->
            { model
                | inner = Playing <| initPlaying model
            }
                |> queueEffect AudioSources.Effects.menuClick

        ( Start, _ ) ->
            model

        ( ToMenu, Lost _ ) ->
            { model | inner = Menu {} }

        ( ToMenu, _ ) ->
            model

        ( Tick frameStuff, Menu _ ) ->
            { model | now = frameStuff.timestamp }

        ( Tick frameStuff, Playing innerModel ) ->
            let
                -- Avoid jumps if focus is lost
                fixed : FrameStuff
                fixed =
                    { frameStuff | dt = min 200 frameStuff.dt }

                ( newInnerModel, newEffects ) =
                    ( innerModel, [] )
                        |> updatePipe (updateHeroPosition fixed)
                        |> updatePipe (updateGuardsPosition fixed)
                        |> updatePipe (updateReversing fixed model)
                        |> updatePipe (increasePanic model)
                        |> updatePipe (decreasePanic model)
                        |> updatePipe (maybeReset fixed model)
                        |> updatePipe moveToPrevious

                newModel : Model
                newModel =
                    { model
                        | now = fixed.timestamp
                        , inner = Playing newInnerModel
                        , effects = newEffects ++ model.effects
                    }
            in
            checkDeath newModel newInnerModel

        ( Tick frameStuff, Lost _ ) ->
            { model | now = frameStuff.timestamp }

        ( Resize w h, _ ) ->
            { model
                | width = toFloat w
                , height = toFloat h
            }

        ( KeyDown key, Playing innerModel ) ->
            { model
                | inner =
                    { innerModel | keyboardPressed = EverySet.insert key innerModel.keyboardPressed }
                        |> Playing
            }

        ( KeyUp key, Playing innerModel ) ->
            { model
                | inner =
                    { innerModel | keyboardPressed = EverySet.remove key innerModel.keyboardPressed }
                        |> Playing
            }

        ( KeyDown _, _ ) ->
            model

        ( KeyUp _, _ ) ->
            model

        ( Loaded _ (Err e), _ ) ->
            let
                _ =
                    Debug.log "Error loading music" e
            in
            model

        ( Loaded key (Ok source), _ ) ->
            { model | sources = Dict.insert key source model.sources }


checkDeath : Model -> PlayingModel -> Model
checkDeath model playingModel =
    let
        isDeaded : Bool
        isDeaded =
            List.any
                (\guard -> guard.position == playingModel.heroPosition)
                playingModel.guards
    in
    if isDeaded then
        { model
            | inner = Lost { level = playingModel.level }
            , effects =
                ( AudioSources.Effects.lose, model.now )
                    :: model.effects
        }

    else
        model


{-| Time elapsed since the given instant.
-}
deltaT : Model -> Time.Posix -> Float
deltaT model old =
    toFloat <| Time.posixToMillis model.now - Time.posixToMillis old


increasePanic : Model -> PlayingModel -> ( PlayingModel, List Effect )
increasePanic model playingModel =
    if deltaT model playingModel.lastPanicIncreaseAt < 1000 then
        ( playingModel, [] )

    else
        let
            guardHasSpotted : { a | direction : Direction, position : Position } -> Bool
            guardHasSpotted guard =
                move guard.direction guard.position == playingModel.heroPosition

            isSpotted : Bool
            isSpotted =
                List.any guardHasSpotted playingModel.guards
        in
        if isSpotted then
            ( { playingModel
                | panicLevel = clamp 0 1 <| 0.5 + playingModel.panicLevel
                , lastPanicIncreaseAt = model.now
                , lastPanicDecreaseAt = model.now -- prevent immediate decrease
              }
            , [ ( AudioSources.Effects.spotted, model.now ) ]
            )

        else
            ( playingModel, [] )


decreasePanic : Model -> PlayingModel -> ( PlayingModel, List Effect )
decreasePanic model playingModel =
    if deltaT model playingModel.lastPanicDecreaseAt < 300 * toFloat playingModel.level then
        ( playingModel, [] )

    else
        ( { playingModel
            | panicLevel = clamp 0 1 <| playingModel.panicLevel - 0.1
            , lastPanicDecreaseAt = model.now
          }
        , []
        )


updateGuardsPosition : FrameStuff -> PlayingModel -> ( PlayingModel, List Effect )
updateGuardsPosition frameStuff model =
    if model.paused then
        ( model, [] )

    else
        let
            newModel : PlayingModel
            newModel =
                { model | guards = List.map (updateGuard frameStuff model) model.guards }
        in
        ( newModel, [] )


updateGuard : FrameStuff -> PlayingModel -> Guard -> Guard
updateGuard frameStuff model guard =
    if guard.waitTime > 0 then
        if move guard.direction guard.position == model.heroPosition then
            { guard
                | waitTime = max 0 <| guard.waitTime - frameStuff.dt
                , behavior = SillyChasingHero
            }

        else
            { guard | waitTime = max 0 <| guard.waitTime - frameStuff.dt }

    else
        let
            waitTime : Float
            waitTime =
                -- Guards are always slower than the hero, but less and less
                (1000 + 3000 / toFloat model.level) / actionsPerSecond
        in
        case guard.behavior of
            RoamingRoom room ->
                let
                    ( minX, minY ) =
                        room.topLeft

                    ( maxX, maxY ) =
                        room.bottomRight

                    ( guardX, guardY ) =
                        guard.position
                in
                if guardX == maxX then
                    if guard.direction == Left then
                        { guard | position = move Left guard.position }

                    else
                        { guard | direction = Left }

                else if guardY == maxY then
                    if guard.direction == Up then
                        { guard | position = move Up guard.position }

                    else
                        { guard | direction = Up }

                else
                    let
                        ( wantedX, wantedY ) =
                            move guard.direction guard.position

                        newDirection : Direction
                        newDirection =
                            if wantedX == minX || wantedX == maxX then
                                if wantedY == minY + 1 then
                                    Down

                                else
                                    Up

                            else if wantedY == minY || wantedY == maxY then
                                if wantedX == minX + 1 then
                                    Right

                                else
                                    Left

                            else
                                guard.direction

                        movedGuard : Guard
                        movedGuard =
                            if newDirection == guard.direction then
                                { guard
                                    | position = move guard.direction guard.position
                                    , waitTime = waitTime
                                }

                            else
                                { guard
                                    | direction = newDirection
                                    , waitTime = waitTime / 4
                                }
                    in
                    if Set.member movedGuard.position model.walls then
                        guard

                    else
                        let
                            lightPosition : Position
                            lightPosition =
                                move movedGuard.direction movedGuard.position
                        in
                        if lightPosition == model.heroPosition then
                            { movedGuard | behavior = SillyChasingHero }

                        else
                            movedGuard

            SillyChasingHero ->
                if model.panicLevel == 0 then
                    case findRoom guard.position model.rooms of
                        Nothing ->
                            guard

                        Just room ->
                            { guard | behavior = RoamingRoom room }

                else
                    let
                        ( heroX, heroY ) =
                            model.heroPosition

                        ( guardX, guardY ) =
                            guard.position

                        isFree : Direction -> Bool
                        isFree direction =
                            not (Set.member (move direction guard.position) model.walls)

                        newDirection : Direction
                        newDirection =
                            if heroX > guardX && isFree Right then
                                Right

                            else if heroX < guardX && isFree Left then
                                Left

                            else if heroY > guardY && isFree Down then
                                Down

                            else if heroY < guardY && isFree Up then
                                Up

                            else
                                guard.direction

                        newPosition : Position
                        newPosition =
                            move newDirection guard.position
                    in
                    if Set.member newPosition model.walls then
                        { guard | direction = newDirection }

                    else if newDirection == guard.direction then
                        { guard
                            | position = newPosition
                            , waitTime = waitTime
                        }

                    else
                        { guard
                            | direction = newDirection
                            , waitTime = waitTime / 2
                        }


findRoom : Position -> List Room -> Maybe Room
findRoom position rooms =
    List.Extra.find (contains position) rooms


contains : Position -> Room -> Bool
contains position room =
    let
        ( x, y ) =
            position

        ( minX, minY ) =
            room.topLeft

        ( maxX, maxY ) =
            room.bottomRight
    in
    minX < x && x <= maxX && minY < y && y <= maxY


queueEffect : String -> Model -> Model
queueEffect key model =
    { model | effects = ( key, model.now ) :: model.effects }


updatePipe :
    (PlayingModel -> ( PlayingModel, List Effect ))
    -> ( PlayingModel, List Effect )
    -> ( PlayingModel, List Effect )
updatePipe f ( model, oldEffects ) =
    let
        ( newModel, newEffects ) =
            f model
    in
    ( newModel, newEffects ++ oldEffects )


moveToPrevious : PlayingModel -> ( PlayingModel, List Effect )
moveToPrevious model =
    ( { model
        | previous =
            { keyboardPressed = model.keyboardPressed
            , heroPosition = model.heroPosition
            }
      }
    , []
    )


maybeReset : FrameStuff -> Model -> PlayingModel -> ( PlayingModel, List Effect )
maybeReset frameStuff model playingModel =
    if wasClicked Gamepad.Start frameStuff playingModel then
        ( { playingModel | paused = not playingModel.paused }, [] )

    else
        let
            _ =
                Debug.todo
        in
        if wasReleased Gamepad.Back frameStuff playingModel then
            ( toLevel 1 model playingModel
            , [ ( AudioSources.Effects.lose, model.now ) ]
            )

        else if wasReleased Gamepad.A frameStuff playingModel then
            ( toLevel (playingModel.level + 1) model playingModel
            , [ ( AudioSources.Effects.victory, model.now ) ]
            )

        else if wasReleased Gamepad.Y frameStuff playingModel then
            ( { playingModel | panicLevel = clamp 0 1 <| playingModel.panicLevel + 0.05 }, [] )

        else if wasReleased Gamepad.X frameStuff playingModel then
            ( { playingModel | panicLevel = clamp 0 1 <| playingModel.panicLevel - 0.05 }, [] )

        else
            ( playingModel, [] )


regen : Time.Posix -> PlayingModel -> PlayingModel
regen now playingModel =
    let
        ( walls, rooms ) =
            generateWalls now playingModel.gameWidth playingModel.gameHeight
    in
    { playingModel
        | walls = walls
        , rolls = createRolls now rooms
        , rooms = rooms
        , heroPosition = ( 1, 1 )
        , guards = createGuards now rooms playingModel.level
    }


createGuards : Time.Posix -> List Room -> Int -> List Guard
createGuards now rooms level =
    let
        guardsCount : Int
        guardsCount =
            if level < 3 then
                0

            else
                clamp 3 (List.length rooms) level
    in
    rooms
        |> List.filter (\room -> room.topLeft /= ( 0, 0 ))
        |> Random.List.choices guardsCount
        |> Random.map (\( picked, _ ) -> List.map createGuardInRoom picked)
        |> randomGenerate now


createGuardInRoom : Room -> Guard
createGuardInRoom room =
    let
        ( x, y ) =
            room.topLeft
    in
    { position = ( x + 1, y + 1 )
    , direction =
        if modBy 2 (x + y) == 0 then
            Down

        else
            Right
    , behavior = RoamingRoom room
    , waitTime = 0
    }


updateReversing : FrameStuff -> Model -> PlayingModel -> ( PlayingModel, List Effect )
updateReversing _ model playingModel =
    let
        tryFlip : Position -> ( Dict Position Roll, Bool ) -> ( Dict Position Roll, Bool )
        tryFlip position ( rolls, alreadyFlipped ) =
            case
                Dict.get position rolls
            of
                Just roll ->
                    ( Dict.insert
                        position
                        { roll | reversed = True }
                        rolls
                    , alreadyFlipped || not roll.reversed
                    )

                _ ->
                    ( rolls, alreadyFlipped )

        diagonals : Maybe ( Position, Position )
        diagonals =
            let
                ( currX, currY ) =
                    playingModel.heroPosition

                ( prevX, prevY ) =
                    playingModel.previous.heroPosition

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
                    && (Set.member diag1 playingModel.walls || Set.member diag2 playingModel.walls)
            then
                Just ( diag1, diag2 )

            else
                Nothing

        ( afterFlip, flipped ) =
            case diagonals of
                Nothing ->
                    ( playingModel.rolls, False )
                        |> tryFlip playingModel.heroPosition

                Just ( diag1, diag2 ) ->
                    ( playingModel.rolls, False )
                        |> tryFlip playingModel.heroPosition
                        |> tryFlip diag1
                        |> tryFlip diag2

        newPlayingModel : PlayingModel
        newPlayingModel =
            { playingModel | rolls = afterFlip }
    in
    if
        List.all
            (\( _, { reversed } ) -> reversed)
            (Dict.toList newPlayingModel.rolls)
    then
        ( toLevel (newPlayingModel.level + 1) model newPlayingModel
        , [ ( AudioSources.Effects.victory, model.now ) ]
        )

    else
        ( newPlayingModel
        , if flipped then
            [ ( AudioSources.Effects.flipped, model.now ) ]

          else
            []
        )


toLevel : Int -> Model -> PlayingModel -> PlayingModel
toLevel level model playingModel =
    let
        diff : Int
        diff =
            level - playingModel.level

        ready : PlayingModel
        ready =
            { playingModel
                | level = level
                , panicLevel = 0
                , lastWonAt = Just model.now
            }

        ( gameWidth, gameHeight ) =
            if playingModel.gameWidth > playingModel.gameHeight then
                ( playingModel.gameWidth + diff
                , (toFloat (playingModel.gameWidth + diff)
                    * model.height
                    / model.width
                  )
                    |> floor
                )

            else
                ( (toFloat (playingModel.gameHeight + diff)
                    * model.width
                    / model.height
                  )
                    |> floor
                , playingModel.gameHeight + diff
                )
    in
    { ready
        | gameWidth = gameWidth
        , gameHeight = gameHeight
    }
        |> regen model.now


updateHeroPosition : FrameStuff -> PlayingModel -> ( PlayingModel, List Effect )
updateHeroPosition frameStuff model =
    if model.paused then
        ( model, [] )

    else
        let
            hero : Hero
            hero =
                model.hero
        in
        if hero.waitTime > 0 then
            ( { model
                | hero =
                    { hero | waitTime = max 0 <| hero.waitTime - frameStuff.dt }
              }
            , []
            )

        else
            let
                onPress : Digital -> number
                onPress key =
                    if isPressed key frameStuff model then
                        1

                    else
                        0

                ( positionX, positionY ) =
                    model.heroPosition

                wantedDx : number
                wantedDx =
                    onPress Gamepad.DpadRight - onPress Gamepad.DpadLeft

                wantedNewX : Int
                wantedNewX =
                    (positionX + wantedDx)
                        |> clamp 0 (model.gameWidth - 1)

                wantedDy : number
                wantedDy =
                    onPress Gamepad.DpadDown - onPress Gamepad.DpadUp

                wantedNewY : Int
                wantedNewY =
                    (positionY + wantedDy)
                        |> clamp 0 (model.gameHeight - 1)

                ( newHero, newPosition ) =
                    if wantedNewX == positionX && wantedNewY == positionY then
                        ( { hero | moving = False }, model.heroPosition )

                    else
                        let
                            free : Int -> Int -> Bool
                            free posX posY =
                                not (Set.member ( posX, posY ) model.walls)

                            newPos : Position
                            newPos =
                                if free wantedNewX wantedNewY then
                                    ( wantedNewX, wantedNewY )

                                else if free wantedNewX positionY then
                                    ( wantedNewX, positionY )

                                else if free positionX wantedNewY then
                                    ( positionX, wantedNewY )

                                else
                                    model.heroPosition
                        in
                        if model.heroPosition == newPos then
                            ( hero, model.heroPosition )

                        else
                            let
                                ( newX, newY ) =
                                    newPos

                                dx : Int
                                dx =
                                    newX - positionX

                                newDirection : Direction
                                newDirection =
                                    if dx > 0 then
                                        Right

                                    else if dx < 0 then
                                        Left

                                    else
                                        let
                                            dy : Int
                                            dy =
                                                newY - positionY
                                        in
                                        if dy > 0 then
                                            Up

                                        else if dy < 0 then
                                            Down

                                        else
                                            hero.direction
                            in
                            ( { hero
                                | direction = newDirection
                                , waitTime = 1000 / actionsPerSecond
                                , moving = True
                              }
                            , newPos
                            )
            in
            ( { model
                | hero = newHero
                , heroPosition = newPosition
              }
            , []
            )


isPressed : Digital -> FrameStuff -> PlayingModel -> Bool
isPressed key frameStuff model =
    EverySet.member key model.keyboardPressed
        || List.any (\gamepad -> Gamepad.isPressed gamepad key) frameStuff.gamepads


wasReleased : Digital -> FrameStuff -> PlayingModel -> Bool
wasReleased key frameStuff model =
    (EverySet.member key model.previous.keyboardPressed
        && not (EverySet.member key model.keyboardPressed)
    )
        || List.any (\gamepad -> Gamepad.wasReleased gamepad key) frameStuff.gamepads


wasClicked : Digital -> FrameStuff -> PlayingModel -> Bool
wasClicked key frameStuff model =
    (not (EverySet.member key model.previous.keyboardPressed)
        && EverySet.member key model.keyboardPressed
    )
        || List.any (\gamepad -> Gamepad.wasClicked gamepad key) frameStuff.gamepads


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        , Time.every 1000 <| \_ -> CleanQueue
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
            Just Gamepad.DpadLeft

        "ArrowRight" ->
            Just Gamepad.DpadRight

        "ArrowUp" ->
            Just Gamepad.DpadUp

        "ArrowDown" ->
            Just Gamepad.DpadDown

        "w" ->
            Just Gamepad.DpadUp

        "a" ->
            Just Gamepad.DpadLeft

        "s" ->
            Just Gamepad.DpadDown

        "d" ->
            Just Gamepad.DpadRight

        "q" ->
            Just Gamepad.X

        "e" ->
            Just Gamepad.Y

        "c" ->
            Just Gamepad.A

        " " ->
            Just Gamepad.Start

        "Backspace" ->
            Just Gamepad.Back

        _ ->
            Nothing


onAnimationFrame : FrameStuff -> Msg
onAnimationFrame frameStuff =
    Tick frameStuff


init : Flags -> ( Model, AudioCmd Msg )
init flags =
    ( { now = flags.now
      , startTime = flags.now
      , effects = []
      , width = flags.width
      , height = flags.height
      , sources = Dict.empty
      , inner = Menu {}
      , mainVolume = 1
      , musicVolume = 1
      , effectsVolume = 1
      }
        |> queueEffect AudioSources.Music.menuIntro
    , AudioSources.all
        |> List.map loadAudio
        |> Audio.cmdBatch
    )


initPlaying : Model -> PlayingModel
initPlaying flags =
    let
        hero : Hero
        hero =
            { waitTime = 0
            , direction = Right
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
    , gameWidth = gameWidth
    , gameHeight = gameHeight
    , walls = walls
    , rooms = rooms
    , rolls = createRolls flags.now rooms
    , level = 1
    , panicLevel = 0
    , lastPanicIncreaseAt = flags.now
    , lastPanicDecreaseAt = flags.now
    , lastWonAt = Nothing
    , paused = False
    , guards = []
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
    in
    rooms
        |> List.filter (\room -> room.topLeft /= ( 0, 0 ))
        |> List.map randomRoll
        |> Random.Extra.sequence
        |> Random.map Dict.fromList
        |> randomGenerate now


randomGenerate : Time.Posix -> Generator a -> a
randomGenerate now generator =
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
            wallGenerator topLeft bottomRight
                |> Random.andThen (openDoors bottomRight)
                |> randomGenerate now
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
        ( minX, minY ) =
            room.topLeft

        ( maxX, maxY ) =
            room.bottomRight

        rightHole : Set Position -> Generator (Set Position)
        rightHole walls =
            if not rightmostRoom then
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
            if not bottommostRoom then
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

        rightmostRoom : Bool
        rightmostRoom =
            maxX == Tuple.first bottomRight

        bottommostRoom : Bool
        bottommostRoom =
            maxY == Tuple.second bottomRight
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


controls : List ( String, Digital )
controls =
    [ ( "Up", Gamepad.DpadUp )
    , ( "Down", Gamepad.DpadDown )
    , ( "Left", Gamepad.DpadLeft )
    , ( "Right", Gamepad.DpadRight )
    , ( "Next level", Gamepad.A )
    , ( "Pause", Gamepad.Start )
    , ( "Reset", Gamepad.Back )
    , ( "Panic up", Gamepad.Y )
    , ( "Panic down", Gamepad.X )
    ]


audio : Model -> Audio
audio model =
    let
        musicAt : Time.Posix
        musicAt =
            (1515 + Time.posixToMillis model.startTime)
                |> Time.millisToPosix

        tracks : List Audio
        tracks =
            [ ( AudioSources.Music.base, baseVolume )
            , ( AudioSources.Music.sneaky, sneakyVolume )
            , ( AudioSources.Music.chase, chaseVolume )
            , ( AudioSources.Music.panic, panicVolume )
            , ( AudioSources.Music.menu, menuVolume )
            ]
                |> List.map
                    (\( key, volume ) ->
                        music model key musicAt
                            |> Audio.scaleVolume (model.musicVolume * volume model model.inner)
                    )

        effects : List Audio
        effects =
            List.map
                (\( key, at ) ->
                    effect model key at
                        |> Audio.scaleVolume model.effectsVolume
                )
                model.effects
    in
    (effects ++ tracks)
        |> Audio.group
        |> Audio.scaleVolume (0.2 * model.mainVolume)


effect : Model -> String -> Time.Posix -> Audio
effect model key at =
    case Dict.get key model.sources of
        Nothing ->
            Audio.silence

        Just source ->
            Audio.audioWithConfig
                { loop = Nothing
                , playbackRate = 1
                , startAt = Quantity.zero
                }
                source
                at


music : Model -> String -> Time.Posix -> Audio
music model key at =
    case Dict.get key model.sources of
        Nothing ->
            Audio.silence

        Just source ->
            Audio.audioWithConfig
                { loop =
                    Just
                        { loopStart = Quantity.zero
                        , loopEnd = Duration.seconds 48.013025
                        }
                , playbackRate = 1
                , startAt = Quantity.zero
                }
                source
                at


loadAudio : String -> AudioCmd Msg
loadAudio key =
    Audio.loadAudio (Loaded key) <| "/audio/" ++ key


fadeForVictory : Model -> Maybe Time.Posix -> Float
fadeForVictory model lastWonAt =
    case lastWonAt of
        Just at ->
            clamp 0.25 1 (0.25 + deltaT model at * 0.75 / 4806.5)

        Nothing ->
            1


baseVolume : Model -> InnerModel -> Float
baseVolume model gameState =
    case gameState of
        Menu _ ->
            1

        Playing { lastWonAt, paused } ->
            if paused then
                0

            else
                fadeForVictory model lastWonAt

        Lost _ ->
            0


sneakyVolume : Model -> InnerModel -> Float
sneakyVolume model gameState =
    case gameState of
        Menu _ ->
            0

        Playing { paused, lastWonAt, panicLevel } ->
            if paused then
                1

            else
                fadeForVictory model lastWonAt
                    * clamp 0 1 (panicLevel * -3 + 1)

        Lost _ ->
            0


chaseVolume : Model -> InnerModel -> Float
chaseVolume _ gameState =
    case gameState of
        Playing { paused, panicLevel } ->
            if paused then
                0

            else
                clamp 0 1 (panicLevel * 3 - 0.5)

        _ ->
            0


panicVolume : Model -> InnerModel -> Float
panicVolume _ gameState =
    case gameState of
        Playing { paused, panicLevel } ->
            if paused then
                0

            else
                clamp 0 1 (panicLevel * 2.5 - 1.25)

        _ ->
            0


menuVolume : Model -> InnerModel -> Float
menuVolume _ gameState =
    case gameState of
        Menu _ ->
            1

        _ ->
            0
