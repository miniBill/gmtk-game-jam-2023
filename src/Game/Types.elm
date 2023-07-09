module Game.Types exposing (Behavior(..), Direction(..), Effect, Flags, Guard, Hero, InnerModel(..), Model, Msg(..), PlayingModel, Position, Roll, Room, actionsPerSecond, move)

import Audio exposing (Source)
import Dict exposing (Dict)
import EverySet exposing (EverySet)
import Gamepad exposing (Digital)
import Gamepad.Simple exposing (FrameStuff)
import Set exposing (Set)
import Time


type alias Flags =
    { now : Time.Posix
    , width : Float
    , height : Float
    }


type alias Model =
    { now : Time.Posix
    , startTime : Time.Posix
    , effects : List Effect
    , width : Float
    , height : Float
    , sources : Dict String Source
    , inner : InnerModel
    , mainVolume : Float
    , musicVolume : Float
    , effectsVolume : Float
    }


type alias Effect =
    ( String, Time.Posix )


type InnerModel
    = Menu {}
    | Playing PlayingModel
    | Lost { level : Int }


type Msg
    = Tick FrameStuff
    | Resize Int Int
    | KeyDown Digital
    | KeyUp Digital
    | Start
    | ToMenu
    | Loaded String (Result Audio.LoadError Source)
    | MenuHover
    | CleanQueue
    | MuteAll
    | MuteMusic
    | MuteEffects


type alias PlayingModel =
    { hero : Hero
    , heroPosition : Position
    , keyboardPressed : EverySet Digital
    , previous :
        { heroPosition : Position
        , keyboardPressed : EverySet Digital
        }
    , gameWidth : Int
    , gameHeight : Int
    , walls : Set Position
    , rolls : Dict Position Roll
    , rooms : List Room
    , level : Int
    , panicLevel : Float
    , lastPanicIncreaseAt : Time.Posix
    , lastPanicDecreaseAt : Time.Posix
    , lastWonAt : Maybe Time.Posix
    , paused : Bool
    , guards : List Guard
    }


type alias Guard =
    { position : Position
    , waitTime : Float
    , direction : Direction
    , behavior : Behavior
    }


type Direction
    = Up
    | Left
    | Right
    | Down


type Behavior
    = RoamingRoom Room
    | SillyChasingHero


type alias Room =
    { topLeft : Position
    , bottomRight : Position
    }


type alias Roll =
    { reversed : Bool
    }


type alias Hero =
    { waitTime : Float
    , direction : Direction
    , moving : Bool
    , attacking : Bool
    }


type alias Position =
    ( Int, Int )


actionsPerSecond : number
actionsPerSecond =
    4


move : Direction -> Position -> Position
move direction position =
    case direction of
        Left ->
            moveLeft position

        Up ->
            moveUp position

        Right ->
            moveRight position

        Down ->
            moveDown position


moveRight : Position -> Position
moveRight ( x, y ) =
    ( x + 1, y )


moveLeft : Position -> Position
moveLeft ( x, y ) =
    ( x - 1, y )


moveDown : Position -> Position
moveDown ( x, y ) =
    ( x, y + 1 )


moveUp : Position -> Position
moveUp ( x, y ) =
    ( x, y - 1 )
