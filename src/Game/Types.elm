module Game.Types exposing (Flags, Hero, Model(..), Msg(..), PlayingModel, Position, Roll, actionsPerSecond)

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


type Model
    = Menu
        { now : Time.Posix
        , width : Float
        , height : Float
        }
    | Playing PlayingModel
    | Lost
        { now : Time.Posix
        , width : Float
        , height : Float
        , level : Int
        }


type Msg
    = Tick FrameStuff
    | Resize Int Int
    | KeyDown Digital
    | KeyUp Digital
    | Start


type alias PlayingModel =
    { hero : Hero
    , heroPosition : Position
    , keyboardPressed : EverySet Digital
    , previous :
        { heroPosition : Position
        , keyboardPressed : EverySet Digital
        }
    , now : Time.Posix
    , width : Float
    , height : Float
    , gameWidth : Int
    , gameHeight : Int
    , walls : Set Position
    , rolls : Dict Position Roll
    , level : Int
    }


type alias Roll =
    { reversed : Bool
    }


type alias Hero =
    { waitTime : Float
    , facingRight : Bool
    , moving : Bool
    , attacking : Bool
    }


type alias Position =
    ( Int, Int )


actionsPerSecond : number
actionsPerSecond =
    4
