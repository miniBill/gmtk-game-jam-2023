module Game.Types exposing (Flags, Hero, InnerModel(..), Model, Msg(..), PlayingModel, Position, Roll, actionsPerSecond)

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
    , width : Float
    , height : Float
    , sources : Dict String Source
    , inner : InnerModel
    }


type InnerModel
    = Menu
    | Playing PlayingModel
    | Lost
        { level : Int
        , sources : Dict String Source
        }


type Msg
    = Tick FrameStuff
    | Resize Int Int
    | KeyDown Digital
    | KeyUp Digital
    | Start
    | Loaded String (Result Audio.LoadError Source)


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
    , level : Int
    , panicLevel : Float
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
