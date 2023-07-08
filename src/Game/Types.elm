module Game.Types exposing (Flags, Hero, Model, Position, Roll)

import Dict exposing (Dict)
import EverySet exposing (EverySet)
import Gamepad exposing (Digital)
import Set exposing (Set)
import Time


type alias Flags =
    { now : Time.Posix
    , width : Float
    , height : Float
    }


type alias Model =
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
