module Game.Types exposing (Flags, Hero, Model, Position)

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
    , keyboardPressed : EverySet Digital
    , now : Time.Posix
    , width : Float
    , height : Float
    , gameWidth : Int
    , gameHeight : Int
    , walls : Set Position
    }


type alias Hero =
    { position : Position
    , waitTime : Float
    , facingRight : Bool
    , moving : Bool
    , attacking : Bool
    }


type alias Position =
    ( Int, Int )
