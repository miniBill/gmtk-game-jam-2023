module Game.Types exposing (Flags, Hero, Model, Position)

import Gamepad exposing (Digital)
import Time


type alias Flags =
    { now : Time.Posix
    , width : Float
    , height : Float
    }


type alias Model =
    { hero : Hero
    , keyboardPressed : List Digital
    , now : Time.Posix
    , width : Float
    , height : Float
    , gameWidth : Int
    , gameHeight : Int
    }


type alias Hero =
    { position : Position
    , waitTime : Float
    , facingRight : Bool
    , moving : Bool
    }


type alias Position =
    { x : Int
    , y : Int
    }
