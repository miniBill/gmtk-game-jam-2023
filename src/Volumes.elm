module Volumes exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Float


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 0.05

        Decrement ->
            model - 0.05


type InnerModel
    = Menu
    | Playing
    | Lost
    | Pause
    | Won



-- VIEW


currentGameState : InnerModel
currentGameState =
    Playing


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , button [ onClick Increment ] [ text "+" ]
        , div [] [ text "PanicLevel: ", text (String.fromFloat model) ]
        , div [] [ text "BaseVolume: ", text (String.fromFloat (baseVolume currentGameState model)) ]
        , div [] [ text "SneakyVolume: ", text (String.fromFloat (sneakyVolume currentGameState model)) ]
        , div [] [ text "ChaseVolume: ", text (String.fromFloat (chaseVolume currentGameState model)) ]
        , div [] [ text "PanicVolume: ", text (String.fromFloat (panicVolume currentGameState model)) ]
        , div [] [ text "MenuVolume: ", text (String.fromFloat (menuVolume currentGameState model)) ]
        ]


baseVolume : InnerModel -> Float -> Float
baseVolume gameState panicLevel =
    case gameState of
        Menu ->
            1

        Playing ->
            1

        Lost ->
            0

        Pause ->
            0

        Won ->
            0.25


sneakyVolume : InnerModel -> Float -> Float
sneakyVolume gameState panicLevel =
    case gameState of
        Menu ->
            0

        Playing ->
            clamp 0 1 (panicLevel * -3 + 1)

        Lost ->
            0

        Pause ->
            1

        Won ->
            0.25


chaseVolume : InnerModel -> Float -> Float
chaseVolume gameState panicLevel =
    case gameState of
        Playing ->
            clamp 0 1 (panicLevel * 3 - 0.5)

        _ ->
            0


panicVolume : InnerModel -> Float -> Float
panicVolume gameState panicLevel =
    case gameState of
        Playing ->
            clamp 0 1 (panicLevel * 2.5 - 1.25)

        _ ->
            0


menuVolume : InnerModel -> Float -> Float
menuVolume gameState panicLevel =
    case gameState of
        Menu ->
            1

        _ ->
            0
