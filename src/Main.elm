port module Main exposing (Flags, Model, Msg, main)

import Audio
import Browser.Dom
import Effect
import Game
import Gamepad.Simple
import GamepadPort
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Encode
import Task
import Time
import WebAudio
import WebAudio.Property


port toWebAudio : Json.Encode.Value -> Cmd msg


port onfocus : ({} -> msg) -> Sub msg


port onblur : ({} -> msg) -> Sub msg


type Model
    = WaitingWebAudioInit
    | WebAudioReady InnerModel


type alias InnerModel =
    { game : Game.Model
    , focused : Bool
    }


type Msg
    = InitWebAudio
    | Init Game.Flags
    | GameMsg Game.Msg
    | OnFocus
    | OnBlur


type alias Flags =
    {}


main : Gamepad.Simple.Program Flags Model Msg
main =
    Gamepad.Simple.element
        gamepadConfig
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


gamepadConfig : Gamepad.Simple.Config Msg
gamepadConfig =
    { onAnimationFrame = \stuff -> GameMsg (Game.onAnimationFrame stuff)
    , onBlob = GamepadPort.onBlob
    , saveToLocalStorage = GamepadPort.saveToLocalStorage
    , controls = Gamepad.Simple.basicControls
    }


audioCmd : InnerModel -> Cmd Msg
audioCmd model =
    audio model
        |> Json.Encode.list WebAudio.encode
        |> toWebAudio


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( WaitingWebAudioInit
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GameMsg gameMsg, WebAudioReady innerModel ) ->
            let
                ( newGame, gameEffect ) =
                    Game.update gameMsg innerModel.game
            in
            ( WebAudioReady
                { innerModel
                    | game = newGame
                }
            , Cmd.batch
                [ Effect.toCmd effectConfig gameEffect
                , audioCmd innerModel
                ]
            )

        ( InitWebAudio, WaitingWebAudioInit ) ->
            ( model
            , Cmd.batch
                [ Task.map2
                    (\now viewport ->
                        { now = now
                        , width = viewport.viewport.width
                        , height = viewport.viewport.height
                        }
                    )
                    Time.now
                    Browser.Dom.getViewport
                    |> Task.perform Init
                , toWebAudio <| Json.Encode.list Json.Encode.int []
                ]
            )

        ( Init flags, WaitingWebAudioInit ) ->
            let
                ( game, gameEffect ) =
                    Game.init flags

                innerModel : InnerModel
                innerModel =
                    { game = game
                    , focused = True
                    }
            in
            ( WebAudioReady innerModel
            , Cmd.batch
                [ Effect.toCmd effectConfig gameEffect
                , audioCmd innerModel
                ]
            )

        ( OnFocus, WebAudioReady innerModel ) ->
            ( WebAudioReady { innerModel | focused = True }, audioCmd innerModel )

        ( OnBlur, WebAudioReady innerModel ) ->
            ( WebAudioReady { innerModel | focused = False }, audioCmd innerModel )

        ( InitWebAudio, WebAudioReady _ ) ->
            ( model, Cmd.none )

        ( Init _, WebAudioReady _ ) ->
            ( model, Cmd.none )

        ( GameMsg _, WaitingWebAudioInit ) ->
            ( model, Cmd.none )

        ( OnFocus, WaitingWebAudioInit ) ->
            ( model, Cmd.none )

        ( OnBlur, WaitingWebAudioInit ) ->
            ( model, Cmd.none )


effectConfig : Effect.Config Msg
effectConfig =
    { loadedTexture =
        \key texture -> GameMsg <| Game.loadedTexture key texture
    }


view : Model -> Html Msg
view model =
    case model of
        WaitingWebAudioInit ->
            Html.div
                [ Html.Events.onClick InitWebAudio
                , Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "top" "0"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "width" "100vw"
                , Html.Attributes.style "height" "100vh"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "justify-content" "center"
                ]
                [ Html.div [] [ Html.text "Click anywhere to start" ]
                ]

        WebAudioReady innerModel ->
            Html.map GameMsg <| Game.view innerModel.game


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        WebAudioReady innerModel ->
            Sub.batch
                [ onfocus <| \_ -> OnFocus
                , onblur <| \_ -> OnBlur
                , Sub.map GameMsg <| Game.subscriptions innerModel.game
                ]

        WaitingWebAudioInit ->
            Sub.none


audio : InnerModel -> List WebAudio.Node
audio model =
    if model.focused && 1 == sqrt 2 then
        [ WebAudio.oscillator
            [ model.game
                |> Game.time
                |> Time.posixToMillis
                |> (\t -> t // 1000)
                |> modBy 8
                |> Audio.note
                |> WebAudio.Property.frequency
            ]
            |> Audio.gain 0.2
            |> Audio.destination
        ]

    else
        []
