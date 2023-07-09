port module Main exposing (Flags, Model, Msg, main)

import Audio exposing (Audio, AudioCmd)
import AudioSources.Music
import Browser.Dom
import Browser.Events
import Game.Types as Game
import Game.Update
import Game.View
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Encode
import Task
import Time


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Encode.Value -> msg) -> Sub msg


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



-- main : Gamepad.Simple.Program Flags Model Msg


main : Program Flags (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        --  Gamepad.Simple.element
        --     gamepadConfig
        { init = init
        , update = \_ -> update
        , view = \_ -> view
        , subscriptions = \_ -> subscriptions
        , audio = \_ -> audio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }


audio : Model -> Audio
audio model =
    case model of
        WaitingWebAudioInit ->
            Audio.silence

        WebAudioReady inner ->
            Game.Update.audio inner.game



-- gamepadConfig : Gamepad.Simple.Config Msg
-- gamepadConfig =
--     { onAnimationFrame = \stuff -> GameMsg (Game.Update.onAnimationFrame stuff)
--     , onBlob = GamepadPort.onBlob
--     , saveToLocalStorage = GamepadPort.saveToLocalStorage
--     , controls = Game.Update.controls
--     }


init : Flags -> ( Model, Cmd Msg, AudioCmd Msg )
init _ =
    ( WaitingWebAudioInit
    , Cmd.none
    , [ AudioSources.Music.menuIntro
      , AudioSources.Music.base
      ]
        |> List.map Game.Update.loadAudio
        |> Audio.cmdBatch
        |> Audio.cmdMap GameMsg
    )


update : Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update msg model =
    case ( msg, model ) of
        ( GameMsg gameMsg, WebAudioReady innerModel ) ->
            let
                newGame : Game.Model
                newGame =
                    Game.Update.update gameMsg innerModel.game
            in
            ( WebAudioReady
                { innerModel
                    | game = newGame
                }
            , Cmd.none
            , Audio.cmdNone
            )

        ( InitWebAudio, WaitingWebAudioInit ) ->
            ( model
            , Task.map2
                (\now viewport ->
                    { now = now
                    , width = viewport.viewport.width
                    , height = viewport.viewport.height
                    }
                )
                Time.now
                Browser.Dom.getViewport
                |> Task.perform Init
            , Audio.cmdNone
            )

        ( Init flags, WaitingWebAudioInit ) ->
            let
                ( game, cmd ) =
                    Game.Update.init flags

                innerModel : InnerModel
                innerModel =
                    { game = game
                    , focused = True
                    }
            in
            ( WebAudioReady innerModel
            , Cmd.none
            , Audio.cmdMap GameMsg cmd
            )

        ( OnFocus, WebAudioReady innerModel ) ->
            ( WebAudioReady { innerModel | focused = True }, Cmd.none, Audio.cmdNone )

        ( OnBlur, WebAudioReady innerModel ) ->
            ( WebAudioReady { innerModel | focused = False }, Cmd.none, Audio.cmdNone )

        ( InitWebAudio, WebAudioReady _ ) ->
            ( model, Cmd.none, Audio.cmdNone )

        ( Init _, WebAudioReady _ ) ->
            ( model, Cmd.none, Audio.cmdNone )

        ( GameMsg _, WaitingWebAudioInit ) ->
            ( model, Cmd.none, Audio.cmdNone )

        ( OnFocus, WaitingWebAudioInit ) ->
            ( model, Cmd.none, Audio.cmdNone )

        ( OnBlur, WaitingWebAudioInit ) ->
            ( model, Cmd.none, Audio.cmdNone )


view : Model -> Html Msg
view model =
    case model of
        WaitingWebAudioInit ->
            [ Html.div [] [ Html.text "Click anywhere to start" ]
            ]
                |> center

        WebAudioReady innerModel ->
            [ Html.map GameMsg <| Game.View.view innerModel.game
            ]
                |> center


center : List (Html Msg) -> Html Msg
center =
    Html.div
        [ Html.Events.onClick InitWebAudio
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "color" "white"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "center"
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        WebAudioReady { game } ->
            Sub.batch
                [ onfocus <| \_ -> OnFocus
                , onblur <| \_ -> OnBlur
                , Sub.map GameMsg <|
                    Game.Update.subscriptions game
                , Browser.Events.onAnimationFrame
                    (\newTime ->
                        { dt = toFloat <| Time.posixToMillis newTime - Time.posixToMillis game.now
                        , timestamp = newTime
                        , gamepads = []
                        }
                            |> Game.Update.onAnimationFrame
                            |> GameMsg
                    )
                ]

        WaitingWebAudioInit ->
            Sub.none
