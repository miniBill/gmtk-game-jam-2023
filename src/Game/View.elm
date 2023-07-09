module Game.View exposing (center, view)

import Color
import Css
import Dict
import Fonts
import Game.Types exposing (Behavior(..), Direction(..), Guard, InnerModel(..), Model, Msg(..), PlayingModel, Position, Roll, actionsPerSecond, move)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Styled as Styled
import Html.Styled.Events as StyledEvents
import PixelEngine
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Options as Options
import PixelEngine.Tile as Tile exposing (Tileset)
import Set
import Sprites
import String.Extra


tileSize : number
tileSize =
    16


button :
    List (Styled.Attribute Msg)
    ->
        { onPress : Msg
        , label : String
        }
    -> Styled.Html Msg
button attrs config =
    Styled.styled Styled.div
        [ Css.border3 (Css.px 1) Css.solid (Css.rgb 255 255 255)
        , Css.padding (Css.px 10)
        , Css.cursor Css.pointer
        , Css.margin Css.auto
        , Css.hover [ Css.backgroundColor <| Css.rgb 128 128 128 ]
        , Css.backgroundColor <| Css.rgb 0 0 0
        , Css.width <| Css.pct 100
        , Css.textAlign Css.center
        ]
        (StyledEvents.onMouseEnter MenuHover :: StyledEvents.onClick config.onPress :: attrs)
        [ Styled.text config.label ]


muteButtons : Model -> Styled.Html Msg
muteButtons model =
    Styled.styled Styled.div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.color <| Css.rgb 255 255 255
        , Css.property "gap" "10px"
        ]
        []
        [ button []
            { onPress = MuteAll
            , label =
                if model.mainVolume > 0 then
                    "MUTE ALL"

                else
                    "UNMUTE ALL"
            }
        , button []
            { onPress = MuteMusic
            , label =
                if model.musicVolume > 0 then
                    "MUTE MUSIC"

                else
                    "UNMUTE MUSIC"
            }
        , button []
            { onPress = MuteEffects
            , label =
                if model.effectsVolume > 0 then
                    "MUTE EFFECTS"

                else
                    "UNMUTE EFFECTS"
            }
        ]


view : Model -> Html Msg
view model =
    case model.inner of
        Menu ->
            Styled.toUnstyled <|
                Styled.styled Styled.div
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.color <| Css.rgb 255 255 255
                    , Css.property "gap" "10px"
                    ]
                    []
                    [ button []
                        { onPress = Start
                        , label = "START PLAYING"
                        }
                    , muteButtons model
                    , Styled.styled Styled.div
                        [ Css.textAlign Css.center
                        , Css.backgroundColor <| Css.rgb 0 0 0
                        ]
                        []
                        [ Styled.text "Use arrow keys or WASD to move,"
                        ]
                    , Styled.styled Styled.div
                        [ Css.textAlign Css.center
                        , Css.backgroundColor <| Css.rgb 0 0 0
                        ]
                        []
                        [ Styled.text "press Space to pause."
                        ]
                    ]

        Playing innerModel ->
            Html.div []
                [ PixelEngine.toHtml
                    { width = toFloat innerModel.gameWidth * tileSize
                    , options =
                        Options.default
                            |> Options.withScale (maxScale model innerModel)
                            |> Options.withAnimationFPS 10
                            |> Options.withMovementSpeed (1.2 / actionsPerSecond)
                            |> Just
                    }
                    [ viewTopBar innerModel
                    , PixelEngine.imageArea
                        { height = toFloat innerModel.gameHeight * tileSize
                        , background =
                            PixelEngine.imageBackground
                                { source = Sprites.floor
                                , width = 9.24
                                , height = 16
                                }
                        }
                        (viewRolls innerModel
                            ++ viewGuards innerModel
                            ++ viewWalls innerModel
                            ++ [ viewHero innerModel ]
                        )
                    , viewStatusMessage innerModel
                    ]
                , if innerModel.paused then
                    center [ Html.Attributes.style "gap" "10vmin" ]
                        [ Html.span
                            [ Html.Attributes.style "font-size" "10vmin"
                            , Html.Attributes.style "transform" "rotate(-10deg)"
                            , Html.Attributes.style "background" "pink"
                            , Html.Attributes.style "padding" "0 2vmin"
                            , Html.Attributes.style "color" "black"
                            ]
                            [ Html.text "PAUSED" ]
                        , Styled.toUnstyled <| muteButtons model
                        , Html.span
                            [ Html.Attributes.style "background" "black"
                            , Html.Attributes.style "padding" "10px"
                            ]
                            [ Html.text "Press Space to resume" ]
                        ]

                  else
                    Html.text ""
                ]

        Lost { level } ->
            Html.div
                [ Html.Attributes.style "font-size" "3vmin"
                , Html.Attributes.style "width" "50vmin"
                , Html.Attributes.style "transform" "rotate(-10deg)"
                , Html.Attributes.style "background" "pink"
                , Html.Attributes.style "padding" "0 2vmin"
                , Html.Attributes.style "color" "black"
                , Html.Events.onClick ToMenu
                ]
                [ Html.text <|
                    "Good job, you reached level "
                        ++ String.fromInt level
                        ++ " before dying a horrible, horrible death. Press 'r' or click to restart"
                ]


type alias Sprite msg =
    ( ( Float, Float ), Image msg )


viewGuards : PlayingModel -> List (Sprite msg)
viewGuards model =
    List.concatMap viewGuard model.guards


viewGuard : Guard -> List (Sprite msg)
viewGuard guard =
    [ ( toFloatPosition guard.position
      , Image.fromSrc Sprites.guard
      )
    , let
        lightPosition : ( Float, Float )
        lightPosition =
            move guard.direction guard.position
                |> toFloatPosition
      in
      ( lightPosition
      , case ( guard.behavior, guard.direction ) of
            ( RoamingRoom _, Right ) ->
                Image.fromSrc Sprites.lightRight

            ( RoamingRoom _, Up ) ->
                Image.fromSrc Sprites.lightUp

            ( RoamingRoom _, Left ) ->
                Image.fromSrc Sprites.lightLeft

            ( RoamingRoom _, Down ) ->
                Image.fromSrc Sprites.lightDown

            ( SillyChasingHero, Right ) ->
                Image.fromSrc Sprites.redLightRight

            ( SillyChasingHero, Up ) ->
                Image.fromSrc Sprites.redLightUp

            ( SillyChasingHero, Left ) ->
                Image.fromSrc Sprites.redLightLeft

            ( SillyChasingHero, Down ) ->
                Image.fromSrc Sprites.redLightDown
      )
    ]


viewRolls : PlayingModel -> List (Sprite msg)
viewRolls model =
    model.rolls
        |> Dict.toList
        |> List.map viewRoll


viewRoll : ( Position, Roll ) -> Sprite msg
viewRoll ( position, roll ) =
    ( toFloatPosition position
    , Image.fromSrc <|
        if roll.reversed then
            Sprites.rollUnder

        else
            Sprites.rollOver
    )


viewWalls : PlayingModel -> List (Sprite msg)
viewWalls model =
    model.walls
        |> Set.toList
        |> List.map viewWall


viewWall : Position -> Sprite msg
viewWall position =
    ( toFloatPosition position
    , Image.fromSrc Sprites.wall
    )


viewTopBar : PlayingModel -> PixelEngine.Area msg
viewTopBar playingModel =
    topBar playingModel
        |> Tile.fromText ( 0, 0 )
        |> List.indexedMap
            (\column tile ->
                ( ( column
                  , 0
                  )
                , tile
                )
            )
        |> PixelEngine.tiledArea
            { rows = 2
            , tileset = textTileset
            , background = PixelEngine.colorBackground <| Color.rgba 0 0 0 0
            }


topBar : PlayingModel -> String
topBar model =
    if model.level < 3 then
        "Level "
            ++ String.fromInt model.level

    else
        "Level "
            ++ String.fromInt model.level
            ++ " - panic "
            ++ String.padLeft 3 ' ' (String.fromInt <| round <| model.panicLevel * 100)
            ++ "%"


viewStatusMessage : PlayingModel -> PixelEngine.Area msg
viewStatusMessage model =
    let
        charsPerLine : Int
        charsPerLine =
            model.gameWidth * tileSize // textTileset.spriteHeight
    in
    statusMessage model
        |> String.Extra.softBreak charsPerLine
        |> List.take statusMessageHeight
        |> List.map (Tile.fromText ( 0, 0 ))
        |> List.indexedMap
            (\row tiles ->
                List.indexedMap
                    (\column tile ->
                        ( ( column
                          , 1 + row
                          )
                        , tile
                        )
                    )
                    tiles
            )
        |> List.concat
        |> PixelEngine.tiledArea
            { rows = statusMessageHeight + 1
            , tileset = textTileset
            , background = PixelEngine.colorBackground <| Color.rgba 0 0 0 0
            }


statusMessage : PlayingModel -> String
statusMessage model =
    case model.level of
        1 ->
            "To win reverse the rolls by touching them"

        3 ->
            "Avoid the guards!!!"

        l ->
            facts
                |> List.drop (modBy (List.length facts) l)
                |> List.head
                |> Maybe.withDefault ""


facts : List String
facts =
    [ "The average roll has 333 sheets, that's plenty!"
    , "One person uses ~384 trees of toilet paper in a lifetime."
    , "Did you know? Nokia used to produce TP rolls!"
    , "The first use of toilet paper was in China, 6th century."
    , "Bidets are mandatory in all houses in Italy since 1975."
    , "Take regular toilet breaks! Your body needs them!"
    ]


maxScale : Model -> PlayingModel -> Int
maxScale model playingModel =
    let
        borderWidth : Float
        borderWidth =
            0.1 * min model.width model.height

        maxScaleWidth : Float
        maxScaleWidth =
            (model.width - 2 * borderWidth)
                / (toFloat playingModel.gameWidth * tileSize)

        maxScaleHeight : Float
        maxScaleHeight =
            (model.height - 2 * borderWidth)
                / ((toFloat playingModel.gameHeight * tileSize)
                    + ((statusMessageHeight + 2) * toFloat textTileset.spriteHeight)
                  )

        maxScaleMin : Float
        maxScaleMin =
            min maxScaleWidth maxScaleHeight
    in
    --2 ^ floor (logBase 2 maxScaleMin)
    floor maxScaleMin


textTileset : Tileset
textTileset =
    Fonts.berlin8x8white.tileset


viewHero : PlayingModel -> Sprite msg
viewHero model =
    ( toFloatPosition model.heroPosition
    , case model.hero.direction of
        Right ->
            Image.fromSrc Sprites.mummyThicklines

        _ ->
            Image.fromSrc Sprites.mummyThicklinesFlipped
    )


statusMessageHeight : number
statusMessageHeight =
    2



-- viewAnimated :
--     { position : Position
--     , spritesheet : { widthInTiles : Int, tileset : Tile.Tileset }
--     , key : String
--     }
--     -> Sprite msg
-- viewAnimated { position, spritesheet, key } =
--     ( toFloatPosition position
--     , Image.fromTile
--         (Tile.fromPosition ( 0, 0 )
--             |> Tile.animated spritesheet.widthInTiles
--         )
--         spritesheet.tileset
--       -- |> Image.movable key
--     )


toFloatPosition : Position -> ( Float, Float )
toFloatPosition ( x, y ) =
    ( toFloat <| tileSize * x
    , toFloat <| tileSize * y
    )


center : List (Attribute msg) -> List (Html msg) -> Html msg
center attrs =
    Html.div
        (Html.Attributes.style "position" "absolute"
            :: Html.Attributes.style "top" "0"
            :: Html.Attributes.style "left" "0"
            :: Html.Attributes.style "width" "100vw"
            :: Html.Attributes.style "height" "100vh"
            :: Html.Attributes.style "display" "flex"
            :: Html.Attributes.style "flex-direction" "column"
            :: Html.Attributes.style "color" "white"
            :: Html.Attributes.style "align-items" "center"
            :: Html.Attributes.style "justify-content" "center"
            :: attrs
        )
