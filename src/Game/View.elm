module Game.View exposing (view)

import Color
import Dict
import Dungeon.Tiles.Wall
import Fonts
import Game.Types exposing (Model(..), Msg(..), PlayingModel, Position, Roll, actionsPerSecond)
import Html exposing (Html)
import Html.Events
import LittleMummy.Idle
import LittleMummy.Walk
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


view : Model -> Html Msg
view model =
    case model of
        Menu _ ->
            Html.div []
                [ Html.button [ Html.Events.onClick Start ]
                    [ Html.text "START PLAYING" ]
                ]

        Playing innerModel ->
            PixelEngine.toHtml
                { width = toFloat innerModel.gameWidth * tileSize
                , options =
                    Options.default
                        |> Options.withScale (maxScale innerModel)
                        |> Options.withAnimationFPS 10
                        |> Options.withMovementSpeed (1.2 / actionsPerSecond)
                        |> Just
                }
                [ viewTopBar innerModel
                , PixelEngine.imageArea
                    { height = toFloat innerModel.gameHeight * tileSize
                    , background = PixelEngine.colorBackground Color.blue
                    }
                    (viewHero innerModel :: viewRolls innerModel ++ viewWalls innerModel)
                , viewStatusMessage innerModel
                ]

        Lost { level } ->
            Html.text <|
                "Good job, you reached level "
                    ++ String.fromInt level
                    ++ " before dying a horrible, horrible death"


viewRolls : PlayingModel -> List ( ( Float, Float ), Image msg )
viewRolls model =
    model.rolls
        |> Dict.toList
        |> List.map viewRoll


viewRoll : ( Position, Roll ) -> ( ( Float, Float ), Image msg )
viewRoll ( position, roll ) =
    ( toFloatPosition position
    , Image.fromSrc <|
        if roll.reversed then
            Sprites.rollOver

        else
            Sprites.rollUnder
    )


viewWalls : PlayingModel -> List ( ( Float, Float ), Image msg )
viewWalls model =
    model.walls
        |> Set.toList
        |> List.map viewWall


viewWall : Position -> ( ( Float, Float ), Image msg )
viewWall position =
    ( toFloatPosition position
    , Image.fromTile
        (Tile.fromPosition ( 0, 0 ))
        Dungeon.Tiles.Wall.wall1
    )


viewTopBar : PlayingModel -> PixelEngine.Area msg
viewTopBar model =
    topBar model
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
            , background = PixelEngine.colorBackground Color.black
            }


topBar : PlayingModel -> String
topBar model =
    "Level " ++ String.fromInt model.level


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
            , background = PixelEngine.colorBackground Color.black
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


maxScale : PlayingModel -> Int
maxScale model =
    let
        borderWidth : Float
        borderWidth =
            0.1 * min model.width model.height

        maxScaleWidth : Float
        maxScaleWidth =
            (model.width - 2 * borderWidth)
                / (toFloat model.gameWidth * tileSize)

        maxScaleHeight : Float
        maxScaleHeight =
            (model.height - 2 * borderWidth)
                / ((toFloat model.gameHeight * tileSize)
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


viewHero : PlayingModel -> ( ( Float, Float ), Image msg )
viewHero model =
    let
        spritesheet : { tileset : Tileset, widthInTiles : Int }
        spritesheet =
            if model.hero.moving then
                if model.hero.facingRight then
                    LittleMummy.Walk.spritesheetFlipped

                else
                    LittleMummy.Walk.spritesheet

            else if model.hero.facingRight then
                LittleMummy.Idle.spritesheetFlipped

            else
                LittleMummy.Idle.spritesheet
    in
    viewAnimated
        { spritesheet = spritesheet
        , position = model.heroPosition
        , key = "hero"
        }


statusMessageHeight : number
statusMessageHeight =
    2


viewAnimated :
    { position : Position
    , spritesheet : { widthInTiles : Int, tileset : Tile.Tileset }
    , key : String
    }
    -> ( ( Float, Float ), Image msg )
viewAnimated { position, spritesheet, key } =
    ( toFloatPosition position
    , Image.fromTile
        (Tile.fromPosition ( 0, 0 )
            |> Tile.animated spritesheet.widthInTiles
        )
        spritesheet.tileset
        |> Image.movable key
    )


toFloatPosition : Position -> ( Float, Float )
toFloatPosition ( x, y ) =
    ( toFloat <| tileSize * x
    , toFloat <| tileSize * y
    )
