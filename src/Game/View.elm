module Game.View exposing (view)

import Color
import Dict
import Dungeon.PropsItens
import Dungeon.Tiles.Wall
import Fonts
import Game.Types exposing (Model, Position, Roll)
import Html exposing (Html)
import LittleMummy.Idle
import LittleMummy.Walk
import PixelEngine
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Options as Options
import PixelEngine.Tile as Tile exposing (Tileset)
import Set
import String.Extra


tileSize : number
tileSize =
    16


view : Model -> Html msg
view model =
    PixelEngine.toHtml
        { width = toFloat model.gameWidth * tileSize
        , options =
            Options.default
                |> Options.withScale (maxScale model)
                |> Options.withAnimationFPS 10
                |> Just
        }
        [ viewTopBar model
        , PixelEngine.imageArea
            { height = toFloat model.gameHeight * tileSize
            , background = PixelEngine.colorBackground Color.blue
            }
            (viewHero model :: viewRolls model ++ viewWalls model)
        , viewStatusMessage model
        ]


viewRolls : Model -> List ( ( Float, Float ), Image msg )
viewRolls model =
    model.rolls
        |> Dict.toList
        |> List.map viewRoll


viewRoll : ( Position, Roll ) -> ( ( Float, Float ), Image msg )
viewRoll ( position, roll ) =
    ( toFloatPosition position
    , Image.fromTile
        (Tile.fromPosition ( 0, 0 ))
        (if roll.reversed then
            Dungeon.PropsItens.flagGreen

         else
            Dungeon.PropsItens.flagRed
        )
    )


viewWalls : Model -> List ( ( Float, Float ), Image msg )
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


viewTopBar : Model -> PixelEngine.Area msg
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
            { rows = 1
            , tileset = textTileset
            , background = PixelEngine.colorBackground Color.white
            }


topBar : Model -> String
topBar model =
    "Level " ++ String.fromInt model.level


viewStatusMessage : Model -> PixelEngine.Area msg
viewStatusMessage model =
    let
        charsPerLine : Int
        charsPerLine =
            model.gameWidth * tileSize // textTileset.spriteHeight - 2
    in
    statusMessage model
        |> String.Extra.softBreak charsPerLine
        |> List.take statusMessageHeight
        |> List.map (Tile.fromText ( 0, 0 ))
        |> List.indexedMap
            (\row tiles ->
                List.indexedMap
                    (\column tile ->
                        ( ( 1 + column
                          , 1 + row
                          )
                        , tile
                        )
                    )
                    tiles
            )
        |> List.concat
        |> PixelEngine.tiledArea
            { rows = statusMessageHeight + 2
            , tileset = textTileset
            , background = PixelEngine.colorBackground Color.white
            }


statusMessage : Model -> String
statusMessage model =
    case model.level of
        1 ->
            "To win reverse the rolls by touching them"

        2 ->
            "Did you know? Nokia used to produce TP rolls!"

        3 ->
            "Avoid the guards!!!"

        l ->
            case modBy 3 (l - 1) of
                0 ->
                    -- -----------------------------------------------------------"
                    "The average roll has 333 sheets, that's plenty!"

                1 ->
                    -- -----------------------------------------------------------"
                    "One person uses ~384 trees of toilet paper in a lifetime."

                _ ->
                    -- -----------------------------------------------------------"
                    "The first use of toilet paper was in China, 6th century."


maxScale : Model -> Int
maxScale model =
    let
        borderWidth : number
        borderWidth =
            tileSize

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
    Fonts.berlin8x8.tileset


viewHero : Model -> ( ( Float, Float ), Image msg )
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
