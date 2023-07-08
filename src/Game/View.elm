module Game.View exposing (view)

import Color
import Dict
import Dungeon.Heroes.Knight
import Dungeon.PropsItens
import Dungeon.Tiles.Wall
import Fonts
import Game.Types exposing (Model, Position, Roll)
import Html exposing (Html)
import PixelEngine
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Options as Options
import PixelEngine.Tile as Tile exposing (Tileset)
import Set
import String.Extra


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
        [ PixelEngine.imageArea
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


viewStatusMessage : Model -> PixelEngine.Area msg
viewStatusMessage model =
    let
        charsPerLine : Int
        charsPerLine =
            model.gameWidth * tileSize // textTileset.spriteHeight - 2
    in
    statusMessage model
        |> String.Extra.softBreak charsPerLine
        |> List.take textHeight
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
            { rows = textHeight * 2
            , tileset = textTileset
            , background = PixelEngine.colorBackground Color.white
            }


statusMessage : Model -> String
statusMessage _ =
    "To win the game, reverse the rolls!"


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
                    + ((textHeight + 2) * toFloat textTileset.spriteHeight)
                  )

        maxScaleMin : Float
        maxScaleMin =
            min maxScaleWidth maxScaleHeight
    in
    2 ^ floor (logBase 2 maxScaleMin)


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
                    Dungeon.Heroes.Knight.knightRunSpritesheet

                else
                    Dungeon.Heroes.Knight.knightRunSpritesheetFlipped

            else if model.hero.facingRight then
                Dungeon.Heroes.Knight.knightIdleSpritesheet

            else
                Dungeon.Heroes.Knight.knightIdleSpritesheetFlipped
    in
    viewAnimated
        { spritesheet = spritesheet
        , position = model.hero.position
        , key = "hero"
        }


textHeight : number
textHeight =
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


tileSize : number
tileSize =
    16
