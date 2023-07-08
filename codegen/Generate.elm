module Generate exposing (main)

{-| -}

import Base64
import Elm exposing (File)
import Gen.CodeGen.Generate as Generate
import Gen.PixelEngine.Tile
import Image
import Json.Decode
import Json.Encode
import List.Extra
import String.Extra


main : Program Json.Encode.Value () ()
main =
    Generate.fromJson
        (Json.Decode.list decodeFile)
        (\files ->
            List.map
                (\( first, rest ) ->
                    directoryToGen first.directory (first :: rest)
                )
                (List.Extra.gatherEqualsBy .directory files)
        )


type alias InputFile =
    { directory : List String
    , filename : String
    , contents : String
    }


decodeFile : Json.Decode.Decoder InputFile
decodeFile =
    Json.Decode.map2
        (\path contents ->
            let
                splat : List String
                splat =
                    String.split "/" path

                directory : List String
                directory =
                    splat
                        |> List.drop 1
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse

                filename : String
                filename =
                    splat
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault "???"
            in
            { directory = directory
            , filename = filename
            , contents = contents
            }
        )
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "contents" Json.Decode.string)


directoryToGen : List String -> List InputFile -> File
directoryToGen moduleName files =
    files
        |> List.filterMap (fileToGen moduleName)
        |> Elm.file (List.map String.Extra.classify moduleName)


fileToGen : List String -> InputFile -> Maybe Elm.Declaration
fileToGen moduleName { filename, contents } =
    let
        name : String
        name =
            toName filename

        path =
            String.join "/" (moduleName ++ [ name ++ ".png" ])
    in
    contents
        |> Base64.toBytes
        |> Maybe.andThen Image.decode
        |> Maybe.map
            (\image ->
                let
                    { width, height } =
                        Image.dimensions image
                in
                if List.member "Dungeon" moduleName || List.member "Fonts" moduleName then
                    let
                        size : number
                        size =
                            if String.contains "8x8" name then
                                8

                            else
                                16

                        widthInTiles : Int
                        widthInTiles =
                            width // size

                        heightInTiles : Int
                        heightInTiles =
                            height // size
                    in
                    if widthInTiles == 1 && heightInTiles == 1 then
                        Gen.PixelEngine.Tile.tileset
                            { source = "img/" ++ path
                            , spriteWidth = size
                            , spriteHeight = size
                            }

                    else
                        Elm.record
                            [ ( "tileset"
                              , Gen.PixelEngine.Tile.tileset
                                    { source = "img/" ++ path
                                    , spriteWidth = size
                                    , spriteHeight = size
                                    }
                              )
                            , ( "widthInTiles", Elm.int widthInTiles )
                            ]

                else
                    Elm.record
                        [ ( "path", Elm.string path )
                        , ( "width", Elm.int width )
                        , ( "height", Elm.int height )
                        ]
            )
        |> Maybe.map
            (\value ->
                value
                    |> Elm.declaration (String.Extra.camelize name)
                    |> Elm.expose
            )


toName : String -> String
toName f =
    let
        cleaned =
            f
                |> String.split "."
                |> List.reverse
                |> List.drop 1
                |> List.reverse
                |> String.join "."
    in
    case String.uncons cleaned of
        Nothing ->
            cleaned

        Just ( first, _ ) ->
            if Char.isDigit first then
                "i_" ++ cleaned

            else
                cleaned
