module Generate exposing (main)

{-| -}

import Base64
import Elm exposing (File)
import Gen.CodeGen.Generate as Generate
import Image
import Json.Decode
import Json.Encode
import List.Extra


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
    { directory : String
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

                directory : String
                directory =
                    splat
                        |> List.drop 1
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse
                        |> String.join "/"

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


directoryToGen : String -> List InputFile -> File
directoryToGen moduleName files =
    files
        |> List.filterMap (fileToGen moduleName)
        |> Elm.file [ moduleName ]


fileToGen : String -> InputFile -> Maybe Elm.Declaration
fileToGen moduleName { filename, contents } =
    contents
        |> Base64.toBytes
        |> Maybe.andThen Image.decode
        |> Maybe.map
            (\image ->
                let
                    name : String
                    name =
                        toName filename

                    { width, height } =
                        Image.dimensions image
                in
                Elm.record
                    [ ( "path", Elm.string <| moduleName ++ "/" ++ name ++ ".png" )
                    , ( "width", Elm.int width )
                    , ( "height", Elm.int height )
                    ]
                    |> Elm.declaration name
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
