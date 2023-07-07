module Effect exposing (Config, Effect(..), none, toCmd)


type alias Config msg =
    { noop : msg }


type Effect
    = Batch (List Effect)


none : Effect
none =
    batch []


batch : List Effect -> Effect
batch =
    Batch


toCmd : Config msg -> Effect -> Cmd msg
toCmd config effect =
    case effect of
        Batch children ->
            Cmd.batch (List.map (toCmd config) children)
