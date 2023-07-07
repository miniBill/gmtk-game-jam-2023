port module GamepadPort exposing (onBlob, saveToLocalStorage)

import Gamepad.Advanced exposing (Blob)


port onBlob : (Blob -> msg) -> Sub msg


port saveToLocalStorage : String -> Cmd a
