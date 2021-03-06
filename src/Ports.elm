port module Ports exposing (..)

import Json.Encode exposing (Value)


port storeSession : Maybe String -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg
