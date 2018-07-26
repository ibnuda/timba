module Data.Sesi exposing (..)

import Data.AuthToken exposing (AuthToken)
import Data.Pengguna exposing (Pengguna)
import Util exposing ((=>))


type alias Sesi =
    { pengguna : Maybe Pengguna
    }


coba : String -> (AuthToken -> Cmd msg) -> Sesi -> ( List String, Cmd msg )
coba aksicoba kecmd sesi =
    case Maybe.map .token sesi.pengguna of
        Nothing ->
            [ "Anda sudah keluar, silakan masuk lagi untuk " ++ aksicoba ++ "." ] => Cmd.none

        Just token ->
            [] => kecmd token
