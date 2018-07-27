module Laman.Beranda exposing (..)

import Data.Pengguna exposing (Pengguna)
import Html exposing (..)


type alias Model =
    { isi : String
    }


initmodel : Model
initmodel =
    { isi = "ini halaman beranda." }


view : Maybe Pengguna -> Html cmd
view mpengguna =
    case mpengguna of
        Nothing ->
            div []
                [ text "antum kok bisa sampai sini?" ]

        Just pengguna ->
            div []
                [ text <| "halo " ++ pengguna.nama ]


type Msg
    = NoOp
