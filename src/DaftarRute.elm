module DaftarRute exposing (..)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, int, oneOf, parseHash, s, string)


type Rute
    = Beranda
    | Root
    | Masuk
    | DetailPelanggan String
    | Keluar


rute : Parser (Rute -> a) a
rute =
    oneOf
        [ Url.map Beranda (s "")
        , Url.map Masuk (s "masuk")
        , Url.map DetailPelanggan (s "pelanggan" </> string)
        , Url.map Keluar (s "keluar")
        ]


ruteKeString : Rute -> String
ruteKeString laman =
    let
        pecahan =
            case laman of
                Beranda ->
                    []

                Root ->
                    []

                Masuk ->
                    [ "masuk" ]

                DetailPelanggan nomet ->
                    [ "pelanggan", nomet ]

                Keluar ->
                    [ "keluar" ]
    in
    "#/" ++ String.join "/" pecahan


href : Rute -> Attribute msg
href r =
    Attr.href (ruteKeString r)


modifikasiUrl : Rute -> Cmd msg
modifikasiUrl =
    ruteKeString >> Navigation.modifyUrl


dariLokasi : Location -> Maybe Rute
dariLokasi lokasi =
    if String.isEmpty lokasi.hash then
        Just Root
    else
        parseHash rute lokasi
