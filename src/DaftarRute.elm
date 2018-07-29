module DaftarRute exposing (..)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, int, oneOf, parseHash, s, string)


type Rute
    = Beranda
    | Root
    | Masuk
    | DaftarPelanggan
    | DaftarTarif
    | DetailPelanggan String
    | DetailTagihan String Int Int
    | Keluar


rute : Parser (Rute -> a) a
rute =
    oneOf
        [ Url.map Masuk (s "masuk")
        , Url.map Beranda (s "")
        , Url.map DaftarPelanggan (s "pelanggan")
        , Url.map DaftarTarif (s "tarif")
        , Url.map DetailPelanggan (s "pelanggan" </> string)
        , Url.map DetailTagihan (s "tagihan" </> string </> int </> int)
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

                DaftarPelanggan ->
                    [ "pelanggan" ]

                DaftarTarif ->
                    [ "tarif" ]

                DetailPelanggan nomet ->
                    [ "pelanggan", nomet ]

                DetailTagihan nomet tahun bulan ->
                    [ "tagihan", nomet, toString tahun, toString bulan ]

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
