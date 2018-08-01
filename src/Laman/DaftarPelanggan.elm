module Laman.DaftarPelanggan exposing (..)

import DaftarRute as Rute
import Data.Pelanggan as Pelanggan
import Data.Sesi as Sesi
import Html exposing (..)
import Html.Attributes exposing (..)
import Http as Http
import Json.Decode as Decode
import Laman.GagalMuat as GagalMuat
import Request.LihatPelanggan as LihatPelanggan
import Task exposing (Task)
import Util exposing ((=>), penangangalat)


type alias Model =
    { galat : String
    , daftarpelanggan : List Pelanggan.Pelanggan
    }


init : Sesi.Sesi -> Task GagalMuat.LamanGagalDimuat Model
init sesi =
    let
        mtoken =
            Maybe.map .token sesi.pengguna

        daftarpelanggan =
            LihatPelanggan.getDaftarPelanggan mtoken
                |> Http.toTask

        gagalpenangan =
            penangangalat
                >> GagalMuat.lamanGagalDimuat
    in
    Task.map (Model "") daftarpelanggan
        |> Task.mapError gagalpenangan


view : Sesi.Sesi -> Model -> Html msg
view _ model =
    div [ class "content" ]
        [ h1 [ class "header" ]
            [ p [ class "header" ]
                [ text "Daftar Pelanggan"
                , a [ Rute.href Rute.TambahPelanggan, class "button is-pulled-right is-primary" ]
                    [ text "Tambah Pelanggan" ]
                ]
            ]
        , div [ class "section" ]
            [ table [ class "table is-stripped is-fullwidth table-container" ]
                [ thead [ class "thead" ]
                    [ tr [ class "tr" ]
                        [ th [ class "th" ] [ text "Nama" ]
                        , th [ class "th" ] [ text "Nomor Telepon" ]
                        , th [ class "th" ] [ text "Alamat" ]
                        , th [ class "th" ] [ text "Wilayah" ]
                        , th [ class "th" ] [ text "Nomor Meteran" ]
                        ]
                    ]
                , tbody [ class "tbody" ] <| List.map viewdatapelanggan model.daftarpelanggan
                ]
            ]
        ]


viewdatapelanggan : Pelanggan.Pelanggan -> Html msg
viewdatapelanggan pelanggan =
    tr [ class "tr" ]
        [ td [ class "td" ] [ text pelanggan.namaPelanggan ]
        , td [ class "td" ] [ text pelanggan.nomorTelepon ]
        , td [ class "td" ] [ text pelanggan.alamat ]
        , td [ class "td" ] [ text pelanggan.wilayah ]
        , td [ class "td" ]
            [ a [ Rute.href (Rute.DetailPelanggan pelanggan.nomorMeteran) ]
                [ text pelanggan.nomorMeteran ]
            ]
        ]


type Msg
    = NoOp
    | DaftarPelangganTerunduh (Result Http.Error (List Pelanggan.Pelanggan))


update : Sesi.Sesi -> Msg -> Model -> ( Model, Cmd Msg )
update sesi msg model =
    case msg of
        NoOp ->
            model => Cmd.none

        DaftarPelangganTerunduh (Err g) ->
            let
                pesangalat =
                    penangangalat g
            in
            { model | galat = pesangalat } => Cmd.none

        DaftarPelangganTerunduh (Ok dp) ->
            { model | daftarpelanggan = dp } => Cmd.none
