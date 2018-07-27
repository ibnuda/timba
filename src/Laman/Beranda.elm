module Laman.Beranda exposing (..)

import DaftarRute as Rute
import Data.Pelanggan as Pelanggan
import Data.Sesi as Sesi
import Html exposing (..)
import Http as Http
import Json.Decode as Decode
import Laman.GagalMuat as GagalMuat
import Request.LihatPelanggan as LihatPelanggan
import Task exposing (Task)
import Util exposing ((=>))


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

        gagalpenangan _ =
            GagalMuat.lamanGagalDimuat "gagal memuat daftar pelanggan"
    in
    Task.map (Model "") daftarpelanggan
        |> Task.mapError gagalpenangan


view : Sesi.Sesi -> Model -> Html msg
view _ model =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Nama" ]
                , th [] [ text "Nomor Telepon" ]
                , th [] [ text "Alamat" ]
                , th [] [ text "Wilayah" ]
                , th [] [ text "Nomor Meteran" ]
                ]
            ]
        , tbody [] <| List.map viewdatapelanggan model.daftarpelanggan
        ]


viewdatapelanggan : Pelanggan.Pelanggan -> Html msg
viewdatapelanggan pelanggan =
    tr []
        [ td [] [ text pelanggan.namaPelanggan ]
        , td [] [ text pelanggan.nomorTelepon ]
        , td [] [ text pelanggan.alamat ]
        , td [] [ text pelanggan.wilayah ]
        , td []
            [ a [ Rute.href (Rute.DataPelanggan pelanggan.nomorMeteran) ]
                [ text pelanggan.nomorMeteran ]
            ]
        ]


type Msg
    = NoOp
    | DaftarPelangganTerunduh (Result Http.Error (List Pelanggan.Pelanggan))


update : Sesi.Sesi -> Msg -> Model -> ( Model, Cmd Msg )
update sesi msg model =
    case ( sesi.pengguna, msg ) of
        ( Nothing, _ ) ->
            model => Rute.modifikasiUrl Rute.Masuk

        ( Just _, NoOp ) ->
            model => Cmd.none

        ( Just _, DaftarPelangganTerunduh (Err g) ) ->
            let
                pesangalat =
                    case g of
                        Http.BadStatus r ->
                            r.body
                                |> Decode.decodeString Decode.string
                                |> Result.withDefault "bad code."

                        _ ->
                            "Lanjutkan nanti."
            in
            { model | galat = pesangalat } => Cmd.none

        ( Just _, DaftarPelangganTerunduh (Ok dp) ) ->
            { model | daftarpelanggan = dp } => Cmd.none
