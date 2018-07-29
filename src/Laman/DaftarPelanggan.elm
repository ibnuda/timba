module Laman.DaftarPelanggan exposing (..)

import DaftarRute as Rute
import Data.Pelanggan as Pelanggan
import Data.Sesi as Sesi
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes exposing (..)
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
    div [ class "container" ]
        [ h1 [ class "header" ]
            [ p [ class "header" ]
                [ text "Daftar Pelanggan" ]
            ]
        , table [ class "table" ]
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

                        Http.BadPayload yangsalah _ ->
                            yangsalah

                        Http.BadUrl u ->
                            u ++ " salah."

                        Http.Timeout ->
                            "timeout."

                        Http.NetworkError ->
                            "cek sambungan internet."
            in
            { model | galat = pesangalat } => Cmd.none

        ( Just _, DaftarPelangganTerunduh (Ok dp) ) ->
            { model | daftarpelanggan = dp } => Cmd.none
