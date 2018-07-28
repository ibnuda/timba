module Laman.RiwayatPelanggan exposing (..)

import DaftarRute as Rute
import Data.DetailPelanggan as DPelanggan
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
    , detailpelanggan : DPelanggan.DetailPelanggan
    }


init : Sesi.Sesi -> String -> Task GagalMuat.LamanGagalDimuat Model
init sesi nomormeteran =
    let
        mtoken =
            Maybe.map .token sesi.pengguna

        dpel =
            LihatPelanggan.getDetailPelanggan mtoken nomormeteran
                |> Http.toTask

        gagalpenangan _ =
            GagalMuat.lamanGagalDimuat "gagal memuat detail pelanggan"
    in
    Task.map (Model "") dpel
        |> Task.mapError gagalpenangan


view : Sesi.Sesi -> Model -> Html msg
view _ model =
    div []
        [ p [] [ text <| "Nama: " ++ model.detailpelanggan.namaPelanggan ]
        , p [] [ text <| "Nomor Meteran: " ++ model.detailpelanggan.nomorMeteran ]
        , p [] [ text <| "Nomor Telepon: " ++ model.detailpelanggan.nomorTelepon ]
        , p [] [ text <| "Alamat: " ++ model.detailpelanggan.alamat ]
        , p [] [ text <| "Wilayah: " ++ model.detailpelanggan.wilayah ]
        , p [] [ text <| "Tanggah Daftar: " ++ model.detailpelanggan.tanggalDaftar ]
        , viewriwayat model.detailpelanggan
        ]


viewriwayat : DPelanggan.DetailPelanggan -> Html msg
viewriwayat dp =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Tahun" ]
                , th [] [ text "Bulan" ]
                , th [] [ text "Penggunaan Air" ]
                , th [] [ text "Tanggal Bayar" ]
                ]
            ]
        , tbody [] <| List.map (viewtagihansimple dp.nomorMeteran) dp.penggunaanAir
        ]


viewtagihansimple : String -> DPelanggan.TagihanSimple -> Html msg
viewtagihansimple nomormeteran ts =
    tr []
        [ td [] [ text <| toString ts.tahun ]
        , td [] [ text <| toString ts.bulan ]
        , td [] [ text <| toString <| ts.minumSekarang - ts.minumLalu ]
        , td []
            [ a [ Rute.href (Rute.DetailTagihan nomormeteran ts.tahun ts.bulan) ]
                [ text ts.tanggalBayar ]
            ]
        ]


type Msg
    = NoOp
    | DetailPelangganTerunduh (Result Http.Error DPelanggan.DetailPelanggan)


update : Sesi.Sesi -> Msg -> Model -> ( Model, Cmd Msg )
update sesi msg model =
    case ( sesi.pengguna, msg ) of
        ( Nothing, _ ) ->
            model => Rute.modifikasiUrl Rute.Masuk

        ( Just _, NoOp ) ->
            model => Cmd.none

        ( Just _, DetailPelangganTerunduh (Err g) ) ->
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

        ( Just _, DetailPelangganTerunduh (Ok dp) ) ->
            { model | detailpelanggan = dp } => Cmd.none
