module Laman.RiwayatPelanggan exposing (..)

import DaftarRute as Rute
import Data.DetailPelanggan as DPelanggan
import Data.Sesi as Sesi
import Html exposing (..)
import Html.Attributes exposing (..)
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
    div [ class "content" ]
        [ viewdetail model.detailpelanggan
        , viewriwayat model.detailpelanggan
        ]


viewdetail : DPelanggan.DetailPelanggan -> Html msg
viewdetail dp =
    div []
        [ div []
            [ p [] [ text <| "Nama: " ++ dp.namaPelanggan ]
            , p [] [ text <| "Nomor Meteran: " ++ dp.nomorMeteran ]
            ]
        , div []
            [ p [] [ text <| "Nomor Telepon: " ++ dp.wilayah ]
            , p [] [ text <| "Alamat: " ++ dp.alamat ]
            ]
        , div []
            [ p [] [ text <| "Tanggal Daftar: " ++ dp.tanggalDaftar ]
            , p [] [ text <| "Nomor Telepon: " ++ dp.nomorTelepon ]
            ]
        ]


viewriwayat : DPelanggan.DetailPelanggan -> Html msg
viewriwayat dp =
    table [ class "table" ]
        [ thead [ class "thead" ]
            [ tr [ class "tr" ]
                [ th [ class "th" ] [ text "Tahun" ]
                , th [ class "th" ] [ text "Bulan" ]
                , th [ class "th" ] [ text "Penggunaan Air" ]
                , th [ class "th" ] [ text "Tanggal Bayar" ]
                ]
            ]
        , tbody [ class "tbody" ] <| List.map (viewtagihansimple dp.nomorMeteran) dp.penggunaanAir
        ]


viewtagihansimple : String -> DPelanggan.TagihanSimple -> Html msg
viewtagihansimple nomormeteran ts =
    tr [ class "tr" ]
        [ td [ class "td" ] [ text <| toString ts.tahun ]
        , td [ class "td" ] [ text <| toString ts.bulan ]
        , td [ class "td" ] [ text <| toString <| ts.minumSekarang - ts.minumLalu ]
        , td [ class "td" ]
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
