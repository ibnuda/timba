module Laman.DetailTagihan exposing (..)

import DaftarRute as Rute
import Data.Sesi as Sesi
import Data.Tagihan as Tagihan
import Data.Tagihan.Tarif as Tarif
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http as Http
import Json.Decode as Decode
import Laman.GagalMuat as GagalMuat
import Request.TagihanPelanggan as TagihanPelanggan
import Task exposing (Task)
import Util exposing ((=>))


type alias Model =
    { galat : String
    , detailtagihan : Tagihan.Tagihan
    }


init : Sesi.Sesi -> String -> Int -> Int -> Task GagalMuat.LamanGagalDimuat Model
init sesi nomet tahun bulan =
    let
        mtoken =
            Maybe.map .token sesi.pengguna

        detailtagihan =
            TagihanPelanggan.getSpesifik mtoken nomet tahun bulan
                |> Http.toTask

        gagalpenangan _ =
            GagalMuat.lamanGagalDimuat "gagal memuat detail tagihan."
    in
    Task.map (Model "") detailtagihan
        |> Task.mapError gagalpenangan


view : Sesi.Sesi -> Model -> Html msg
view _ model =
    div []
        [ viewTagihan model.detailtagihan
        ]


viewTagihan : Tagihan.Tagihan -> Html msg
viewTagihan tagihan =
    div [ class "container" ]
        [ nav [ class "breadcrumb", ariaLabel "breadcrumbs" ]
            [ ul []
                [ li [] [ a [ Rute.href Rute.Beranda ] [ text "Beranda" ] ]
                , li [] [ a [ Rute.href Rute.DaftarPelanggan ] [ text "Daftar Pelanggan" ] ]
                , li []
                    [ a
                        [ Rute.href (Rute.DetailPelanggan tagihan.nomorMeteran)
                        , class "is-active"
                        , attribute "aria-current" "page"
                        ]
                        [ text tagihan.pengguna.namaPelanggan ]
                    ]
                ]
            ]
        , div [ class "columns" ]
            [ viewpelanggan tagihan.pengguna
            , viewtagihan tagihan
            ]
        , viewtarif tagihan.tarif
        ]


viewpelanggan : Tagihan.TagihanPelanggan -> Html msg
viewpelanggan pelanggan =
    div [ class "column" ]
        [ table [ class "table is-stripped table-container" ]
            [ viewbaristabel "Nama Pelanggan" pelanggan.namaPelanggan
            , viewbaristabel "Nomor Telepon" pelanggan.nomorTelepon
            , viewbaristabel "Alamat" pelanggan.alamat
            , viewbaristabel "Wilayah" pelanggan.wilayah
            ]
        ]


viewtagihan : Tagihan.Tagihan -> Html msg
viewtagihan tagihan =
    let
        minum =
            toString <| tagihan.minumSekarang - tagihan.minumLalu
    in
    div [ class "column" ]
        [ table [ class "table table-container" ]
            [ viewbaristabel "Nomor Meteran" tagihan.nomorMeteran
            , viewbaristabel "Tanggal Tagihan" <| toString tagihan.tahun ++ " Bulan " ++ toString tagihan.bulan
            , viewbaristabel "Penggunaan Air" <| minum ++ " M3"
            ]
        ]


viewbaristabel : String -> String -> Html msg
viewbaristabel d i =
    tr [ class "tr" ]
        [ th [ class "th is-narrow" ] [ text d ]
        , td [ class "td" ] [ text i ]
        ]


viewbagian : String -> String -> String -> Html msg
viewbagian detail info satuan =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ p [] [ text <| detail ++ ":" ] ]
        , div [ class "column" ]
            [ p [] [ text <| info ++ " " ++ satuan ] ]
        ]


viewtarif : Tarif.Tarif -> Html msg
viewtarif tarif =
    div [ class "tile is-ancestor" ]
        [ div [ class "tile is-parent" ]
            [ div [ class "tile is-child is-4 box" ]
                [ viewbagian "Mulai" "0" "M3"
                , viewbagian "Sampai" (toString tarif.sampaiawal) "M3"
                , viewbagian "Harga" (toString tarif.hargaawal) "Rp"
                ]
            , div [ class "tile is-child is-4 box" ]
                [ viewbagian "Mulai" (toString tarif.sampaiawal) "M3"
                , viewbagian "Sampai" (toString tarif.sampaitengah) "M3"
                , viewbagian "Harga" (toString tarif.hargatengah) "Rp"
                ]
            , div [ class "tile is-child is-4 box" ]
                [ viewbagian "Mulai" (toString tarif.sampaitengah) "M3"
                , viewbagian "Harga" (toString tarif.hargaakhir) "Rp"
                ]
            ]
        ]


type Msg
    = NoOp
    | DetailTagihanTerunduh (Result Http.Error Tagihan.Tagihan)


update : Sesi.Sesi -> Msg -> Model -> ( Model, Cmd Msg )
update sesi msg model =
    case ( sesi.pengguna, msg ) of
        ( Nothing, _ ) ->
            model
                => Rute.modifikasiUrl Rute.Masuk

        ( _, NoOp ) ->
            model
                => Cmd.none

        ( _, DetailTagihanTerunduh (Ok dt) ) ->
            { model | detailtagihan = dt }
                => Cmd.none

        ( _, DetailTagihanTerunduh (Err g) ) ->
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
            { model | galat = pesangalat }
                => Cmd.none
