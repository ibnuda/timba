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
        , viewtariflagi tagihan.tarif (tagihan.minumSekarang - tagihan.minumLalu)
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


dikurangiataunol : Int -> Int -> Int -> Int
dikurangiataunol mulai sampai penggunaan =
    if mulai > penggunaan then
        0
    else if penggunaan > sampai then
        sampai - mulai
    else
        penggunaan - sampai


selisihataunol : Int -> Int -> Int
selisihataunol mulai penggunaan =
    if mulai > penggunaan then
        0
    else
        penggunaan - mulai


viewtariflagi : Tarif.Tarif -> Int -> Html msg
viewtariflagi tarif penggunaan =
    let
        gunaawal =
            dikurangiataunol 0 tarif.sampaiawal penggunaan

        gunatengah =
            dikurangiataunol tarif.sampaiawal tarif.sampaitengah penggunaan

        gunaakhir =
            selisihataunol tarif.sampaitengah penggunaan
    in
    div [ class "content" ]
        [ table [ class "table" ]
            [ thead [ class "thead" ]
                [ th [ class "th" ] [ text "Mulai" ]
                , th [ class "th" ] [ text "Sampai" ]
                , th [ class "th" ] [ text "Harga" ]
                , th [ class "th" ] [ text "Penggunaan" ]
                , th [ class "th" ] [ text "Bayar" ]
                ]
            , tbody [ class "tbody" ]
                [ tr [ class "tr" ]
                    [ td [ class "td" ] [ text "0 M3" ]
                    , td [ class "td" ] [ text <| toString tarif.sampaiawal ++ " M3" ]
                    , td [ class "td" ] [ text <| "Rp. " ++ toString tarif.hargaawal ]
                    , td [ class "td" ] [ text <| toString gunaawal ++ " M3" ]
                    , td [ class "td" ] [ text <| "Rp. " ++ toString (gunaawal * tarif.hargaawal) ]
                    ]
                , tr [ class "tr" ]
                    [ td [ class "td" ] [ text <| toString tarif.sampaiawal ++ " M3" ]
                    , td [ class "td" ] [ text <| toString tarif.sampaitengah ++ " M3" ]
                    , td [ class "td" ] [ text <| "Rp. " ++ toString tarif.hargatengah ]
                    , td [ class "td" ] [ text <| toString gunatengah ++ " M3" ]
                    , td [ class "td" ] [ text <| "Rp. " ++ toString (gunatengah * tarif.hargatengah) ]
                    ]
                , tr [ class "tr" ]
                    [ td [ class "td" ] [ text <| toString tarif.sampaitengah ++ " M3" ]
                    , td [ class "td" ] [ text "-" ]
                    , td [ class "td" ] [ text <| "Rp. " ++ toString tarif.hargaakhir ]
                    , td [ class "td" ] [ text <| toString gunaakhir ++ " M3" ]
                    , td [ class "td" ] [ text <| "Rp. " ++ toString (gunaakhir * tarif.hargaakhir) ]
                    ]
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
