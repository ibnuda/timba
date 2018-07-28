module Laman.DetailTagihan exposing (..)

import DaftarRute as Rute
import Data.Sesi as Sesi
import Data.Tagihan as Tagihan
import Data.Tagihan.Tarif as Tarif
import Html exposing (..)
import Html.Attributes exposing (..)
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
    div []
        [ a [ Rute.href (Rute.DetailPelanggan tagihan.nomorMeteran) ]
            [ text "Kembali" ]
        , h1 [] [ text "Tagihan Minum" ]
        , p [] [ text <| "Nomor Meteran: " ++ tagihan.nomorMeteran ]
        , p [] [ text <| "Tahun : " ++ toString tagihan.tahun ++ " Bulan " ++ toString tagihan.bulan ]
        , p [] [ text <| "Tanggal Bayar : " ++ tagihan.tanggalBayar ]
        , p [] [ text <| "Minum: " ++ toString (tagihan.minumSekarang - tagihan.minumLalu) ]
        , viewPelanggan tagihan.pengguna
        , viewTarif tagihan.tarif
        ]


viewPelanggan : Tagihan.TagihanPelanggan -> Html msg
viewPelanggan pelanggan =
    div []
        [ p [] [ text <| "Nama Pelanggan: " ++ pelanggan.namaPelanggan ]
        , p [] [ text <| "Nomor Telepon: " ++ pelanggan.nomorTelepon ]
        , p [] [ text <| "Alamat: " ++ pelanggan.alamat ]
        , p [] [ text <| "Wilayah: " ++ pelanggan.wilayah ]
        ]


viewTarif : Tarif.Tarif -> Html msg
viewTarif tarif =
    div []
        [ p [] [ text <| "Biaya Beban :" ++ toString tarif.biayaBeban ]
        , div [ class "pure-g" ] <| List.map viewTarifItem tarif.satuan
        ]


viewTarifItem : Tarif.TarifItem -> Html msg
viewTarifItem tarifitem =
    let
        sampai =
            if tarifitem.sampai == 0 then
                ""
            else
                toString tarifitem.sampai
    in
    div [ class "pure-u-1 pure-u-md-1-3" ]
        [ p [] [ text <| "Mulai: " ++ toString tarifitem.mulai ]
        , p [] [ text <| "Sampai :" ++ sampai ]
        , p [] [ text <| "Harga: " ++ toString tarifitem.harga ]
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
