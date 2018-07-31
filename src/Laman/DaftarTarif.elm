module Laman.DaftarTarif exposing (..)

import Data.Sesi as Sesi
import Data.Tagihan.Tarif as Tarif
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as Http
import Json.Decode as Decode exposing (..)
import Laman.GagalMuat as GagalMuat
import Request.LihatIkhtisar as LihatIkhtisar
import Task exposing (Task)
import Util exposing (..)
import Validate exposing (Validator, ifBlank, ifNotInt, validate)
import Views.Borang as Borang


type alias Model =
    { galat : List String
    , awalharga : String
    , awalsampai : String
    , tengahharga : String
    , tengahsampai : String
    , akhirharga : String
    , biayabeban : String
    , daftartarif : List Tarif.Tarif
    }


init : Sesi.Sesi -> Task GagalMuat.LamanGagalDimuat Model
init sesi =
    let
        mtoken =
            Maybe.map .token sesi.pengguna

        daftartarif =
            LihatIkhtisar.getDaftarTarif mtoken
                |> Http.toTask

        gagalpenangan _ =
            GagalMuat.lamanGagalDimuat "gagal memuat ikhtisar."
    in
    Task.map (Model [] "0" "0" "0" "0" "0" "0") daftartarif
        |> Task.mapError gagalpenangan


type Msg
    = SetAwalHarga String
    | SetAwalSampai String
    | SetTengahHarga String
    | SetTengahSampai String
    | SetAkhirHarga String
    | SetBiayaBeban String
    | AjukanBorang
    | BuatTarifSelesai (Result Http.Error Tarif.Tarif)


type EksternalMsg
    = NoOp
    | SetTarifMsg Tarif.Tarif


validator : Validator String { a | akhirharga : String, awalharga : String, awalsampai : String, biayabeban : String, tengahharga : String, tengahsampai : String }
validator =
    Validate.all
        [ ifNotInt .awalharga "Harus angka."
        , ifNotInt .awalsampai "Harus angka."
        , ifNotInt .tengahharga "Harus angka."
        , ifNotInt .tengahsampai "Harus angka."
        , ifNotInt .akhirharga "Harus angka."
        , ifNotInt .biayabeban "Harus angka."
        ]


view : Sesi.Sesi -> Model -> Html Msg
view sesi model =
    div [ class "content" ]
        [ div [ class "columns" ]
            [ div [ class "column is-two-third" ]
                [ Borang.viewGalat model.galat
                , viewdaftartarif model.daftartarif
                ]
            , div [ class "column is-one-third" ]
                [ h2 [ class "header" ]
                    [ text "Tambah Tarif" ]
                , viewborangtarifbaru
                ]
            ]
        ]


viewdaftartarif : List Tarif.Tarif -> Html Msg
viewdaftartarif daftartarif =
    table [ class "table is-stripped" ]
        [ thead [ class "thead" ]
            [ tr [ class "tr" ]
                [ th [ class "th" ] [ text "Biaya Beban" ]
                , th [ class "th" ] [ text "Harga Awal" ]
                , th [ class "th" ] [ text "Sampai" ]
                , th [ class "th" ] [ text "Harga Tengah" ]
                , th [ class "th" ] [ text "Sampai" ]
                , th [ class "th" ] [ text "Harga Akhir" ]
                ]
            ]
        , tbody [ class "tbody" ] <| List.map viewtarifitem daftartarif
        ]


viewtarifitem : Tarif.Tarif -> Html Msg
viewtarifitem tarif =
    tr [ class "tr" ]
        [ td [ class "td" ] [ text <| toString tarif.biayabeban ]
        , td [ class "td" ] [ text <| toString tarif.hargaawal ]
        , td [ class "td" ] [ text <| toString tarif.sampaiawal ]
        , td [ class "td" ] [ text <| toString tarif.hargatengah ]
        , td [ class "td" ] [ text <| toString tarif.sampaitengah ]
        , td [ class "td" ] [ text <| toString tarif.hargaakhir ]
        ]


viewborangtarifbaru : Html Msg
viewborangtarifbaru =
    Html.form [ onSubmit AjukanBorang, class "" ]
        [ Borang.input
            [ class "input"
            , onInput SetBiayaBeban
            , placeholder "Biaya Beban"
            ]
            []
        , Borang.input
            [ class "input"
            , onInput SetAwalSampai
            , placeholder "Tarif Awal Sampai"
            ]
            []
        , Borang.input
            [ class "input"
            , onInput SetAwalHarga
            , placeholder "Tarif Awal Harga"
            ]
            []
        , Borang.input
            [ class "input"
            , onInput SetTengahSampai
            , placeholder "Tarif Tengah Sampai"
            ]
            []
        , Borang.input
            [ class "input"
            , onInput SetTengahHarga
            , placeholder "Tarif Tengah Harga"
            ]
            []
        , Borang.input
            [ class "input"
            , onInput SetAkhirHarga
            , placeholder "Tarif Selanjutnya Harga"
            ]
            []
        , button [ class "button is-primary is-pulled-right" ]
            [ text "Simpan"
            ]
        ]


update : Sesi.Sesi -> Msg -> Model -> ( ( Model, Cmd Msg ), EksternalMsg )
update sesi msg model =
    let
        dt =
            model.daftartarif
    in
    case msg of
        SetAwalHarga s ->
            { model | awalharga = s } => Cmd.none => NoOp

        SetAwalSampai s ->
            { model | awalsampai = s } => Cmd.none => NoOp

        SetTengahHarga s ->
            { model | tengahharga = s } => Cmd.none => NoOp

        SetTengahSampai s ->
            { model | tengahsampai = s } => Cmd.none => NoOp

        SetAkhirHarga s ->
            { model | akhirharga = s } => Cmd.none => NoOp

        SetBiayaBeban s ->
            { model | biayabeban = s } => Cmd.none => NoOp

        AjukanBorang ->
            case validate validator model of
                [] ->
                    { model | galat = [] }
                        => Http.send BuatTarifSelesai (kirimmodel sesi model)
                        => NoOp

                x ->
                    { model | galat = x }
                        => Cmd.none
                        => NoOp

        BuatTarifSelesai (Ok r) ->
            { model | daftartarif = List.append dt [ r ] }
                => Cmd.none
                => SetTarifMsg r

        BuatTarifSelesai (Err g) ->
            let
                pesangalat =
                    case g of
                        Http.BadStatus r ->
                            r.body
                                |> Decode.decodeString (field "errors" Decode.string)
                                |> Result.withDefault "kok bisa, ya?"

                        Http.BadPayload yangsalah _ ->
                            yangsalah

                        Http.BadUrl u ->
                            u ++ " salah."

                        Http.Timeout ->
                            "timeout."

                        Http.NetworkError ->
                            "cek sambungan internet."
            in
            { model | galat = [ pesangalat ] }
                => Cmd.none
                => NoOp


kirimmodel : Sesi.Sesi -> Model -> Http.Request Tarif.Tarif
kirimmodel sesi model =
    let
        mtoken =
            Maybe.map .token sesi.pengguna

        a =
            paksa model.biayabeban

        b =
            paksa model.awalharga

        c =
            paksa model.awalsampai

        d =
            paksa model.tengahharga

        e =
            paksa model.tengahsampai

        f =
            paksa model.akhirharga
    in
    LihatIkhtisar.postTarif mtoken a b c d e f


modelketarif : Model -> Tarif.Tarif
modelketarif model =
    let
        a =
            paksa model.biayabeban

        b =
            paksa model.awalharga

        c =
            paksa model.awalsampai

        d =
            paksa model.tengahharga

        e =
            paksa model.tengahsampai

        f =
            paksa model.akhirharga
    in
    { biayabeban = a, hargaawal = b, sampaiawal = c, hargatengah = d, sampaitengah = e, hargaakhir = f }
