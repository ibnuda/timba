module Laman.TambahPelanggan exposing (..)

import DaftarRute as Rute
import Data.Pelanggan as Pelanggan
import Data.Sesi as Sesi
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as Http
import Json.Decode as Decode
import Request.LihatPelanggan as LihatPelanggan
import Util exposing ((=>))
import Validate exposing (Validator, ifBlank, ifNotInt, validate)
import Views.Borang as Borang


type alias Model =
    { galat : List String
    , namapelanggan : String
    , nomortelepon : String
    , password : String
    , alamat : String
    , wilayah : String
    , nomormeteran : String
    }


initmodel : Model
initmodel =
    { galat = []
    , namapelanggan = ""
    , nomortelepon = ""
    , nomormeteran = ""
    , alamat = ""
    , wilayah = ""
    , password = ""
    }


type Msg
    = AjukanBorang
    | SetNamaPelanggan String
    | SetNomorTelepon String
    | SetNomorMeteran String
    | SetAlamat String
    | SetWilayah String
    | SetPassword String
    | TambahPelangganSelesai (Result Http.Error Pelanggan.Pelanggan)


validator : Validator String { a | alamat : String, namapelanggan : String, nomormeteran : String, nomortelepon : String, password : String, wilayah : String }
validator =
    Validate.all
        [ ifBlank .namapelanggan "Nama pelanggan tidak boleh kosong."
        , ifBlank .nomormeteran "Nomor meteran harap diisi."
        , ifBlank .nomortelepon "Nomor telepon harap diisi."
        , ifBlank .alamat "Alamat harus diisi."
        , ifBlank .wilayah "Wilayah harus diisi."
        , ifBlank .password "Password tidak boleh kosong."
        ]


update : Sesi.Sesi -> Msg -> Model -> ( Model, Cmd Msg )
update sesi msg model =
    case msg of
        SetNamaPelanggan s ->
            { model | namapelanggan = s }
                => Cmd.none

        SetNomorTelepon s ->
            { model | nomortelepon = s }
                => Cmd.none

        SetNomorMeteran s ->
            { model | nomormeteran = s }
                => Cmd.none

        SetAlamat s ->
            { model | alamat = s }
                => Cmd.none

        SetWilayah s ->
            { model | wilayah = s }
                => Cmd.none

        SetPassword s ->
            { model | password = s }
                => Cmd.none

        AjukanBorang ->
            case validate validator model of
                [] ->
                    { model | galat = [] }
                        => Http.send TambahPelangganSelesai (kirimPelanggan sesi model)

                x ->
                    { model | galat = x }
                        => Cmd.none

        TambahPelangganSelesai (Ok r) ->
            model
                => Rute.modifikasiUrl Rute.DaftarPelanggan

        TambahPelangganSelesai (Err g) ->
            let
                pesangalat =
                    case g of
                        Http.BadStatus r ->
                            r.body
                                |> Decode.decodeString (Decode.field "errors" Decode.string)
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


kirimPelanggan : Sesi.Sesi -> Model -> Http.Request Pelanggan.Pelanggan
kirimPelanggan sesi model =
    let
        mtoken =
            Maybe.map .token sesi.pengguna
    in
    LihatPelanggan.postTambahPelanggan mtoken model


view : Sesi.Sesi -> Model -> Html Msg
view sesi model =
    div [ class "content" ]
        [ div [ class "columns" ]
            [ div [ class "column is-one-fifth" ]
                []
            , div [ class "column auto" ]
                [ h2 [ class "header" ] [ text "Tambah Pelanggan" ]
                , Borang.viewGalat model.galat
                , viewborangpenggunabaru
                ]
            , div [ class "column is-one-fifth" ]
                []
            ]
        ]


viewborangpenggunabaru : Html Msg
viewborangpenggunabaru =
    Html.form [ onSubmit AjukanBorang, class "" ]
        [ Borang.input
            [ class "input"
            , onInput SetNamaPelanggan
            , placeholder "Nama Pelanggan"
            ]
            []
        , Borang.input
            [ class "input"
            , onInput SetNomorTelepon
            , placeholder "Nomor Telepon"
            ]
            []
        , Borang.password
            [ class "input"
            , onInput SetPassword
            , placeholder "Password Pengguna"
            ]
            []
        , Borang.input
            [ class "input"
            , onInput SetAlamat
            , placeholder "Alamat"
            ]
            []
        , Borang.input
            [ class "input"
            , onInput SetWilayah
            , placeholder "Wilayah"
            ]
            []
        , Borang.input
            [ class "input"
            , onInput SetNomorMeteran
            , placeholder "NomorMeteran"
            ]
            []
        , button [ class "button is-primary is-pulled-right" ]
            [ text "Tambah"
            ]
        ]
