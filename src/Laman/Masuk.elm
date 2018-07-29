module Laman.Masuk exposing (..)

import DaftarRute exposing (..)
import Data.Pengguna exposing (Pengguna)
import Data.Sesi exposing (Sesi)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Request.KeluarMasuk exposing (..)
import Util exposing (..)
import Validate exposing (Validator, ifBlank, validate)
import Views.Borang as Borang exposing (..)


type alias Model =
    { galat : List String
    , telepon : String
    , sandi : String
    }


initmodel : Model
initmodel =
    { galat = []
    , telepon = ""
    , sandi = ""
    }


type Msg
    = AjukanBorang
    | SetTelepon String
    | SetSandi String
    | MasukSelesai (Result Http.Error Pengguna)


type EksternalMsg
    = NoOp
    | SetPenggunaMsg Pengguna


update : Msg -> Model -> ( ( Model, Cmd Msg ), EksternalMsg )
update mess model =
    case mess of
        SetSandi s ->
            { model | sandi = s }
                => Cmd.none
                => NoOp

        SetTelepon t ->
            { model | telepon = t }
                => Cmd.none
                => NoOp

        MasukSelesai (Err g) ->
            let
                pesangalat =
                    case g of
                        Http.BadStatus r ->
                            r.body
                                |> decodeString (field "errors" Decode.string)
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

        MasukSelesai (Ok p) ->
            model
                => Cmd.batch
                    [ simpanSesi p
                    , modifikasiUrl Beranda
                    ]
                => SetPenggunaMsg p

        AjukanBorang ->
            case validate validatorModel model of
                [] ->
                    { model | galat = [] }
                        => Http.send MasukSelesai (Request.KeluarMasuk.masuk model)
                        => NoOp

                x ->
                    { model | galat = x }
                        => Cmd.none
                        => NoOp


validatorModel : Validator String { a | sandi : String, telepon : String }
validatorModel =
    Validate.all
        [ ifBlank .telepon "telepon tidak boleh kosong."
        , ifBlank .sandi "sandi tidak boleh kosong."
        ]


view : Sesi -> Model -> Html Msg
view _ model =
    div []
        [ h2 [ class "title has-text-centered" ] [ text "Masuk" ]
        , div [ class "columns" ]
            [ div [ class "column one-third" ] []
            , div [ class "column one-third" ]
                [ Borang.viewGalat model.galat
                , viewBorang
                ]
            , div [ class "column one-third" ] []
            ]
        ]


viewBorang : Html Msg
viewBorang =
    Html.form [ onSubmit AjukanBorang, class "" ]
        [ Borang.input
            [ class "input"
            , onInput SetTelepon
            , placeholder "Telepon"
            ]
            []
        , Borang.password
            [ class "input"
            , onInput SetSandi
            , placeholder "Sandi"
            ]
            []
        , button [ class "button is-link is-pulled-right" ]
            [ text "Masuk"
            ]
        ]
