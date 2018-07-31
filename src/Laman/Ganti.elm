module Laman.Ganti exposing (..)

import Data.Pengguna as Pengguna
import Data.Sesi as Sesi
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as Http
import Json.Decode as Decode
import Request.KeluarMasuk as KeluarMasuk
import Util exposing (..)
import Validate exposing (Validator, firstError, fromErrors, ifBlank, validate)
import Views.Borang as Borang


type alias Model =
    { galat : List String
    , passwordlama : String
    , passwordbaru : String
    , passwordbaruvalid : String
    }


init : Sesi.Sesi -> Model
init sesi =
    { galat = []
    , passwordlama = ""
    , passwordbaru = ""
    , passwordbaruvalid = ""
    }


initmodel : Model
initmodel =
    { galat = []
    , passwordlama = ""
    , passwordbaru = ""
    , passwordbaruvalid = ""
    }


type Msg
    = AjukanBorangGanti
    | SetPasswordLama String
    | SetPasswordBaru String
    | SetPasswordBaruValid String
    | GantiSelesai (Result Http.Error Pengguna.Pengguna)


type EksternalMsg
    = NoOp
    | SetPenggunaMsg Pengguna.Pengguna


view : Sesi.Sesi -> Model -> Html Msg
view sesi model =
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "column" ] []
            , div [ class "column" ]
                [ header [ class "title" ]
                    [ text "Ganti Password" ]
                , ul [] <| List.map (\a -> li [] [ text a ]) model.galat
                , Html.form [ onSubmit AjukanBorangGanti ]
                    [ Borang.password
                        [ class "input"
                        , onInput SetPasswordLama
                        , placeholder "Sandi Lama"
                        ]
                        []
                    , Borang.password
                        [ class "input"
                        , onInput SetPasswordBaru
                        , placeholder "Sandi Baru"
                        ]
                        []
                    , Borang.password
                        [ class "input"
                        , onInput SetPasswordBaruValid
                        , placeholder "Ulang Sandi Baru"
                        ]
                        []
                    , button [ class "button is-pulled-right is-primary" ]
                        [ text "Ubah" ]
                    ]
                ]
            , div [ class "column" ] []
            ]
        ]


validasimodel : Validator String Model
validasimodel =
    Validate.all
        [ ifBlank .passwordlama "Sandi lama harus diisi."
        , ifBlank .passwordbaru "Sandi baru harus isi."
        , ifBlank .passwordbaruvalid "Sandi baru harus isi."
        , validasiulang
        ]


validasiulang : Validator String Model
validasiulang =
    fromErrors modelkeerror


modelkeerror : Model -> List String
modelkeerror model =
    if model.passwordbaru == model.passwordbaruvalid then
        []
    else
        [ "Harap mengulang kata sandi baru." ]


update : Sesi.Sesi -> Msg -> Model -> ( ( Model, Cmd Msg ), EksternalMsg )
update sesi msg mdl =
    case msg of
        SetPasswordLama s ->
            { mdl | passwordlama = s }
                => Cmd.none
                => NoOp

        SetPasswordBaru s ->
            { mdl | passwordbaru = s }
                => Cmd.none
                => NoOp

        SetPasswordBaruValid s ->
            case validate validasiulang mdl of
                [] ->
                    { mdl | passwordbaruvalid = s }
                        => Cmd.none
                        => NoOp

                x ->
                    { mdl | passwordbaruvalid = s, galat = Debug.log "setpas" x }
                        => Cmd.none
                        => NoOp

        AjukanBorangGanti ->
            case validate validasimodel mdl of
                [] ->
                    let
                        mtoken =
                            Maybe.map .token sesi.pengguna
                    in
                    { mdl | galat = [] }
                        => Http.send GantiSelesai (KeluarMasuk.gantiPassword mtoken mdl)
                        => NoOp

                x ->
                    { mdl | galat = x }
                        => Cmd.none
                        => NoOp

        GantiSelesai (Ok pengguna) ->
            mdl
                => Cmd.none
                => SetPenggunaMsg pengguna

        GantiSelesai (Err r) ->
            let
                pesangalat =
                    case r of
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
            { mdl | galat = [ pesangalat ] }
                => Cmd.none
                => NoOp
