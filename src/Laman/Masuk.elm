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
        [ h2 [ class "title" ] [ text "Masuk" ]
        , Borang.viewGalat model.galat
        , viewBorang
        ]



--      <a href="#" class="card-footer-item">Save</a>
--      <a href="#" class="card-footer-item">Edit</a>
--      <a href="#" class="card-footer-item">Delete</a>
--  <div class="card">
--  <header class="card-header">
--      <p class="card-header-title">
--      Component
--      </p>
--      <a href="#" class="card-header-icon" aria-label="more options">
--      <span class="icon">
--          <i class="fas fa-angle-down" aria-hidden="true"></i>
--      </span>
--      </a>
--  </header>
--  <div class="card-content">
--      <div class="content">
--      Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus nec iaculis mauris.
--      <a href="#">@bulmaio</a>. <a href="#">#css</a> <a href="#">#responsive</a>
--      <br>
--      <time datetime="2016-1-1">11:09 PM - 1 Jan 2016</time>
--      </div>
--  </div>
--  <footer class="card-footer">
--      <a href="#" class="card-footer-item">Save</a>
--      <a href="#" class="card-footer-item">Edit</a>
--      <a href="#" class="card-footer-item">Delete</a>
--  </footer>
--  </div>


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
        , button [ class "button is-link" ]
            [ text "Masuk"
            ]
        ]
