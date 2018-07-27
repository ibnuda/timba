module Main exposing (main)

import DaftarRute as Rute
import Data.Pengguna as Pengguna
import Data.Sesi exposing (Sesi)
import Html exposing (..)
import Json.Decode as Decode
import Laman.Beranda as Beranda
import Laman.Masuk as Masuk
import Navigation exposing (Location)
import Task
import Util exposing (..)


type ModelLamanTermuat
    = LamanKosong
    | LamanTakKetemu
    | LamanMasuk Masuk.Model
    | LamanBeranda Beranda.Model


type KondisiLaman
    = LamanSudahDimuat ModelLamanTermuat
    | PindahDariLaman ModelLamanTermuat


type alias Model =
    { sesi : Sesi
    , kondisilaman : KondisiLaman
    }


type Msg
    = SetRute (Maybe Rute.Rute)
    | SetPengguna (Maybe Pengguna.Pengguna)
    | MasukMsg Masuk.Msg
    | BerandaMsg Beranda.Msg


getLaman : KondisiLaman -> ModelLamanTermuat
getLaman kl =
    case kl of
        LamanSudahDimuat l ->
            l

        PindahDariLaman l ->
            l


setRute : Maybe Rute.Rute -> Model -> ( Model, Cmd msg )
setRute mrute model =
    let
        transisi kemsg task =
            { model | kondisilaman = PindahDariLaman (getLaman model.kondisilaman) }
                => Task.attempt kemsg task
    in
    case ( model.sesi.pengguna, mrute ) of
        ( Nothing, Just Rute.Masuk ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanMasuk Masuk.initmodel) }
                => Cmd.none

        ( Nothing, _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanMasuk Masuk.initmodel) }
                => Cmd.none

        ( Just p, _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanBeranda Beranda.initmodel) }
                => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updateLaman (getLaman model.kondisilaman) msg model


updateLaman : ModelLamanTermuat -> Msg -> Model -> ( Model, Cmd Msg )
updateLaman laman msg model =
    let
        sesi =
            model.sesi

        kelaman kemodel kemsg subupd submsg submod =
            let
                ( modelbaru, cmdbaru ) =
                    subupd submsg submod
            in
            { model | kondisilaman = LamanSudahDimuat (kemodel modelbaru) }
                => Cmd.map kemsg cmdbaru
    in
    case ( msg, laman ) of
        ( SetRute r, _ ) ->
            setRute r model

        ( MasukMsg submsg, LamanMasuk submod ) ->
            let
                ( ( modellaman, cmd ), msgdarilaman ) =
                    Masuk.update submsg submod

                modelbaru =
                    case msgdarilaman of
                        Masuk.NoOp ->
                            model

                        Masuk.SetPenggunaMsg p ->
                            { model | sesi = { pengguna = Just p } }
            in
            { modelbaru | kondisilaman = LamanSudahDimuat (LamanMasuk modellaman) }
                => Cmd.map MasukMsg cmd

        ( SetPengguna p, _ ) ->
            let
                cmd =
                    if sesi.pengguna /= Nothing && p == Nothing then
                        Rute.modifikasiUrl Rute.Beranda
                    else
                        Cmd.none
            in
            { model | sesi = { sesi | pengguna = p } } => cmd

        ( _, _ ) ->
            model
                => Cmd.none


decodePenggunaJson : Decode.Value -> Maybe Pengguna.Pengguna
decodePenggunaJson =
    Decode.decodeValue Decode.string
        >> Result.toMaybe
        >> Maybe.andThen (Decode.decodeString Pengguna.decoder >> Result.toMaybe)


view : Model -> Html Msg
view model =
    case model.kondisilaman of
        LamanSudahDimuat l ->
            viewLaman model.sesi l

        PindahDariLaman l ->
            viewLaman model.sesi l


viewLaman : Sesi -> ModelLamanTermuat -> Html Msg
viewLaman sesi laman =
    let
        bingkai konten =
            div [] [ konten ]
    in
    case laman of
        LamanKosong ->
            Html.text "kosong"
                |> bingkai

        LamanTakKetemu ->
            Html.text "halaman tidak ketemu"
                |> bingkai

        LamanMasuk x ->
            Masuk.view sesi x
                |> bingkai
                |> Html.map MasukMsg

        LamanBeranda _ ->
            Beranda.view sesi.pengguna
                |> bingkai
                |> Html.map BerandaMsg


init : Decode.Value -> Location -> ( Model, Cmd Msg )
init v l =
    setRute (Rute.dariLokasi l)
        { kondisilaman = LamanSudahDimuat LamanKosong
        , sesi = { pengguna = decodePenggunaJson v }
        }


main : Program Decode.Value Model Msg
main =
    Navigation.programWithFlags (Rute.dariLokasi >> SetRute)
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
