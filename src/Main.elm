module Main exposing (main)

import DaftarRute as Rute
import Data.Pengguna as Pengguna
import Data.Sesi exposing (Sesi)
import Html exposing (..)
import Json.Decode as Decode
import Laman.Ikhtisar as Ikhtisar
import Laman.DaftarPelanggan as DaftarPelanggan
import Laman.GagalMuat as GagalMuat
import Laman.Masuk as Masuk
import Laman.RiwayatPelanggan as RiwayatPelanggan
import Laman.DetailTagihan as DetailTagihan 
import Navigation exposing (Location)
import Ports
import Task
import Util exposing (..)
import Views.Bingkai as Bingkai


type ModelLamanTermuat
    = LamanKosong
    | LamanTakKetemu
    | LamanMasuk Masuk.Model
    | LamanIkhtisar Ikhtisar.Model
    | LamanDaftarPelanggan DaftarPelanggan.Model
    | LamanRiwayatPelanggan RiwayatPelanggan.Model
    | LamanDetailTagihan DetailTagihan.Model
    | LamanKeluar
    | LamanGagalMuat GagalMuat.LamanGagalDimuat


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
    | DaftarPelangganMsg DaftarPelanggan.Msg
    | DetailTagihanMsg DetailTagihan.Msg
    | RiwayatPelangganMsg RiwayatPelanggan.Msg
    | IkhtisarTermuat (Result GagalMuat.LamanGagalDimuat Ikhtisar.Model)
    | DaftarPelangganTermuat (Result GagalMuat.LamanGagalDimuat DaftarPelanggan.Model)
    | DetailPelangganTermuat (Result GagalMuat.LamanGagalDimuat RiwayatPelanggan.Model)
    | DetailTagihanTermuat (Result GagalMuat.LamanGagalDimuat DetailTagihan.Model)


getLaman : KondisiLaman -> ModelLamanTermuat
getLaman kl =
    case kl of
        LamanSudahDimuat l ->
            l

        PindahDariLaman l ->
            l


setRute : Maybe Rute.Rute -> Model -> ( Model, Cmd Msg )
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

        ( Just p, Just Rute.Beranda) ->
            transisi IkhtisarTermuat (Ikhtisar.init model.sesi)

        ( Just p, Just Rute.DaftarPelanggan ) ->
            transisi DaftarPelangganTermuat (DaftarPelanggan.init model.sesi)

        ( Just p, Just (Rute.DetailPelanggan nomormeteran) ) ->
            transisi DetailPelangganTermuat (RiwayatPelanggan.init model.sesi nomormeteran)

        (Just p, Just (Rute.DetailTagihan nomet tahun bulan)) ->
            transisi DetailTagihanTermuat (DetailTagihan.init model.sesi nomet tahun bulan)

        ( Just p, Just Rute.Keluar ) ->
            let
                sesi =
                    model.sesi
            in
            { model | sesi = { sesi | pengguna = Nothing } }
                => Cmd.batch
                    [ Ports.storeSession Nothing
                    , Rute.modifikasiUrl Rute.Masuk
                    ]

        ( Just p, _ ) ->
            { model | kondisilaman = LamanSudahDimuat LamanKosong }
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

        ( DaftarPelangganTermuat (Err g), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanGagalMuat g) }
                => Cmd.none

        ( IkhtisarTermuat (Ok i), _) ->
            { model | kondisilaman = LamanSudahDimuat (LamanIkhtisar i) }
                => Cmd.none

        (IkhtisarTermuat (Err g), _) ->
            { model | kondisilaman = LamanSudahDimuat (LamanGagalMuat g) }
                => Cmd.none

        ( DaftarPelangganTermuat (Ok dp), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanDaftarPelanggan dp) }
                => Cmd.none

        ( DetailPelangganTermuat (Err g), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanGagalMuat g) }
                => Cmd.none

        ( DetailPelangganTermuat (Ok dp), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanRiwayatPelanggan dp) }
                => Cmd.none

        ( DetailTagihanTermuat (Err g), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanGagalMuat g) }
                => Cmd.none

        ( DetailTagihanTermuat (Ok dp), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanDetailTagihan dp) }
                => Cmd.none

        ( SetPengguna p, _ ) ->
            let
                cmd =
                    if sesi.pengguna /= Nothing && p == Nothing then
                        Rute.modifikasiUrl Rute.DaftarPelanggan
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
    case laman of
        LamanKosong ->
            Html.text "kosong"
                |> Bingkai.bingkai sesi.pengguna

        LamanGagalMuat gagal ->
            GagalMuat.view sesi gagal
                |> Bingkai.bingkai sesi.pengguna

        LamanTakKetemu ->
            Html.text "halaman tidak ketemu"
                |> Bingkai.bingkai sesi.pengguna

        LamanMasuk x ->
            Masuk.view sesi x
                |> Bingkai.bingkai sesi.pengguna
                |> Html.map MasukMsg

        LamanIkhtisar submodel ->
            Ikhtisar.view sesi submodel
                |> Bingkai.bingkai sesi.pengguna

        LamanDaftarPelanggan submodel ->
            DaftarPelanggan.view sesi submodel
                |> Bingkai.bingkai sesi.pengguna
                |> Html.map DaftarPelangganMsg

        LamanRiwayatPelanggan submodel ->
            RiwayatPelanggan.view sesi submodel
                |> Bingkai.bingkai sesi.pengguna
                |> Html.map RiwayatPelangganMsg

        LamanDetailTagihan submodel ->
            DetailTagihan.view sesi submodel
                |> Bingkai.bingkai sesi.pengguna
                |> Html.map DetailTagihanMsg


        LamanKeluar ->
            Html.text "keluar"
                |> Bingkai.bingkai sesi.pengguna


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
