module Main exposing (main)

import DaftarRute as Rute
import Data.Pengguna as Pengguna
import Data.Sesi exposing (Sesi)
import Data.Tagihan.Tarif as Tarif
import Html exposing (..)
import Json.Decode as Decode
import Laman.DaftarPelanggan as DaftarPelanggan
import Laman.DaftarTarif as DaftarTarif
import Laman.DetailTagihan as DetailTagihan
import Laman.GagalMuat as GagalMuat
import Laman.Ganti as Ganti
import Laman.Ikhtisar as Ikhtisar
import Laman.Masuk as Masuk
import Laman.RiwayatPelanggan as RiwayatPelanggan
import Laman.TambahPelanggan as TambahPelanggan
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
    | LamanTambahPelanggan TambahPelanggan.Model
    | LamanDaftarTarif DaftarTarif.Model
    | LamanRiwayatPelanggan RiwayatPelanggan.Model
    | LamanDetailTagihan DetailTagihan.Model
    | LamanGantiInformasi Ganti.Model
    | LamanKeluar
    | LamanGagalMuat GagalMuat.LamanGagalDimuat


type KondisiLaman
    = LamanSudahDimuat ModelLamanTermuat
    | PindahDariLaman ModelLamanTermuat


type alias Model =
    { sesi : Sesi
    , kondisilaman : KondisiLaman
    , tariftersimpan : List Tarif.Tarif
    }


type Msg
    = SetRute (Maybe Rute.Rute)
    | SetPengguna (Maybe Pengguna.Pengguna)
    | MasukMsg Masuk.Msg
    | DaftarPelangganMsg DaftarPelanggan.Msg
    | TambahPelangganMsg TambahPelanggan.Msg
    | DaftarTarifMsg DaftarTarif.Msg
    | DetailTagihanMsg DetailTagihan.Msg
    | GantiInformasiMsg Ganti.Msg
    | RiwayatPelangganMsg RiwayatPelanggan.Msg
    | IkhtisarTermuat (Result GagalMuat.LamanGagalDimuat Ikhtisar.Model)
    | DaftarPelangganTermuat (Result GagalMuat.LamanGagalDimuat DaftarPelanggan.Model)
    | DaftarTarifTermuat (Result GagalMuat.LamanGagalDimuat DaftarTarif.Model)
    | DetailPelangganTermuat (Result GagalMuat.LamanGagalDimuat RiwayatPelanggan.Model)
    | DetailTagihanTermuat (Result GagalMuat.LamanGagalDimuat DetailTagihan.Model)
    | PasswordTerganti (Result GagalMuat.LamanGagalDimuat Pengguna.Pengguna)


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

        ( Just p, Just Rute.Beranda ) ->
            transisi IkhtisarTermuat (Ikhtisar.init model.sesi)

        ( Just p, Just Rute.DaftarPelanggan ) ->
            transisi DaftarPelangganTermuat (DaftarPelanggan.init model.sesi)

        ( Just p, Just Rute.TambahPelanggan ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanTambahPelanggan TambahPelanggan.initmodel) }
                => Cmd.none

        ( Just p, Just Rute.DaftarTarif ) ->
            transisi DaftarTarifTermuat (DaftarTarif.init model.sesi)

        ( Just p, Just (Rute.DetailPelanggan nomormeteran) ) ->
            transisi DetailPelangganTermuat
                (RiwayatPelanggan.init model.sesi nomormeteran)

        ( Just p, Just (Rute.DetailTagihan nomet tahun bulan) ) ->
            transisi DetailTagihanTermuat
                (DetailTagihan.init model.sesi nomet tahun bulan)

        ( Just p, Just Rute.GantiInformasi ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanGantiInformasi <| Ganti.init model.sesi) }
                => Cmd.none

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

        tersimpan =
            model.tariftersimpan
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

        ( TambahPelangganMsg submsg, LamanTambahPelanggan submod ) ->
            let
                ( modellaman, cmd ) =
                    TambahPelanggan.update sesi submsg submod
            in
            { model | kondisilaman = LamanSudahDimuat (LamanTambahPelanggan modellaman) }
                => Cmd.map TambahPelangganMsg cmd

        ( DaftarTarifMsg submsg, LamanDaftarTarif submod ) ->
            let
                ( ( modellaman, cmd ), msgdarilaman ) =
                    DaftarTarif.update sesi submsg submod

                modelbaru =
                    case msgdarilaman of
                        DaftarTarif.NoOp ->
                            model

                        DaftarTarif.SetTarifMsg x ->
                            { model | tariftersimpan = tersimpan ++ [ x ] }
            in
            { modelbaru | kondisilaman = LamanSudahDimuat (LamanDaftarTarif modellaman) }
                => Cmd.map DaftarTarifMsg cmd

        ( GantiInformasiMsg submsg, LamanGantiInformasi submod ) ->
            let
                ( ( modellaman, cmd ), msgdarilaman ) =
                    Ganti.update sesi submsg submod

                modelbaru =
                    case msgdarilaman of
                        Ganti.NoOp ->
                            model

                        Ganti.SetPenggunaMsg p ->
                            { model | sesi = { sesi | pengguna = Just p } }
            in
            { modelbaru | kondisilaman = LamanSudahDimuat (LamanGantiInformasi submod) }
                => Cmd.map GantiInformasiMsg cmd

        ( IkhtisarTermuat (Ok i), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanIkhtisar i) }
                => Cmd.none

        ( IkhtisarTermuat (Err g), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanGagalMuat g) }
                => Cmd.none

        ( DaftarPelangganTermuat (Ok dp), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanDaftarPelanggan dp) }
                => Cmd.none

        ( DaftarPelangganTermuat (Err g), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanGagalMuat g) }
                => Cmd.none

        ( DetailPelangganTermuat (Err g), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanGagalMuat g) }
                => Cmd.none

        ( DaftarTarifTermuat (Ok dt), _ ) ->
            let
                tarif =
                    DaftarTarif.modelketarif dt

                pengguna =
                    model.sesi.pengguna
            in
            { model
                | kondisilaman = LamanSudahDimuat (LamanDaftarTarif dt)
                , tariftersimpan = tersimpan ++ [ tarif ]
                , sesi = { sesi | pengguna = pengguna }
            }
                => Cmd.none

        ( DaftarTarifTermuat (Err g), _ ) ->
            { model | kondisilaman = LamanSudahDimuat (LamanGagalMuat g) }
                => Cmd.none

        ( PasswordTerganti (Ok p), _ ) ->
            { model | sesi = { sesi | pengguna = Just p } }
                => Cmd.none

        ( PasswordTerganti (Err g), _ ) ->
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
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifBeranda

        LamanGagalMuat gagal ->
            GagalMuat.view sesi gagal
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifBeranda

        LamanTakKetemu ->
            Html.text "halaman tidak ketemu"
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifBeranda

        LamanMasuk x ->
            Masuk.view sesi x
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifBeranda
                |> Html.map MasukMsg

        LamanIkhtisar submodel ->
            Ikhtisar.view sesi submodel
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifBeranda

        LamanDaftarPelanggan submodel ->
            DaftarPelanggan.view sesi submodel
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifPelanggan
                |> Html.map DaftarPelangganMsg

        LamanTambahPelanggan submodel ->
            TambahPelanggan.view sesi submodel
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifTambahPelanggan
                |> Html.map TambahPelangganMsg

        LamanDaftarTarif submodel ->
            DaftarTarif.view sesi submodel
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifTarif
                |> Html.map DaftarTarifMsg

        LamanRiwayatPelanggan submodel ->
            RiwayatPelanggan.view sesi submodel
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifRoot
                |> Html.map RiwayatPelangganMsg

        LamanDetailTagihan submodel ->
            DetailTagihan.view sesi submodel
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifRoot
                |> Html.map DetailTagihanMsg

        LamanGantiInformasi submodel ->
            Ganti.view sesi submodel
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifGanti
                |> Html.map GantiInformasiMsg

        LamanKeluar ->
            Html.text "keluar"
                |> Bingkai.bingkai sesi.pengguna Bingkai.AktifRoot


init : Decode.Value -> Location -> ( Model, Cmd Msg )
init v l =
    setRute (Rute.dariLokasi l)
        { kondisilaman = LamanSudahDimuat LamanKosong
        , sesi = { pengguna = decodePenggunaJson v }
        , tariftersimpan = []
        }


main : Program Decode.Value Model Msg
main =
    Navigation.programWithFlags (Rute.dariLokasi >> SetRute)
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
