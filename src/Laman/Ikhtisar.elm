module Laman.Ikhtisar exposing (..)

import Data.Ringkasan as Ringkasan
import Data.Sesi as Sesi
import Data.Tagihan.Tarif as Tarif
import Html exposing (..)
import Html.Attributes exposing (..)
import Http as Http
import Laman.GagalMuat as GagalMuat
import Request.LihatIkhtisar as LihatIkhtisar
import Task exposing (Task)
import Util exposing (..)


type alias Model =
    { galat : String
    , ringkasan : Ringkasan.Ringkasan
    }


init : Sesi.Sesi -> Task GagalMuat.LamanGagalDimuat Model
init sesi =
    let
        mtoken =
            Maybe.map .token sesi.pengguna

        detailikhtisar =
            LihatIkhtisar.getIkhtisar mtoken
                |> Http.toTask

        gagalpenangan =
            penangangalat >> GagalMuat.lamanGagalDimuat
    in
    Task.map (Model "") detailikhtisar
        |> Task.mapError gagalpenangan


view : Sesi.Sesi -> Model -> Html msg
view sesi model =
    div [ class "content" ]
        [ div [ class "columns" ]
            [ viewkartusepertiga "Jumlah Pelanggan" <| toString model.ringkasan.jumlahpelanggan
            , viewkartusepertiga "Tagihan Tercatat" <| toString model.ringkasan.jumlahtagihan
            , viewkartusepertiga "Tagihan Terbayar" <| toString model.ringkasan.jumlahbayar
            ]
        , viewtarif model.ringkasan.tarif
        ]


viewkartusepertiga : String -> String -> Html msg
viewkartusepertiga judul isi =
    div [ class "column is-third" ]
        [ div [ class "card" ]
            [ header [ class "card-header" ]
                [ p [ class "card-header-title" ]
                    [ text judul ]
                ]
            , div [ class "card-content" ]
                [ div [ class "content" ]
                    [ text isi ]
                ]
            ]
        ]


viewtarif : Tarif.Tarif -> Html msg
viewtarif tarif =
    div []
        [ p [ class "subtitle" ]
            [ text <| "Biaya beban: " ++ toString tarif.biayabeban ]
        , table [ class "table" ]
            [ thead [ class "thead" ]
                [ th [ class "th" ] [ text "Mulai" ]
                , th [ class "th" ] [ text "Sampai" ]
                , th [ class "th" ] [ text "Harga" ]
                ]
            , tr [ class "tr" ]
                [ td [ class "td" ] [ text "0 M3" ]
                , td [ class "td" ] [ text <| toString tarif.sampaiawal ++ " M3" ]
                , td [ class "td" ] [ text <| "Rp. " ++ toString tarif.hargaawal ]
                ]
            , tr [ class "tr" ]
                [ td [ class "td" ] [ text <| toString tarif.sampaiawal ++ " M3" ]
                , td [ class "td" ] [ text <| toString tarif.sampaitengah ++ " M3" ]
                , td [ class "td" ] [ text <| "Rp. " ++ toString tarif.hargatengah ]
                ]
            , tr [ class "tr" ]
                [ td [ class "td" ] [ text <| toString tarif.sampaitengah ++ " M3" ]
                , td [ class "td" ] [ text "-" ]
                , td [ class "td" ] [ text <| "Rp. " ++ toString tarif.hargaakhir ]
                ]
            ]
        ]
