module Views.Bingkai exposing (..)

import DaftarRute as Rute exposing (..)
import Data.Pengguna as Pengguna
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)


bingkai : Maybe Pengguna.Pengguna -> Html msg -> Html msg
bingkai mpengguna konten =
    --  div [ class "container is-fluid" ]
    lihatIsi mpengguna konten

lihatIsi : Maybe Pengguna.Pengguna -> Html msg -> Html msg
lihatIsi mpengguna konten =
    case mpengguna of
        Nothing ->
            div [ class "container is-fluid" ]
                [ section [ class "section" ]
                    [ konten
                    ]
                ]

        Just pengguna ->
            div []
                [ navigasibar
                , div [ class "container is-fluid" ]
                    [ section [ class "section" ]
                        [ konten
                        ]
                    ]
                ]


navigasibar : Html msg
navigasibar =
    nav [ class "navbar is-blue", role "navigation" ]
        [ div [ class "navbar-brand" ]
            [ a [ Rute.href Rute.Beranda, class "navbar-item brand-text" ]
                [ text "Timba" ]
            , div [ class "navbar-burger" ]
                [ span [ ariaHidden True ] []
                , span [ ariaHidden True ] []
                , span [ ariaHidden True ] []
                ]
            ]
        , div [ id "navMenu", class "navbar-menu" ]
            [ div [ class "navbar-start" ]
                [ a [ Rute.href Rute.DaftarPelanggan, class "navbar-item" ]
                    [ text "Pelanggan" ]
                , a [ Rute.href Rute.DaftarTarif, class "navbar-item" ]
                    [ text "Tarif" ]
                ]
            , div [ class "navbar-end" ]
                [ a [ Rute.href Rute.Keluar, class "navbar-item" ]
                    [ text "Ubah Informasi" ]
                , a [ Rute.href Rute.Keluar, class "navbar-item" ]
                    [ text "Keluar" ]
                ]
            ]
        ]
