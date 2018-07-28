module Views.Bingkai exposing (..)

import DaftarRute as Rute exposing (..)
import Data.Pengguna as Pengguna
import Html exposing (..)
import Html.Attributes exposing (..)


bingkai : Maybe Pengguna.Pengguna -> Html msg -> Html msg
bingkai mpengguna konten =
    div [ class "content" ]
        [ viewMenu mpengguna konten
        ]


viewMenu : Maybe Pengguna.Pengguna -> Html msg -> Html msg
viewMenu mpengguna konten =
    case mpengguna of
        Nothing ->
            div [ class "pure-g" ]
                [ div [ class "content pure-u-1 pure-u-md-24-24" ]
                    [ konten
                    ]
                ]

        Just pengguna ->
            div [ class "pure-g" ]
                [ div [ class "pure-u-1 pure-u-md-3-24" ]
                    [ div [ id "menu" ]
                        [ div [ class "pure-menu" ]
                            [ p [ class "pure-menu-heading" ]
                                [ text pengguna.nama ]
                            , ul [ class "pure-menu-list" ]
                                [ li [ class "pure-menu-item" ]
                                    [ a
                                        [ Rute.href Rute.Beranda
                                        , class "pure-menu-link"
                                        ]
                                        [ text "Beranda" ]
                                    ]
                                , li [ class "pure-menu-item" ]
                                    [ a
                                        [ Rute.href Rute.Keluar
                                        , class "pure-menu-link"
                                        ]
                                        [ text "Keluar" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "content pure-u-1 pure-u-md-21-24" ]
                    [ konten
                    ]
                ]
