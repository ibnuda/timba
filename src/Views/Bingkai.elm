module Views.Bingkai exposing (..)

import DaftarRute as Rute exposing (..)
import Data.Pengguna as Pengguna
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)


type LamanAktif
    = AktifBeranda
    | AktifIkhtisar
    | AktifRoot
    | AktifMasuk
    | AktifPelanggan
    | AktifTambahPelanggan
    | AktifTarif


bingkai : Maybe Pengguna.Pengguna -> LamanAktif -> Html msg -> Html msg
bingkai mpengguna laman konten =
    lihatIsi mpengguna laman konten


lihatIsi : Maybe Pengguna.Pengguna -> LamanAktif -> Html msg -> Html msg
lihatIsi mpengguna laman konten =
    case mpengguna of
        Nothing ->
            div [ class "container is-fluid" ]
                [ section [ class "section" ]
                    [ konten
                    ]
                ]

        Just pengguna ->
            div []
                [ navigasibar laman
                , div [ class "container is-fluid" ]
                    [ section [ class "section" ]
                        [ konten
                        ]
                    ]
                ]


navigasibar : LamanAktif -> Html msg
navigasibar laman =
    nav [ class "navbar is-blue", role "navigation", ariaLabel "main navigation" ]
        [ div [ class "navbar-brand" ]
            [ a [ Rute.href Rute.Beranda, class "navbar-item brand-text" ]
                [ text "Timba" ]
            , a [ role "button", class "navbar-burger is-active", ariaLabel "menu", ariaExpanded "true" ]
                [ span [ ariaHidden True ] []
                , span [ ariaHidden True ] []
                , span [ ariaHidden True ] []
                , span [ ariaHidden True ] []
                ]
            ]
        , div [ id "navMenu", class "navbar-menu is-active" ]
            [ div [ class "navbar-start" ]
                [ tautannavbar laman Rute.DaftarPelanggan (a [ Rute.href Rute.DaftarPelanggan ] [ text "Pelanggan" ])
                , tautannavbar laman Rute.TambahPelanggan (a [ Rute.href Rute.TambahPelanggan ] [ text "Tambah Pelanggan" ])
                , tautannavbar laman Rute.DaftarTarif (a [ Rute.href Rute.DaftarTarif ] [ text "Daftar Tarif" ])
                ]
            , div [ class "navbar-end" ]
                [ a [ Rute.href Rute.Keluar, class "navbar-item" ]
                    [ text "Ubah Informasi" ]
                , a [ Rute.href Rute.Keluar, class "navbar-item" ]
                    [ text "Keluar" ]
                ]
            ]
        ]


tautannavbar : LamanAktif -> Rute -> Html msg -> Html msg
tautannavbar laman rute konten =
    div [ classList [ ( "navbar-item", True ), ( "is-active", isactive laman rute ) ] ]
        [ konten ]


isactive : LamanAktif -> Rute -> Bool
isactive laman rute =
    case ( laman, rute ) of
        ( AktifBeranda, Rute.Beranda ) ->
            True

        ( AktifIkhtisar, Rute.Beranda ) ->
            True

        ( AktifPelanggan, Rute.DaftarPelanggan ) ->
            True

        ( AktifTambahPelanggan, Rute.TambahPelanggan ) ->
            True

        ( AktifTarif, Rute.DaftarTarif ) ->
            True

        ( _, _ ) ->
            False
