module Laman.Ganti exposing (..)

import Data.Sesi as Sesi
import Html exposing (..)
import Html.Attributes exposing (..)
import Views.Borang as Borang


type alias Model =
    { galat : String
    , nama : String
    , passwordlama : String
    , passwordbaru : String
    }


init : Sesi.Sesi -> Model
init sesi =
    { galat = ""
    , nama = ""
    , passwordlama = ""
    , passwordbaru = ""
    }


view : Sesi.Sesi -> Model -> Html msg
view sesi model =
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "column" ] []
            , div [ class "column" ]
                [ header [ class "title" ]
                    [ text "Ganti Password (belum ditulis)" ]
                , Html.form []
                    [ Borang.input
                        [ class "input"
                        , placeholder "Nama"
                        ]
                        [ text model.nama ]
                    , Borang.password
                        [ class "input"
                        , placeholder "Password Lama"
                        ]
                        []
                    , Borang.password
                        [ class "input"
                        , placeholder "Password Baru"
                        ]
                        []
                    , button [ class "button is-pulled-right is-primary" ]
                        [ text "Ubah" ]
                    ]
                ]
            , div [ class "column" ] []
            ]
        ]
