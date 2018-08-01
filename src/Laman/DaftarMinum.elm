module Laman.DaftarMinum exposing (..)

import Data.MinumPelanggan as Minum
import Data.Sesi as Sesi
import Html exposing (..)
import Html.Attributes exposing (..)
import Http as Http
import Laman.GagalMuat as GagalMuat
import Request.LihatPelanggan as LihatPelanggan
import Task exposing (Task)
import Util exposing (..)


type alias Model =
    { galat : String
    , daftarminum : List Minum.StatusMinum
    }


type Msg
    = NoOp
    | DaftarMinumTerunduh (Result Http.Error (List Minum.StatusMinum))


init : Sesi.Sesi -> Task GagalMuat.LamanGagalDimuat Model
init sesi =
    let
        mtoken =
            Maybe.map .token sesi.pengguna

        daftarstatus =
            LihatPelanggan.getDaftarMinumPelanggan mtoken
                |> Http.toTask

        gagalpenangan =
            penangangalat
                >> GagalMuat.lamanGagalDimuat
    in
    Task.map (Model "") daftarstatus
        |> Task.mapError gagalpenangan


view : Sesi.Sesi -> Model -> Html Msg
view sesi model =
    div [ class "content" ]
        [ p [ class "is-danger" ] [ text model.galat ], viewtable model.daftarminum ]


viewtable : List Minum.StatusMinum -> Html Msg
viewtable dm =
    table [ class "table" ]
        [ thead [ class "thead" ]
            [ th [ class "th" ] [ text "Nama" ]
            , th [ class "th" ] [ text "Nomor Telepon" ]
            , th [ class "th" ] [ text "Nomor Meteran" ]
            , th [ class "th" ] [ text "Alamat" ]
            , th [ class "th" ] [ text "Sudah Dicatat?" ]
            ]
        , tbody [ class "tbody" ] <| List.map viewbaris dm
        ]


viewbaris : Minum.StatusMinum -> Html Msg
viewbaris m =
    tr
        [ classList
            [ "table" => True
            , "has-text-danger" => not m.sudahdicatat
            , "has-text-success" => m.sudahdicatat
            ]
        ]
        [ td [ class "td" ] [ text m.namapelanggan ]
        , td [ class "td" ] [ text m.nomortelepon ]
        , td [ class "td" ] [ text m.nomormeteran ]
        , td [ class "td" ] [ text m.alamat ]
        , td [ class "td" ] [ text <| sudahataubelum m.sudahdicatat ]
        ]


sudahataubelum : Bool -> String
sudahataubelum sudah =
    if sudah then
        "Sudah"
    else
        "Belum"


update : Sesi.Sesi -> Msg -> Model -> ( Model, Cmd Msg )
update sesi msg model =
    case msg of
        NoOp ->
            model
                => Cmd.none

        DaftarMinumTerunduh (Ok dm) ->
            { model | daftarminum = dm }
                => Cmd.none

        DaftarMinumTerunduh (Err g) ->
            let
                pesangalat =
                    penangangalat g
            in
            { model | galat = pesangalat }
                => Cmd.none
