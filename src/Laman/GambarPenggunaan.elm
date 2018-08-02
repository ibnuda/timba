module Laman.GambarPenggunaan exposing (..)

import Data.MinumPelanggan as Minum
import Data.Sesi as Sesi
import Html exposing (..)
import Http as Http
import Laman.GagalMuat as GagalMuat
import Request.LihatIkhtisar as LihatIkhtisar
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task exposing (Task)
import Util exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)


type alias Model =
    { galat : String
    , riwayatminum : List Minum.Minum
    }


init : Sesi.Sesi -> Task GagalMuat.LamanGagalDimuat Model
init sesi =
    let
        mtoken =
            Maybe.map .token sesi.pengguna

        dminum =
            LihatIkhtisar.getDaftarMinum mtoken |> Http.toTask

        gagalpenangan =
            penangangalat
                >> GagalMuat.lamanGagalDimuat
    in
    Task.map (Model "") dminum |> Task.mapError gagalpenangan


view : Model -> Html msg
view model =
    div [ class "content" ]
        [ h1 [ class "title" ] [ Html.text "Grafik Penggunaan Air" ]
        , svg
            [ width (toString lebar ++ "px")
            , height (toString tinggi ++ "px")
            ]
            [ Svg.style []
                [ Svg.text """
                .kolom rect { fill: rgba(118, 214, 78, 0.8); }
                .kolom text { display: none; }
                .kolom:hover rect { fill: rgb(118, 214, 78); }
                .kolom:hover text { display: inline; }
                """
                ]
            , g [ transform ("translate(" ++ toString (lapis - 1) ++ ", " ++ toString (tinggi - lapis) ++ ")") ]
                [ sumbux model.riwayatminum ]
            , g [ transform ("translate(" ++ toString (lapis - 1) ++ ", " ++ toString lapis ++ ")") ]
                [ sumbuy ]
            , g [ transform ("translate(" ++ toString lapis ++ ", " ++ toString lapis ++ ")"), class "seri" ]
                <| List.map (kolom (skalax model.riwayatminum)) model.riwayatminum
            ]
        ]


lebar : Float
lebar =
    900


tinggi : Float
tinggi =
    450


lapis : Float
lapis =
    30


skalax : List Minum.Minum -> BandScale ( Int, Int )
skalax minum =
    Scale.band
        { defaultBandConfig
            | paddingInner = 0.1
            , paddingOuter = 0.2
        }
        (List.map (\m -> ( m.tahun, m.bulan )) minum)
        ( 0, lebar - 2 * lapis )


skalay : ContinuousScale
skalay =
    Scale.linear ( 0, 1000 ) ( tinggi - 2 * lapis, 0 )


sumbux : List Minum.Minum -> Svg msg
sumbux minum =
    Axis.axis
        { defaultOptions
            | orientation = Axis.Bottom
            , tickFormat = Just (\( a, b ) -> toString a ++ "/" ++ toString b)
        }
        (Scale.toRenderable (skalax minum))


sumbuy : Svg msg
sumbuy =
    Axis.axis
        { defaultOptions
            | orientation = Axis.Left
            , tickCount = 5
        }
        skalay


kolom : BandScale ( Int, Int ) -> Minum.Minum -> Svg msg
kolom skalax minum =
    g [ class "kolom" ]
        [ rect
            [ x <| toString <| Scale.convert skalax ( minum.tahun, minum.bulan )
            , y <| toString <| Scale.convert skalay <| toFloat minum.minum
            , width <| toString <| Scale.bandwidth skalax
            , height <| toString <| tinggi - Scale.convert skalay (toFloat minum.minum) - 2 * lapis
            ]
            []
        , text_
            [ x <| toString <| Scale.convert (Scale.toRenderable skalax) ( minum.tahun, minum.bulan )
            , y <| toString <| Scale.convert skalay (toFloat minum.minum) - 5
            , textAnchor "minum"
            ]
            [ Html.text <| toString minum.minum ]
        ]
