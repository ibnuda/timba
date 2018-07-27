module Laman.GagalMuat exposing (..)

import Data.Sesi as Sesi
import Html exposing (..)
import Html.Attributes exposing (..)


type LamanGagalDimuat
    = LamanGagalDimuat Model


type alias Model =
    { pesangalat : String
    }


lamanGagalDimuat : String -> LamanGagalDimuat
lamanGagalDimuat p =
    LamanGagalDimuat { pesangalat = p }


view : Sesi.Sesi -> LamanGagalDimuat -> Html msg
view sesi (LamanGagalDimuat m) =
    main_ [ tabindex -1 ]
        [ h1 [] [ text "Gagal Memuat Laman." ]
        , div []
            [ p [] [ text m.pesangalat ] ]
        ]
