module Laman.TidakKetemu exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg
view =
    main_ [ class "container" ]
        [ h1 []
            [ text "Tidak Ketemu." ]
        ]
