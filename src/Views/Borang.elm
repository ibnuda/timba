module Views.Borang exposing (..)

import Html exposing (Attribute, Html, fieldset, li, text, ul)
import Html.Attributes exposing (class, type_)


password : List (Attribute msg) -> List (Html msg) -> Html msg
password attrs =
    control Html.input ([ type_ "password" ] ++ attrs)


input : List (Attribute msg) -> List (Html msg) -> Html msg
input attrs =
    control Html.input ([ type_ "text" ] ++ attrs)


viewGalat : List String -> Html msg
viewGalat galat =
    galat
        |> List.map
            (\g ->
                li [] [ text g ]
            )
        |> ul [ class "error-messages" ]


control :
    (List (Attribute msg)
     -> List (Html msg)
     -> Html msg
    )
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
control elemen attr children =
    fieldset [ class "form-group" ]
        [ elemen (class "form-control" :: attr) children
        ]
