module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


type Msg
    = Inc


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Inc ->
            ( add1 model, Cmd.none )


add1 : Model -> Model
add1 model =
    model + 1


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [
        ]
