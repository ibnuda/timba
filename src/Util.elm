module Util exposing (..)

import Html exposing (Attribute, Html)
import Html.Events exposing (defaultOptions, onWithOptions)
import Http as Http
import Json.Decode as Decode


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
infixl 0 =>


pair : a -> b -> ( a, b )
pair a b =
    a => b


(!!) : List a -> Int -> Maybe a
(!!) xs n =
    List.head (List.drop n xs)
infixl 9 !!


paksa : String -> Int
paksa x =
    case String.toInt x of
        Err _ ->
            0

        Ok a ->
            a


onClickStopPropagation : a -> Attribute a
onClickStopPropagation msg =
    onWithOptions "click" { defaultOptions | stopPropagation = True } (Decode.succeed msg)


appendErrors : { model | errors : List error } -> List error -> { model | errors : List error }
appendErrors model errs =
    { model | errors = model.errors ++ errs }


penangangalat : Http.Error -> String
penangangalat x =
    case x of
        Http.BadUrl u ->
            "url kaco."

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error."

        Http.BadStatus r ->
            r.body

        Http.BadPayload s rs ->
            rs.body
