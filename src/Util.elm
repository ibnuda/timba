module Util exposing (..)

import Html exposing (Attribute, Html)
import Html.Events exposing (defaultOptions, onWithOptions)
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
