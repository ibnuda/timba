module Data.AuthToken exposing (..)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type AuthToken
    = AuthToken String


encode : AuthToken -> Value
encode (AuthToken token) =
    Encode.string token


decoder : Decoder AuthToken
decoder =
    Decode.string
        |> Decode.map AuthToken


withAuthorization : Maybe AuthToken -> RequestBuilder a -> RequestBuilder a
withAuthorization mtoken builder =
    case mtoken of
        Nothing ->
            builder

        Just (AuthToken t) ->
            builder |> withHeader "authorization" ("Bearer " ++ t)
