module Data.Pengguna exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))


type alias Pengguna =
    { nama : String
    , telp : String
    , idGrup : Int
    , token : AuthToken
    }


decoder : Decoder Pengguna
decoder =
    decode Pengguna
        |> required "nama" Decode.string
        |> required "telp" Decode.string
        |> required "id_grup" Decode.int
        |> required "token" AuthToken.decoder


encode : Pengguna -> Value
encode pengguna =
    Encode.object
        [ "nama" => Encode.string pengguna.nama
        , "telp" => Encode.string pengguna.telp
        , "id_grup" => Encode.int pengguna.idGrup
        , "token" => AuthToken.encode pengguna.token
        ]
