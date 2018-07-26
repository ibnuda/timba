module Data.Pelanggan exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode, optional, required)


type alias Pelanggan =
    { namaPelanggan : String
    , nomorTelepon : String
    , nomorMeteran : String
    , alamat : String
    , wilayah : String
    }


decoder : Decoder Pelanggan
decoder =
    decode Pelanggan
        |> required "nama_pelanggan" Decode.string
        |> required "nomor_telepon" Decode.string
        |> required "nomor_meteran" Decode.string
        |> required "alamat" Decode.string
        |> required "wilayah" Decode.string


listDecoder : Decoder (List Pelanggan)
listDecoder =
    Decode.list decoder
