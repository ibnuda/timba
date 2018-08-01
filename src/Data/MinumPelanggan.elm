module Data.MinumPelanggan exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)


type alias StatusMinum =
    { namapelanggan : String
    , nomortelepon : String
    , nomormeteran : String
    , alamat : String
    , sudahdicatat : Bool
    }


decoder : Decode.Decoder StatusMinum
decoder =
    decode StatusMinum
        |> required "nama_pelanggan" Decode.string
        |> required "nomor_telepon" Decode.string
        |> required "nomor_meteran" Decode.string
        |> required "alamat" Decode.string
        |> required "sudah_catat" Decode.bool
