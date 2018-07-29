module Data.Tagihan.Tarif exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)



type alias Tarif =
    { biayabeban : Int
    , hargaawal : Int
    , sampaiawal : Int
    , hargatengah : Int
    , sampaitengah : Int
    , hargaakhir : Int
    }


decoderTarif : Decoder Tarif
decoderTarif =
    decode Tarif
        |> required "biaya_beban" Decode.int
        |> required "harga_awal" Decode.int
        |> required "sampai_awal" Decode.int
        |> required "harga_tengah" Decode.int
        |> required "sampai_tengah" Decode.int
        |> required "harga_akhir" Decode.int

