module Data.Ringkasan exposing (..)

import Data.Tagihan.Tarif as Tarif
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)

type alias Ringkasan =
    { jumlahpelanggan : Int
    , jumlahtagihan : Int
    , jumlahbayar : Int
    , tarif : Tarif.Tarif
    }

decoder : Decoder Ringkasan
decoder =
    decode Ringkasan
        |> required "jumlah_pelanggan" Decode.int
        |> required "jumlah_tagihan" Decode.int 
        |> required "tagihan_bayar" Decode.int
        |> required "tarif" Tarif.decoderTarif