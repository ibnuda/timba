module Data.Tagihan.Tarif exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias TarifItem =
    { mulai : Int
    , sampai : Int
    , harga : Int
    }


type alias Tarif =
    { biayaBeban : Int
    , satuan : List TarifItem
    }


decoderTarifItem : Decoder TarifItem
decoderTarifItem =
    decode TarifItem
        |> required "mulai" Decode.int
        |> optional "sampai" Decode.int 0
        |> required "harga" Decode.int


decoderTarif : Decoder Tarif
decoderTarif =
    decode Tarif
        |> required "biaya_beban" Decode.int
        |> required "satuan" (Decode.list decoderTarifItem)
