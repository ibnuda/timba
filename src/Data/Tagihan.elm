module Data.Tagihan exposing (..)

import Data.Pelanggan as Pelanggan exposing (..)
import Data.Tagihan.Tarif as Tarif exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode, required, optional)


type alias Tagihan =
    { nomorTagihan : Int
    , nomorMeteran : String
    , tahun : Int
    , bulan : Int
    , pengguna : DataPelanggan
    , tarif : Tarif
    , minumLalu : Int
    , minumSekarang : Int
    , tanggalBayar : String
    }


decoderTagihan : Decoder Tagihan
decoderTagihan =
    decode Tagihan
        |> required "nomor_tagihan" Decode.int
        |> required "nomor_meteran" Decode.string
        |> required "tahun" Decode.int
        |> required "bulan" Decode.int
        |> required "pengguna" DataPelanggan.decoder
        |> required "tarif" Tarif.decoderTarif
        |> required "minum_lalu" Decode.int
        |> required "minum_sekarang" Decode.int
        |> optional "tanggal_bayar" Decode.string "Belum Dibayar"


decoderDaftarTagihan : Decoder (List Tagihan)
decoderDaftarTagihan =
    Decode.list decoderTagihan
