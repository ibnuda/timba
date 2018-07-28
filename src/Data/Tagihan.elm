module Data.Tagihan exposing (..)

import Data.Tagihan.Tarif as Tarif exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode, optional, required)


type alias TagihanPelanggan =
    { nomorPelanggan : Int
    , namaPelanggan : String
    , nomorTelepon : String
    , alamat : String
    , wilayah : String
    }


type alias Tagihan =
    { nomorTagihan : Int
    , nomorMeteran : String
    , tahun : Int
    , bulan : Int
    , pengguna : TagihanPelanggan
    , tarif : Tarif
    , minumLalu : Int
    , minumSekarang : Int
    , tanggalBayar : String
    }


decoderTagihanPelanggan : Decoder TagihanPelanggan
decoderTagihanPelanggan =
    decode TagihanPelanggan
        |> required "nomor_pelanggan" Decode.int
        |> required "nama_pelanggan" Decode.string
        |> required "nomor_telepon" Decode.string
        |> required "alamat" Decode.string
        |> required "wilayah" Decode.string


decoderTagihan : Decoder Tagihan
decoderTagihan =
    decode Tagihan
        |> required "nomor_tagihan" Decode.int
        |> required "nomor_meteran" Decode.string
        |> required "tahun" Decode.int
        |> required "bulan" Decode.int
        |> required "pengguna" decoderTagihanPelanggan
        |> required "tarif" Tarif.decoderTarif
        |> required "minum_lalu" Decode.int
        |> required "minum_sekarang" Decode.int
        |> optional "tanggal_bayar" Decode.string "Belum Dibayar"


decoderDaftarTagihan : Decoder (List Tagihan)
decoderDaftarTagihan =
    Decode.list decoderTagihan
