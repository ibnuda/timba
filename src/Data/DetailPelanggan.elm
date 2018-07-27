module Data.DetailPelanggan exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode, optional, required)


type alias TagihanSimple =
    { tahun : Int
    , bulan : Int
    , minumLalu : Int
    , minumSekarang : Int
    , tanggalBayar : String
    }


type alias DetailPelanggan =
    { namaPelanggan : String
    , nomorTelepon : String
    , alamat : String
    , wilayah : String
    , nomorMeteran : String
    , tanggalDaftar : String
    , penggunaanAir : List TagihanSimple
    }


tagihanSimpleDecoder : Decoder TagihanSimple
tagihanSimpleDecoder =
    decode TagihanSimple
        |> required "tahun" Decode.int
        |> required "bulan" Decode.int
        |> required "minum_lalu" Decode.int
        |> required "minum_sekarang" Decode.int
        |> optional "tanggal_bayar" Decode.string "Belum Dibayar"


decoder : Decoder DetailPelanggan
decoder =
    decode DetailPelanggan
        |> required "nama_pelanggan" Decode.string
        |> required "nomor_telepon" Decode.string
        |> required "alamat" Decode.string
        |> required "wilayah" Decode.string
        |> required "nomor_meteran" Decode.string
        |> required "tanggal_daftar" Decode.string
        |> required "penggunaan_air" (Decode.list tagihanSimpleDecoder)
