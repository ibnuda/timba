module Request.LihatPelanggan exposing (..)

import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.DetailPelanggan as DetailPelanggan exposing (..)
import Data.Pelanggan as Pelanggan exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Json.Encode as Encode
import Request.Bantuan exposing (apiUrl)
import Util exposing ((=>))


getDaftarPelanggan : Maybe AuthToken -> Http.Request (List Pelanggan)
getDaftarPelanggan mtoken =
    let
        ekspektasi =
            Pelanggan.listDecoder
                |> Http.expectJson
    in
    apiUrl "/pelanggan"
        |> HttpBuilder.get
        |> HttpBuilder.withExpect ekspektasi
        |> withAuthorization mtoken
        |> HttpBuilder.toRequest


getDetailPelanggan : Maybe AuthToken -> String -> Http.Request DetailPelanggan
getDetailPelanggan mtoken nomormeteran =
    let
        ekspektasi =
            DetailPelanggan.decoder
                |> Http.expectJson
    in
    apiUrl ("/pelanggan/" ++ nomormeteran)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect ekspektasi
        |> withAuthorization mtoken
        |> HttpBuilder.toRequest


postTambahPelanggan : Maybe AuthToken -> { a | alamat : String, namapelanggan : String, nomormeteran : String, nomortelepon : String, password : String, wilayah : String } -> Http.Request Pelanggan
postTambahPelanggan mtoken { namapelanggan, nomortelepon, password, alamat, wilayah, nomormeteran } =
    let
        pelanggan =
            Encode.object
                [ "nama" => Encode.string namapelanggan
                , "nomor_telepon" => Encode.string nomortelepon
                , "password" => Encode.string password
                , "alamat" => Encode.string alamat
                , "wilayah" => Encode.string wilayah
                , "nomor_meteran" => Encode.string nomormeteran
                ]

        ekspektasi =
            Pelanggan.decoder |> Http.expectJson
    in
    apiUrl "/tambah"
        |> HttpBuilder.post
        |> HttpBuilder.withExpect ekspektasi
        |> HttpBuilder.withJsonBody pelanggan
        |> withAuthorization mtoken
        |> HttpBuilder.toRequest
