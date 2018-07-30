module Request.LihatIkhtisar exposing (..)

import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.Ringkasan as Ringkasan
import Data.Tagihan.Tarif as Tarif
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode
import Request.Bantuan exposing (apiUrl)
import Util exposing (..)


getIkhtisar : Maybe AuthToken -> Http.Request Ringkasan.Ringkasan
getIkhtisar mtoken =
    let
        expect =
            Ringkasan.decoder
                |> Http.expectJson
    in
    apiUrl "/ikhtisar"
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> withAuthorization mtoken
        |> HttpBuilder.toRequest


getDaftarTarif : Maybe AuthToken -> Http.Request (List Tarif.Tarif)
getDaftarTarif mtoken =
    let
        expect =
            Decode.list Tarif.decoderTarif |> Http.expectJson
    in
    apiUrl "/tarif"
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> withAuthorization mtoken
        |> HttpBuilder.toRequest


postTarif : Maybe AuthToken -> Int -> Int -> Int -> Int -> Int -> Int -> Http.Request Tarif.Tarif
postTarif mtoken a b c d e f =
    let
        tarif =
            Encode.object
                [ "biaya_beban" => Encode.int a
                , "awal_harga" => Encode.int b
                , "awal_sampai" => Encode.int c
                , "tengah_harga" => Encode.int d
                , "tengah_sampai" => Encode.int e
                , "akhir_harga" => Encode.int f
                ]

        expect =
            Tarif.decoderTarif |> Http.expectJson
    in
    apiUrl "/tarif"
        |> HttpBuilder.post
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withJsonBody tarif
        |> withAuthorization mtoken
        |> HttpBuilder.toRequest
