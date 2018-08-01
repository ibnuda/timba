module Request.TagihanPelanggan exposing (..)

import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.Tagihan as Tagihan exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Json.Encode as Encode
import Request.Bantuan exposing (apiUrl)
import Util exposing (..)


getSpesifik : Maybe AuthToken -> String -> Int -> Int -> Http.Request Tagihan
getSpesifik mtoken nomet tahun bulan =
    let
        ekspektasi =
            Tagihan.decoderTagihan
                |> Http.expectJson
    in
    apiUrl ("/tagihan/" ++ nomet ++ "/" ++ toString tahun ++ "/" ++ toString bulan)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect ekspektasi
        |> withAuthorization mtoken
        |> HttpBuilder.toRequest


putBayarTagihan : Maybe AuthToken -> String -> Int -> Int -> Http.Request Tagihan
putBayarTagihan mtoken nomet tahun bulan =
    let
        ekspektasi =
            Tagihan.decoderTagihan
                |> Http.expectJson

        isiisian =
            Encode.object
                [ "admin" => Encode.string "to be decided."
                ]
                |> Http.jsonBody
    in
    apiUrl ("/tagihan/" ++ nomet ++ "/" ++ toString tahun ++ "/" ++ toString bulan)
        |> HttpBuilder.put
        |> HttpBuilder.withExpect ekspektasi
        |> HttpBuilder.withBody isiisian
        |> withAuthorization mtoken
        |> HttpBuilder.toRequest
