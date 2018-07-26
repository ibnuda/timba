module Request.TagihanPelanggan exposing (..)

import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.Tagihan as Tagihan exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Request.Bantuan exposing (apiUrl)


getSpesifik : Maybe AuthToken -> String -> Int -> Int -> Http.Request Tagihan
getSpesifik mtoken nomet tahun bulan =
    let
        expect =
            Tagihan.decoderTagihan
                |> Http.expectJson
    in
    apiUrl ("/tagihan/" ++ nomet ++ "/" ++ toString tahun ++ "/" ++ toString bulan)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> withAuthorization mtoken
        |> HttpBuilder.toRequest
