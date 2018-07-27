module Request.LihatPelanggan exposing (..)

import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.DetailPelanggan as DetailPelanggan exposing (..)
import Data.Pelanggan as Pelanggan exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Request.Bantuan exposing (apiUrl)


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
