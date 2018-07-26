module Request.KeluarMasuk exposing (..)

import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.Pengguna as Pengguna exposing (Pengguna)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Encode as Encode
import Ports
import Request.Bantuan exposing (..)
import Util exposing ((=>))


simpanSesi : Pengguna -> Cmd msg
simpanSesi pengguna =
    Pengguna.encode pengguna |> Encode.encode 0 |> Just |> Ports.storeSession


masuk : { r | telepon : String, sandi : String } -> Http.Request Pengguna
masuk { telepon, sandi } =
    let
        pengguna =
            Encode.object
                [ "nomor_telepon" => Encode.string telepon
                , "password" => Encode.string sandi
                ]
                |> Http.jsonBody
    in
    Pengguna.decoder
        |> Http.post (apiUrl "/masuk") pengguna


gantiPassword : { a | passbaru : String, passlama : String } -> Maybe AuthToken -> Http.Request Pengguna
gantiPassword { passlama, passbaru } mtoken =
    let
        gantipass =
            Encode.object
                [ "pass_lama" => Encode.string passlama
                , "pass_baru" => Encode.string passbaru
                ]
                |> Http.jsonBody

        ekspektasi =
            Pengguna.decoder |> Http.expectJson
    in
    apiUrl "/gantipass"
        |> HttpBuilder.put
        |> HttpBuilder.withExpect ekspektasi
        |> HttpBuilder.withBody gantipass
        |> withAuthorization mtoken
        |> HttpBuilder.toRequest
