module Request.LihatIkhtisar exposing (..)

import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.Ringkasan as Ringkasan
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Request.Bantuan exposing (apiUrl)


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
