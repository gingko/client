module Api exposing (exportDocx)

import Bytes exposing (Bytes)
import Http exposing (Error(..), Response(..))
import Json.Encode as Enc
import Result exposing (Result(..))


exportDocx : (Result Http.Error Bytes -> msg) -> { docId : String, markdown : String } -> Cmd msg
exportDocx msg { docId, markdown } =
    let
        body =
            Enc.object
                [ ( "markdown", Enc.string markdown )
                , ( "docId", Enc.string docId )
                ]
    in
    Http.post
        { url = "/export-docx"
        , body = Http.jsonBody body
        , expect = Http.expectBytesResponse msg (resolve Ok)
        }


resolve : (body -> Result String a) -> Http.Response body -> Result Http.Error a
resolve toResult response =
    case response of
        BadUrl_ url ->
            Err (BadUrl url)

        Timeout_ ->
            Err Timeout

        NetworkError_ ->
            Err NetworkError

        BadStatus_ metadata _ ->
            Err (BadStatus metadata.statusCode)

        GoodStatus_ _ body ->
            Result.mapError BadBody (toResult body)
