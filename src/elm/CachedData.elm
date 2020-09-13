module CachedData exposing (CachedData(..), expectJson, fromResult)

import Dict
import Http exposing (Expect, expectStringResponse)
import Iso8601
import Json.Decode as Dec exposing (Decoder)
import Time


type CachedData e a
    = NotAsked
    | Loading
    | Failure e
    | SuccessLocal (Maybe Time.Posix) a
    | Success (Maybe Time.Posix) a


fromResult : Result e ( a, Maybe Time.Posix ) -> CachedData e a
fromResult result =
    case result of
        Err e ->
            Failure e

        Ok ( x, timestamp_ ) ->
            Success timestamp_ x


expectJson : (CachedData Http.Error a -> msg) -> Decoder a -> Expect msg
expectJson toMsg decoder =
    expectStringResponse (fromResult >> toMsg) <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case Dec.decodeString decoder body of
                        Ok value ->
                            let
                                timestamp_ : Maybe Time.Posix
                                timestamp_ =
                                    metadata.headers
                                        |> Dict.get "x-timestamp"
                                        |> Maybe.andThen String.toInt
                                        |> Maybe.map Time.millisToPosix
                            in
                            Ok ( value, timestamp_ )

                        Err err ->
                            Err (Http.BadBody (Dec.errorToString err))
