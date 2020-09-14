module CachedData exposing (CachedData(..), expectJson, fromLocal, fromResult)

import Dict
import Http exposing (Expect, expectStringResponse)
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


fromLocal : Decoder a -> Dec.Value -> CachedData Http.Error a
fromLocal decoder json =
    let
        timestampDecoder =
            Dec.field "timestamp" (Dec.nullable Dec.int) |> Dec.map (Maybe.map Time.millisToPosix)

        decoderWithTimestamp =
            Dec.map2 Tuple.pair
                decoder
                timestampDecoder
    in
    case Dec.decodeValue decoderWithTimestamp json of
        Ok ( val, time_ ) ->
            SuccessLocal time_ val

        Err err ->
            Failure (Http.BadBody (Dec.errorToString err))


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
