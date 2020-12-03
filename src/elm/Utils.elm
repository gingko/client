module Utils exposing (decodeDebug, getFieldErrors, hexEncode, randomPositiveInt)

import Hex
import Json.Decode as Dec exposing (Decoder)
import Random


hexEncode : String -> String
hexEncode input =
    input
        |> String.toList
        |> List.map Char.toCode
        |> List.map Hex.toString
        |> String.join ""


randomPositiveInt : Random.Generator Int
randomPositiveInt =
    Random.int 0 Random.maxInt


getFieldErrors : field -> List ( field, String ) -> List String
getFieldErrors field errs =
    errs
        |> List.filter ((==) field << Tuple.first)
        |> List.map Tuple.second


decodeDebug : String -> Decoder a -> Decoder a
decodeDebug message decoder =
    Dec.value
        |> Dec.andThen (debugHelper message decoder)


debugHelper : String -> Decoder a -> Dec.Value -> Decoder a
debugHelper message decoder value =
    {-
       let
           _ =
               Debug.log message (Dec.decodeValue decoder value)
       in
    -}
    decoder
