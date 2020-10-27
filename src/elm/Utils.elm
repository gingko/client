module Utils exposing (getFieldErrors, hexEncode, randomPositiveInt)

import Hex
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
