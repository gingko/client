module Utils exposing (getFieldErrors, hexEncode)

import Hex


hexEncode : String -> String
hexEncode input =
    input
        |> String.toList
        |> List.map Char.toCode
        |> List.map Hex.toString
        |> String.join ""


getFieldErrors : field -> List ( field, String ) -> List String
getFieldErrors field errs =
    errs
        |> List.filter ((==) field << Tuple.first)
        |> List.map Tuple.second
