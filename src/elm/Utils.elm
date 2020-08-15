module Utils exposing (hexEncode)

import Hex


hexEncode : String -> String
hexEncode input =
    input
        |> String.toList
        |> List.map Char.toCode
        |> List.map Hex.toString
        |> String.join ""
