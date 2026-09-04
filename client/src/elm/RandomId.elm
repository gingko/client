module RandomId exposing (fromObjectId, generate, stringGenerator)

import Hex
import Random


generate : (String -> msg) -> Cmd msg
generate msgTag =
    Random.generate msgTag (stringGenerator 7)


fromObjectId : Int -> String -> String
fromObjectId seed objId =
    case Hex.fromString (String.slice 17 24 objId) of
        Ok val ->
            Random.step (stringGenerator 7) (Random.initialSeed (seed + val))
                |> Tuple.first

        Err _ ->
            objId
                |> String.slice 17 24
                |> String.toList
                |> List.map Char.toCode
                |> List.map (modBy 62)
                |> List.map intToValidChar
                |> String.fromList


stringGenerator : Int -> Random.Generator String
stringGenerator numberOfChars =
    Random.int 0 61
        |> Random.map intToValidChar
        |> Random.list numberOfChars
        |> Random.map String.fromList



-- INTERNAL


intToValidChar : Int -> Char
intToValidChar int =
    if int <= 9 then
        -- 0 to 9
        int + 48 |> Char.fromCode

    else if int < 10 + 26 then
        -- A to Z
        int - 10 + 65 |> Char.fromCode

    else
        -- a to z
        int - 10 - 26 + 97 |> Char.fromCode
