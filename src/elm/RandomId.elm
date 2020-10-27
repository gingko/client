module RandomId exposing (fromObjectId, generate, stringGenerator)

import Random


generate : (String -> msg) -> Cmd msg
generate msgTag =
    Random.generate msgTag stringGenerator


fromObjectId : String -> String
fromObjectId objId =
    objId
        |> String.slice 12 17
        |> String.toList
        |> List.map Char.toCode
        |> List.map (modBy 62)
        |> List.map intToValidChar
        |> String.fromList


stringGenerator : Random.Generator String
stringGenerator =
    Random.int 0 61
        |> Random.map intToValidChar
        |> Random.list 5
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
