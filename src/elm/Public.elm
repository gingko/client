module Public exposing (..)

import Url exposing (Url)


isPublic : Url -> Bool
isPublic url =
    url.host
        |> String.split "."
        |> List.head
        |> Maybe.map (\subdomain -> subdomain == "public")
        |> Maybe.withDefault False
        |> Debug.log "isPublic"
