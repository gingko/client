module Page.Doc.ContextMenu exposing (open)

import Html exposing (Attribute)
import Html.Events
import Json.Decode as Dec
import Tuple


open : (( Float, Float ) -> msg) -> Attribute msg
open openMsg =
    let
        position =
            Dec.map2 Tuple.pair
                (Dec.field "clientX" Dec.float)
                (Dec.field "clientY" Dec.float)
    in
    Html.Events.custom "contextmenu"
        (position
            |> Dec.map openMsg
            |> Dec.map
                (\msg ->
                    { message = msg
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
        )
