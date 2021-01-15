module Upgrade.UI exposing (view)

import Html exposing (div, h1, text)
import Html.Attributes exposing (id)
import SharedUI exposing (modalWrapper)


view modalToggleMsg =
    [ h1 [] [ text "Upgrade Gingko Writer" ]
    , div [ id "upgrade-copy" ] [ text "body copy here" ]
    , div [ id "upgrade-checkout" ] [ text "pricing and checkout here" ]
    ]
        |> modalWrapper modalToggleMsg
