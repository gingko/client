module Upgrade.UI exposing (view)

import Html exposing (div, text)
import Html.Attributes exposing (id)
import SharedUI exposing (modalWrapper)


view modalToggleMsg =
    [ div [ id "upgrade-copy" ] [ text "body copy here" ]
    , div [ id "upgrade-checkout" ] [ text "pricing and checkout here" ]
    ]
        |> modalWrapper modalToggleMsg (Just "upgrade-modal") "Upgrade Gingko Writer"
