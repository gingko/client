module Upgrade.UI exposing (view)

import Html exposing (text)
import SharedUI exposing (modalWrapper)


view modalToggleMsg =
    [ text "Upgrade modal" ]
        |> modalWrapper modalToggleMsg
