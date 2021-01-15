module Upgrade exposing (view)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import SharedUI exposing (modalWrapper)


view : { modalClosedMsg : msg, checkoutClickedMsg : msg } -> List (Html msg)
view { modalClosedMsg, checkoutClickedMsg } =
    [ div [ id "upgrade-copy" ] [ text "body copy here" ]
    , div [ id "upgrade-checkout" ]
        [ text "pricing and checkout here"
        , button [ onClick checkoutClickedMsg ] [ text "Pay Now" ]
        ]
    ]
        |> modalWrapper modalClosedMsg (Just "upgrade-modal") "Upgrade Gingko Writer"
