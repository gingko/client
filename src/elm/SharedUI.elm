module SharedUI exposing (modalWrapper)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


modalWrapper : msg -> List (Html msg) -> List (Html msg)
modalWrapper closeMsg body =
    [ div [ class "modal-overlay" ] []
    , div [ class "modal" ] [ button [ class "close-button", onClick closeMsg ] [ text "X" ], div [ class "modal-guts" ] body ]
    ]
