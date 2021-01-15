module SharedUI exposing (modalWrapper)

import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


modalWrapper : msg -> String -> List (Html msg) -> List (Html msg)
modalWrapper closeMsg titleString body =
    [ div [ class "modal-overlay" ] []
    , div [ class "modal" ] [ div [ class "modal-header" ] [ h1 [] [ text titleString ], button [ class "close-button", onClick closeMsg ] [ text "X" ] ], div [ class "modal-guts" ] body ]
    ]
