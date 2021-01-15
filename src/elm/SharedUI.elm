module SharedUI exposing (modalWrapper)

import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)


modalWrapper : msg -> Maybe String -> String -> List (Html msg) -> List (Html msg)
modalWrapper closeMsg id_ titleString body =
    let
        idAttr =
            case id_ of
                Just bodyId ->
                    [ id bodyId ]

                Nothing ->
                    []
    in
    [ div [ class "modal-overlay" ] []
    , div [ class "modal" ]
        [ div [ class "modal-header" ]
            [ h1 [] [ text titleString ]
            , button [ class "close-button", onClick closeMsg ] [ text "X" ]
            ]
        , div ([ class "modal-guts" ] ++ idAttr) body
        ]
    ]
