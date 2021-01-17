module SharedUI exposing (modalWrapper)

import Ant.Icons.Svg as Icons
import Html exposing (Html, a, button, div, h1, h2, text)
import Html.Attributes exposing (class, height, id, width)
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
            [ h2 [] [ text titleString ]
            , a [ class "close-button", onClick closeMsg ] [ Icons.closeCircleOutlined [ width 20, height 20 ] ]
            ]
        , div ([ class "modal-guts" ] ++ idAttr) body
        ]
    ]
