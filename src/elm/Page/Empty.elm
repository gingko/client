module Page.Empty exposing (..)

import Ant.Icons.Svg as AntIcons
import Html exposing (Html, br, div, h1, p, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)



-- VIEW


view : msg -> List (Html msg)
view newClicked =
    [ div [ id "document-header" ] []
    , div [ id "loading-overlay" ] []
    , div [ id "empty-message" ]
        [ h1 [] [ text "You don't have any documents" ]
        , p [] [ text "Click to create one:" ]
        , br [] []
        , div [ id "new-button", onClick newClicked ] [ AntIcons.fileAddOutlined [] ]
        ]
    ]
