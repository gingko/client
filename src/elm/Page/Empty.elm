module Page.Empty exposing (..)

import Ant.Icons.Svg as AntIcons
import Html exposing (Html, br, div, h1, img, p, text)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (on, onClick)
import Json.Decode as Dec



-- VIEW


view : { newClicked : msg, emptyMessage : msg } -> List (Html msg)
view msgs =
    [ div [ id "document-header" ] []
    , div [ id "loading-overlay" ] []
    , div [ id "empty-message" ]
        [ h1 [] [ text "You don't have any documents" ]
        , p [] [ text "Click to create one:" ]
        , br [] []
        , div [ id "new-button", onClick msgs.newClicked ] [ AntIcons.fileAddOutlined [] ]
        , img [ src "", on "error" (Dec.succeed msgs.emptyMessage) ] []
        ]
    ]
