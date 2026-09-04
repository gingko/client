module Page.DocMessage exposing (..)

import Ant.Icons.Svg as AntIcons
import Html exposing (Html, br, div, h1, img, p, text)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (on, onClick)
import Json.Decode as Dec



-- VIEW


viewEmpty : { newClicked : msg, emptyMessage : msg } -> List (Html msg)
viewEmpty msgs =
    [ div [ id "document-header" ] []
    , div [ id "empty-message" ]
        [ h1 [] [ text "You don't have any documents" ]
        , p [] [ text "Click to create one:" ]
        , br [] []
        , div [ id "new-button", onClick msgs.newClicked ] [ AntIcons.fileAddOutlined [] ]
        , img [ src "", on "error" (Dec.succeed msgs.emptyMessage) ] []
        ]
    ]


viewNotFound : msg -> List (Html msg)
viewNotFound contactSupport =
    [ div [ id "document-header" ] []
    , div [ id "doc-error-message" ]
        [ h1 [] [ text "Hmm, we couldn't find this document" ]
        , p [] [ text "The file might have been moved, or deleted." ]
        , br [] []
        , p [] [ text "Is it still in your list of documents?" ]
        , p [] [ text "If so, let us know and we'll help you fix it!" ]
        , br [] []
        , div [ class "modal-buttons" ]
            [ div [ id "email-support", onClick contactSupport ] [ text "Contact Support" ]
            ]
        ]
    ]
