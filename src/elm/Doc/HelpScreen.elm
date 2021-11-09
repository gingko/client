module Doc.HelpScreen exposing (view)

import Ant.Icons.Svg as Icons
import Html exposing (Html, a, button, div, h2, h3, kbd, li, span, table, text, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, height, href, style, target, width)
import Html.Events exposing (onClick)
import SharedUI exposing (modalWrapper)


view : { closeModal : msg, showVideoTutorials : msg, contactSupport : msg } -> List (Html msg)
view msg =
    [ div [ class "modal-overlay", onClick msg.closeModal ] []
    , div [ class "max-width-grid" ]
        [ div [ class "modal", class "help-modal" ]
            [ div [ class "modal-header" ]
                [ h2 [] [ text "Help" ]
                , div [ class "close-button", onClick msg.closeModal ] [ Icons.closeCircleOutlined [ width 20, height 20 ] ]
                ]
            , div [ class "modal-guts" ]
                [ h3 [] [ text "Keyboard Shortcuts" ]
                , div [ style "display" "flex", style "gap" "16px", style "justify-content" "space-between" ]
                    [ div [ class "shortcut-table" ]
                        ([ div [ class "shortcut-table-title" ] [ text "Normal Mode" ]
                         ]
                            ++ normalShortcuts
                        )
                    , div [ class "shortcut-table" ]
                        ([ div [ class "shortcut-table-title" ] [ text "Editing Mode" ]
                         ]
                            ++ editingShortcuts
                        )
                    ]
                ]
            , div [ class "modal-buttons" ]
                [ button [ onClick msg.showVideoTutorials ] [ text "Video Tutorials" ]
                , a [ href "https://docs.gingkowriter.com", target "_blank" ] [ text "FAQ" ]
                , button [ onClick msg.contactSupport ] [ text "Contact Support" ]
                ]
            ]
        ]
    ]


normalShortcuts : List (Html msg)
normalShortcuts =
    [ shortcutRow "Edit Card" [ key "Enter" ] ]


editingShortcuts : List (Html msg)
editingShortcuts =
    [ shortcutRow "Save Changes" [ key "Ctrl", key "S" ] ]


shortcutRow : String -> List (Html msg) -> Html msg
shortcutRow desc keys =
    div [ class "shortcut-row" ] [ div [] [ text desc ], div [] keys ]


key : String -> Html msg
key str =
    span [ class "shortcut-key", style "margin" "0" ] [ text str ]
