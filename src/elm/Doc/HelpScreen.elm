module Doc.HelpScreen exposing (view)

import Ant.Icons.Svg as Icons
import Html exposing (Html, a, button, div, h2, h3, h4, kbd, li, span, table, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, height, href, id, style, target, width)
import Html.Events exposing (onClick)
import SharedUI exposing (ctrlOrCmdText)


view : Bool -> { closeModal : msg, showVideoTutorials : msg, contactSupport : msg } -> List (Html msg)
view isMac msg =
    let
        ctrlOrCmd =
            ctrlOrCmdText isMac
    in
    [ div [ class "modal-overlay", onClick msg.closeModal ] []
    , div [ class "max-width-grid" ]
        [ div [ class "modal", class "help-modal" ]
            [ div [ class "modal-header" ]
                [ h2 [] [ text "Help" ]
                , div [ class "close-button", onClick msg.closeModal ] [ Icons.closeCircleOutlined [ width 20, height 20 ] ]
                ]
            , div [ class "modal-guts" ]
                [ h2 [ id "shortcut-main-title" ] [ text "Keyboard Shortcuts" ]
                , div [ id "shortcut-modes-wrapper" ]
                    [ div []
                        [ h3 [ id "view-mode-shortcuts-title" ] [ text "View Mode Shortcuts :" ]
                        , shortcutTable "Card Edit, Create, Delete" (normalEditShortcuts ctrlOrCmd)
                        , shortcutTable "Navigation, Moving Cards" (normalNavigationShortcuts ctrlOrCmd)
                        , shortcutTable "Copy/Paste" (normalCopyShortcuts ctrlOrCmd)
                        , shortcutTable "Searching, Merging Cards" (normalAdvancedShortcuts ctrlOrCmd)
                        , shortcutTable "Help, Info, Documents" (normalOtherShortcuts ctrlOrCmd)
                        ]
                    , div [ id "mode-divider" ] []
                    , div []
                        [ h3 [ id "edit-mode-shortcuts-title" ] [ text "Edit Mode Shortcuts :" ]
                        , shortcutTable "Card Save, Create" (editSaveShortcuts ctrlOrCmd)
                        , shortcutTable "Formatting" (editFormatShortcuts ctrlOrCmd)
                        ]
                    ]
                ]
            , div [ class "modal-buttons" ]
                [ div [ onClick msg.showVideoTutorials ] [ text "Help Videos" ]
                , a [ href "https://docs.gingkowriter.com", target "_blank" ]
                    [ text "FAQ"
                    , Icons.linkOutlined [ width 16, height 16, style "margin-left" "4px", style "margin-top" "2px", style "fill" "hsl(86deg 54% 25%)" ]
                    ]
                , div [ id "email-support", onClick msg.contactSupport ] [ text "Contact Support" ]
                ]
            ]
        ]
    ]


shortcutTable : String -> List (Html msg) -> Html msg
shortcutTable tableTitle tableRows =
    div [ class "shortcut-table-wrapper" ]
        [ h4 [ class "shortcut-table-title" ] [ text tableTitle ]
        , table [ class "shortcut-table" ] tableRows
        ]


normalEditShortcuts : String -> List (Html msg)
normalEditShortcuts ctrlOrCmd =
    [ shortcutRow "Edit card" [ key "Enter" ]
    , shortcutRow "Edit card in fullscreen mode" [ key "Shift", key "Enter" ]
    , shortcutRow "Add card below" [ key ctrlOrCmd, key "↓", text " or ", key ctrlOrCmd, key "J" ]
    , shortcutRow "Add card above" [ key ctrlOrCmd, key "↑", text " or ", key ctrlOrCmd, key "K" ]
    , shortcutRow "Add card to the right (as child)" [ key ctrlOrCmd, key "→", text " or ", key ctrlOrCmd, key "L" ]
    , shortcutRow "Delete card (and its children)" [ key ctrlOrCmd, key "Backspace" ]
    ]


normalNavigationShortcuts : String -> List (Html msg)
normalNavigationShortcuts ctrlOrCmd =
    [ shortcutRow "Go up/down/left/right" [ key "↑", key "↓", key "←", key "→", text " or ", key "H", key "J", key "K", key "L" ]
    , shortcutRow "Go to beginning of group" [ key "PageUp" ]
    , shortcutRow "Go to end of group" [ key "PageDown" ]
    , shortcutRow "Go to beginning of column" [ key "Home" ]
    , shortcutRow "Go to end of column" [ key "End" ]
    , shortcutRow "Move current card (and children)" [ key "Alt", key "(any of above)", text " or ", dragCommand "Drag card by left edge" ]
    ]


normalAdvancedShortcuts : String -> List (Html msg)
normalAdvancedShortcuts ctrlOrCmd =
    [ shortcutRow "Search" [ key "/" ]
    , shortcutRow "Clear search, focus current card" [ key "Esc" ]
    , shortcutRow "Merge card up" [ key ctrlOrCmd, key "Shift", key "↑", text " or ", key ctrlOrCmd, key "Shift", key "J" ]
    , shortcutRow "Merge card down" [ key ctrlOrCmd, key "Shift", key "↓", text " or ", key ctrlOrCmd, key "Shift", key "K" ]
    ]


normalCopyShortcuts : String -> List (Html msg)
normalCopyShortcuts ctrlOrCmd =
    [ shortcutRow "Copy current subtree" [ key ctrlOrCmd, key "C" ]
    , shortcutRow "Paste subtree below current card" [ key ctrlOrCmd, key "V" ]
    , shortcutRow "Paste subtree as child of current card" [ key ctrlOrCmd, key "Shift", key "V" ]
    , shortcutRow "Insert selected text as new card" [ dragCommand "Drag selected text into tree" ]
    ]


normalOtherShortcuts : String -> List (Html msg)
normalOtherShortcuts ctrlOrCmd =
    [ shortcutRow "Word counts" [ key "W" ]
    , shortcutRow "Switch to different document" [ key ctrlOrCmd, key "O" ]
    , shortcutRow "This help screen" [ key "?" ]
    ]


editSaveShortcuts : String -> List (Html msg)
editSaveShortcuts ctrlOrCmd =
    [ shortcutRow "Save changes" [ key ctrlOrCmd, key "S" ]
    , shortcutRow "Save changes and exit card" [ key ctrlOrCmd, key "Enter" ]
    , shortcutRow "Add card below (split at cursor)" [ key ctrlOrCmd, key "J" ]
    , shortcutRow "Add card above (split at cursor)" [ key ctrlOrCmd, key "K" ]
    , shortcutRow "Add card to the right (split at cursor)" [ key ctrlOrCmd, key "L" ]
    , shortcutRow "Exit edit mode" [ key "Esc" ]
    ]


editFormatShortcuts : String -> List (Html msg)
editFormatShortcuts ctrlOrCmd =
    [ shortcutRow "Bold selection" [ key ctrlOrCmd, key "B" ]
    , shortcutRow "Italicize selection" [ key ctrlOrCmd, key "I" ]
    , shortcutRow "Set title level (# to #####)" [ key "Alt", key "1", text " ... ", key "6" ]
    ]


shortcutRow : String -> List (Html msg) -> Html msg
shortcutRow desc keys =
    tr [ class "shortcut-row" ] [ td [ style "text-align" "right" ] keys, td [] [ text (": " ++ desc) ] ]


key : String -> Html msg
key str =
    span [ class "shortcut-key" ] [ text str ]


dragCommand : String -> Html msg
dragCommand str =
    span [ class "shortcut-key", class "drag-command" ] [ text str ]
