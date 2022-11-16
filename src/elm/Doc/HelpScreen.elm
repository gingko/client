module Doc.HelpScreen exposing (view, viewShortcuts)

import Ant.Icons.Svg as Icons
import Html exposing (Html, a, button, div, h2, h3, h4, kbd, li, span, table, td, th, thead, ul)
import Html.Attributes exposing (class, colspan, height, href, id, style, target, width)
import Html.Events exposing (onClick)
import SharedUI exposing (ctrlOrCmdText)
import Translation exposing (Language(..), TranslationId(..), tr)



-- Translation Helper Function


text : Language -> TranslationId -> Html msg
text lang tid =
    Html.text <| tr lang tid


textNoTr : String -> Html msg
textNoTr str =
    Html.text str


emptyText : Html msg
emptyText =
    Html.text ""



-- VIEW


view : Language -> Bool -> { closeModal : msg, showVideoTutorials : msg, showWidget : msg, contactSupport : msg } -> List (Html msg)
view lang isMac msg =
    [ div [ class "modal-overlay", onClick msg.closeModal ] []
    , div [ class "max-width-grid" ]
        [ div [ class "modal", class "help-modal" ]
            [ div [ class "modal-header" ]
                [ h2 [] [ text lang Help ]
                , div [ class "close-button", onClick msg.closeModal ] [ Icons.closeCircleOutlined [ width 20, height 20 ] ]
                ]
            , div [ class "modal-guts" ]
                (viewShortcuts lang isMac)
            , div [ class "modal-buttons" ]
                [ div [ onClick msg.showVideoTutorials ] [ text lang HelpVideos ]
                , div [ onClick msg.showWidget ] [ text lang FAQAndDocs ]
                , div [ id "email-support", onClick msg.contactSupport ] [ text lang ContactSupport ]
                ]
            ]
        ]
    ]


viewShortcuts : Language -> Bool -> List (Html msg)
viewShortcuts lang isMac =
    let
        ctrlOrCmd =
            ctrlOrCmdText isMac
    in
    [ h2 [ id "shortcut-main-title" ] [ text lang KeyboardShortcuts ]
    , div [ id "shortcut-modes-wrapper" ]
        [ div []
            [ h3 [ id "view-mode-shortcuts-title" ] [ text lang ViewModeShortcuts ]
            , shortcutTable lang CardEditCreateDelete (normalEditShortcuts lang ctrlOrCmd)
            , shortcutTable lang NavigationMovingCards (normalNavigationShortcuts lang ctrlOrCmd)
            , shortcutTable lang CopyPaste (normalCopyShortcuts lang ctrlOrCmd)
            , shortcutTable lang SearchingMerging (normalAdvancedShortcuts lang ctrlOrCmd)
            , shortcutTable lang HelpInfoDocs (normalOtherShortcuts lang ctrlOrCmd)
            ]
        , div [ id "mode-divider" ] []
        , div []
            [ h3 [ id "edit-mode-shortcuts-title" ] [ text lang EditModeShortcuts ]
            , shortcutTable lang CardSaveCreate (editSaveShortcuts lang ctrlOrCmd)
            , shortcutTable lang Formatting (editFormatShortcuts lang ctrlOrCmd)
            ]
        ]
    ]


shortcutTable : Language -> TranslationId -> List (Html msg) -> Html msg
shortcutTable lang tableTitle tableRows =
    div [ class "shortcut-table-wrapper" ]
        [ h4 [ class "shortcut-table-title" ] [ text lang tableTitle ]
        , table [ class "shortcut-table" ] tableRows
        ]


keyNoTr : String -> Html msg
keyNoTr str =
    key En (NoTr str)


normalEditShortcuts : Language -> String -> List (Html msg)
normalEditShortcuts lang ctrlOrCmd =
    [ shortcutRow lang EditCard [ key lang EnterKey ]
    , shortcutRow lang EditCardFullscreen [ key lang ShiftKey, key lang EnterKey ]
    , shortcutRow lang AddCardBelow [ keyNoTr ctrlOrCmd, keyNoTr "↓", text lang Or, keyNoTr ctrlOrCmd, keyNoTr "J" ]
    , shortcutRow lang AddCardAbove [ keyNoTr ctrlOrCmd, keyNoTr "↑", text lang Or, keyNoTr ctrlOrCmd, keyNoTr "K" ]
    , shortcutRow lang AddCardToRight [ keyNoTr ctrlOrCmd, keyNoTr "→", text lang Or, keyNoTr ctrlOrCmd, keyNoTr "L" ]
    , shortcutRow lang DeleteCard [ keyNoTr ctrlOrCmd, key lang Backspace ]
    ]


normalNavigationShortcuts : Language -> String -> List (Html msg)
normalNavigationShortcuts lang ctrlOrCmd =
    [ shortcutRow lang GoUpDownLeftRight [ keyNoTr "↑", keyNoTr "↓", keyNoTr "←", keyNoTr "→", text lang Or, keyNoTr "H", keyNoTr "J", keyNoTr "K", keyNoTr "L" ]
    , shortcutRow lang GoToBeginningOfGroup [ key lang PageUp ]
    , shortcutRow lang GoToEndOfGroup [ key lang PageDown ]
    , shortcutRow lang GoToBeginningOfColumn [ key lang HomeKey ]
    , shortcutRow lang GoToEndOfColumn [ key lang EndKey ]
    , shortcutRow lang MoveCurrentCard [ key lang AltKey, key lang AnyOfAbove, text lang Or, dragCommand lang DragCard ]
    ]


normalAdvancedShortcuts : Language -> String -> List (Html msg)
normalAdvancedShortcuts lang ctrlOrCmd =
    [ shortcutRow lang Search [ keyNoTr "/" ]
    , shortcutRow lang ClearSearch [ key lang EscKey ]
    , shortcutRow lang MergeCardUp [ keyNoTr ctrlOrCmd, key lang ShiftKey, keyNoTr "↑", text lang Or, keyNoTr ctrlOrCmd, key lang ShiftKey, keyNoTr "J" ]
    , shortcutRow lang MergeCardDown [ keyNoTr ctrlOrCmd, key lang ShiftKey, keyNoTr "↓", text lang Or, keyNoTr ctrlOrCmd, key lang ShiftKey, keyNoTr "K" ]
    ]


normalCopyShortcuts : Language -> String -> List (Html msg)
normalCopyShortcuts lang ctrlOrCmd =
    [ shortcutRow lang CopyCurrent [ keyNoTr ctrlOrCmd, keyNoTr "C" ]
    , shortcutRow lang PasteBelow [ keyNoTr ctrlOrCmd, keyNoTr "V" ]
    , shortcutRow lang PasteAsChild [ keyNoTr ctrlOrCmd, key lang ShiftKey, keyNoTr "V" ]
    , shortcutRow lang InsertSelected [ dragCommand lang DragSelected ]
    ]


normalOtherShortcuts : Language -> String -> List (Html msg)
normalOtherShortcuts lang ctrlOrCmd =
    [ shortcutRow lang WordCounts [ keyNoTr "W" ]
    , shortcutRow lang SwitchDocuments [ keyNoTr ctrlOrCmd, keyNoTr "O" ]
    , shortcutRow lang ThisHelpScreen [ keyNoTr "?" ]
    ]


editSaveShortcuts : Language -> String -> List (Html msg)
editSaveShortcuts lang ctrlOrCmd =
    [ shortcutRow lang SaveChanges [ keyNoTr ctrlOrCmd, keyNoTr "S" ]
    , shortcutRow lang SaveChangesAndExit [ keyNoTr ctrlOrCmd, key lang EnterKey ]
    , shortcutRow lang AddCardBelowSplit [ keyNoTr ctrlOrCmd, keyNoTr "J" ]
    , shortcutRow lang AddCardAboveSplit [ keyNoTr ctrlOrCmd, keyNoTr "K" ]
    , shortcutRow lang AddCardToRightSplit [ keyNoTr ctrlOrCmd, keyNoTr "L" ]
    , shortcutRow lang ExitEditMode [ key lang EscKey ]
    ]


editFormatShortcuts : Language -> String -> List (Html msg)
editFormatShortcuts lang ctrlOrCmd =
    [ shortcutRow lang BoldSelection [ keyNoTr ctrlOrCmd, keyNoTr "B" ]
    , shortcutRow lang ItalicizeSelection [ keyNoTr ctrlOrCmd, keyNoTr "I" ]
    , shortcutRow lang SetTitleLevel [ key lang AltKey, keyNoTr "1", text lang (NoTr " ... "), keyNoTr "6" ]
    ]


shortcutRow : Language -> TranslationId -> List (Html msg) -> Html msg
shortcutRow lang desc keys =
    Html.tr [ class "shortcut-row" ] [ td [ style "text-align" "right" ] keys, td [] [ Html.text (": " ++ tr lang desc) ] ]


key : Language -> TranslationId -> Html msg
key lang str =
    span [ class "shortcut-key" ] [ text lang str ]


dragCommand : Language -> TranslationId -> Html msg
dragCommand lang str =
    span [ class "shortcut-key", class "drag-command" ] [ text lang str ]
