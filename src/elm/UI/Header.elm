module UI.Header exposing (HeaderMenuState(..), viewHeader)

import Ant.Icons.Svg as AntIcons
import Doc.Data as Data
import Doc.History as History
import Doc.UI exposing (viewSaveIndicator)
import GlobalData exposing (GlobalData)
import Html exposing (Html, div, h4, input, span)
import Html.Attributes exposing (attribute, class, classList, id, size, style, type_, value, width)
import Html.Attributes.Extra exposing (attributeIf)
import Html.Events exposing (keyCode, on, onBlur, onClick, onFocus, onInput, onMouseEnter, onMouseLeave)
import Html.Extra exposing (viewIf)
import Json.Decode as Dec
import Page.Doc.Export exposing (ExportFormat(..), ExportSelection(..))
import Page.Doc.Theme exposing (Theme(..))
import Session exposing (LoggedIn)
import Time
import Translation exposing (Language, TranslationId(..))
import Types exposing (Collaborator, TooltipPosition(..), Tree)
import UI.Collaborators
import Utils exposing (emptyText, text, textNoTr)



-- MODEL


type HeaderMenuState
    = NoHeaderMenu
    | ExportPreview
    | HistoryView History.History
    | Settings



-- VIEW


viewHeader :
    { noOp : msg
    , titleFocused : msg
    , titleFieldChanged : String -> msg
    , titleEdited : msg
    , titleEditCanceled : msg
    , tooltipRequested : String -> TooltipPosition -> TranslationId -> msg
    , tooltipClosed : msg
    , migrateClicked : msg
    , toggledHistory : Bool -> msg
    , checkoutTree : String -> msg
    , restore : msg
    , cancelHistory : msg
    , toggledDocSettings : msg
    , wordCountClicked : msg
    , themeChanged : Theme -> msg
    , toggledExport : msg
    , exportSelectionChanged : ExportSelection -> msg
    , exportFormatChanged : ExportFormat -> msg
    , export : msg
    , printRequested : msg
    , toggledUpgradeModal : Bool -> msg
    }
    ->
        { session : LoggedIn
        , title_ : Maybe String
        , titleField_ : Maybe String
        , headerMenu : HeaderMenuState
        , isGitLike : Bool
        , exportSettings : ( ExportSelection, ExportFormat )
        , data : Data.Model
        , dirty : Bool
        , lastLocalSave : Maybe Time.Posix
        , lastRemoteSave : Maybe Time.Posix
        , globalData : GlobalData
        , collaborators : List Collaborator
        }
    -> Html msg
viewHeader msgs { session, title_, titleField_, headerMenu, isGitLike, exportSettings, data, dirty, lastLocalSave, lastRemoteSave, globalData, collaborators } =
    let
        language =
            GlobalData.language globalData

        currentTime =
            GlobalData.currentTime globalData

        handleKeys =
            on "keyup"
                (Dec.andThen
                    (\int ->
                        case int of
                            27 ->
                                Dec.succeed msgs.titleEditCanceled

                            13 ->
                                Dec.succeed msgs.titleEdited

                            _ ->
                                Dec.fail "Ignore keyboard event"
                    )
                    keyCode
                )

        titleArea =
            let
                titleString =
                    titleField_ |> Maybe.withDefault "Untitled"
            in
            span [ id "title" ]
                [ div [ class "title-grow-wrap" ]
                    [ div [ class "shadow" ]
                        [ Html.text <|
                            if titleString /= "" then
                                titleString

                            else
                                " "
                        ]
                    , input
                        [ id "title-rename"
                        , type_ "text"
                        , onInput msgs.titleFieldChanged
                        , onBlur msgs.titleEdited
                        , onFocus msgs.titleFocused
                        , handleKeys
                        , size 1
                        , value titleString
                        , attribute "data-private" "lipsum"
                        ]
                        []
                    ]
                , viewSaveIndicator language
                    { dirty = dirty, lastLocalSave = lastLocalSave, lastRemoteSave = lastRemoteSave }
                    (GlobalData.currentTime globalData)
                ]

        isHistoryView =
            case headerMenu of
                HistoryView _ ->
                    True

                _ ->
                    False
    in
    div [ id "document-header" ]
        [ titleArea
        , viewIf (not (List.isEmpty collaborators)) <|
            div
                [ id "header-collaborators" ]
                [ UI.Collaborators.viewHeader collaborators ]
        , viewIf isGitLike <|
            div
                [ id "migrate-button"
                , class "header-button"
                , onClick msgs.migrateClicked
                , onMouseEnter <| msgs.tooltipRequested "migrate-button" BelowTooltip MigrateTooltip
                , onMouseLeave msgs.tooltipClosed
                ]
                [ AntIcons.thunderboltFilled [] ]
        , div
            [ id "history-icon"
            , class "header-button"
            , classList [ ( "open", isHistoryView ) ]
            , onClick <| msgs.toggledHistory (not isHistoryView)
            , attributeIf (not isHistoryView) <| onMouseEnter <| msgs.tooltipRequested "history-icon" BelowTooltip VersionHistory
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.historyOutlined [] ]
        , case headerMenu of
            HistoryView historyModel ->
                History.view
                    { lang = language
                    , noOp = msgs.noOp
                    , checkoutTree = msgs.checkoutTree
                    , restore = msgs.restore
                    , cancel = msgs.cancelHistory
                    , tooltipRequested = msgs.tooltipRequested
                    , tooltipClosed = msgs.tooltipClosed
                    }
                    historyModel

            _ ->
                emptyText
        , div
            [ id "doc-settings-icon"
            , class "header-button"
            , classList [ ( "open", headerMenu == Settings ) ]
            , onClick msgs.toggledDocSettings
            , attributeIf (headerMenu /= Settings) <| onMouseEnter <| msgs.tooltipRequested "doc-settings-icon" BelowLeftTooltip DocumentSettings
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.controlOutlined [] ]
        , viewIf (headerMenu == Settings) <|
            div [ id "doc-settings-menu", class "header-menu" ]
                [ div [ id "wordcount-menu-item", onClick msgs.wordCountClicked ] [ text language WordCount ]
                , h4 [] [ text language DocumentTheme ]
                , div [ onClick <| msgs.themeChanged Default ] [ text language ThemeDefault ]
                , div [ onClick <| msgs.themeChanged Dark ] [ text language ThemeDarkMode ]
                , div [ onClick <| msgs.themeChanged Classic ] [ text language ThemeClassic ]
                , div [ onClick <| msgs.themeChanged Gray ] [ text language ThemeGray ]
                , div [ onClick <| msgs.themeChanged Green ] [ text language ThemeGreen ]
                , div [ onClick <| msgs.themeChanged Turquoise ] [ text language ThemeTurquoise ]
                ]
        , viewIf (headerMenu == Settings) <| div [ id "doc-settings-menu-exit-left", onMouseEnter msgs.toggledDocSettings ] []
        , viewIf (headerMenu == Settings) <| div [ id "doc-settings-menu-exit-bottom", onMouseEnter msgs.toggledDocSettings ] []
        , div
            [ id "export-icon"
            , class "header-button"
            , classList [ ( "open", headerMenu == ExportPreview ) ]
            , onClick msgs.toggledExport
            , attributeIf (headerMenu /= ExportPreview) <| onMouseEnter <| msgs.tooltipRequested "export-icon" BelowLeftTooltip ExportOrPrint
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.fileDoneOutlined [] ]
        , viewUpgradeButton
            msgs.toggledUpgradeModal
            globalData
            session
        , viewIf (headerMenu == ExportPreview) <| viewExportMenu language msgs False exportSettings
        ]


viewExportMenu :
    Language
    ->
        { m
            | exportSelectionChanged : ExportSelection -> msg
            , exportFormatChanged : ExportFormat -> msg
            , toggledExport : msg
            , tooltipRequested : String -> TooltipPosition -> TranslationId -> msg
            , tooltipClosed : msg
        }
    -> Bool
    -> ( ExportSelection, ExportFormat )
    -> Html msg
viewExportMenu language msgs showCloseButton ( exportSelection, exportFormat ) =
    let
        exportSelectionBtnAttributes expSel expSelString tooltipText =
            [ id <| "export-select-" ++ expSelString
            , onClick <| msgs.exportSelectionChanged expSel
            , classList [ ( "selected", expSel == exportSelection ) ]
            , onMouseEnter <| msgs.tooltipRequested ("export-select-" ++ expSelString) BelowTooltip tooltipText
            , onMouseLeave msgs.tooltipClosed
            ]

        exportFormatBtnAttributes expFormat expFormatString =
            [ id <| "export-format-" ++ expFormatString
            , onClick <| msgs.exportFormatChanged expFormat
            , classList [ ( "selected", expFormat == exportFormat ) ]
            ]
    in
    div [ id "export-menu" ]
        ([ div [ id "export-selection", class "toggle-button" ]
            [ div (exportSelectionBtnAttributes ExportEverything "all" ExportSettingEverythingDesc) [ text language ExportSettingEverything ]
            , div (exportSelectionBtnAttributes ExportSubtree "subtree" ExportSettingCurrentSubtreeDesc) [ text language ExportSettingCurrentSubtree ]
            , div (exportSelectionBtnAttributes ExportLeaves "leaves" ExportSettingLeavesOnlyDesc) [ text language ExportSettingLeavesOnly ]
            , div (exportSelectionBtnAttributes ExportCurrentColumn "column" ExportSettingCurrentColumnDesc) [ text language ExportSettingCurrentColumn ]
            ]
         , div [ id "export-format", class "toggle-button" ]
            [ div (exportFormatBtnAttributes DOCX "word") [ text language ExportSettingWord ]
            , div (exportFormatBtnAttributes PlainText "text") [ text language ExportSettingPlainText ]
            , div (exportFormatBtnAttributes OPML "opml") [ text language ExportSettingOPML ]
            , div (exportFormatBtnAttributes JSON "json") [ text language ExportSettingJSON ]
            ]
         ]
            ++ (if showCloseButton then
                    [ span
                        [ id "export-button-close"
                        , onClick msgs.toggledExport
                        , onMouseEnter <| msgs.tooltipRequested "export-button-close" BelowLeftTooltip CloseExportView
                        , onMouseLeave msgs.tooltipClosed
                        ]
                        [ AntIcons.closeCircleOutlined [ width 16 ] ]
                    ]

                else
                    []
               )
        )


viewUpgradeButton :
    (Bool -> msg)
    -> GlobalData
    -> LoggedIn
    -> Html msg
viewUpgradeButton toggledUpgradeModal globalData session =
    let
        lang =
            GlobalData.language globalData

        upgradeCTA isExpired prepends =
            div
                [ id "upgrade-cta"
                , onClick <| toggledUpgradeModal True
                , classList [ ( "trial-expired", isExpired ) ]
                ]
                (prepends ++ [ div [ id "upgrade-button" ] [ text lang Upgrade ] ])

        maybeUpgrade =
            case Session.daysLeft (GlobalData.currentTime globalData) session of
                Just daysLeft ->
                    let
                        trialClass =
                            if daysLeft <= 7 && daysLeft > 5 then
                                "trial-light"

                            else if daysLeft <= 5 && daysLeft > 3 then
                                "trial-medium"

                            else
                                "trial-dark"
                    in
                    if daysLeft <= 0 then
                        upgradeCTA True
                            [ span []
                                [ AntIcons.exclamationCircleOutlined [ width 16, style "margin-bottom" "-3px", style "margin-right" "6px" ]
                                , text lang TrialExpired
                                ]
                            ]

                    else if daysLeft <= 7 then
                        upgradeCTA False [ span [ class trialClass ] [ text lang (DaysLeft daysLeft) ] ]

                    else
                        upgradeCTA False []

                Nothing ->
                    emptyText
    in
    maybeUpgrade
