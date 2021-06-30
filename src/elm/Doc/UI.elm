module Doc.UI exposing (countWords, fillet, viewConflict, viewHeader, viewLoadingSpinner, viewMobileButtons, viewSaveIndicator, viewSearchField, viewShortcuts, viewSidebar, viewSidebarStatic, viewTemplateSelector, viewTooltip, viewVideo, viewWordCount)

import Ant.Icons.Svg as AntIcons
import Browser.Dom exposing (Element)
import Coders exposing (treeToMarkdownString)
import Diff exposing (..)
import Doc.Data as Data exposing (CommitObject)
import Doc.Data.Conflict as Conflict exposing (Conflict, Op(..), Selection(..), opString)
import Doc.List as DocList exposing (Model(..))
import Doc.Metadata as Metadata exposing (Metadata)
import Doc.TreeStructure as TreeStructure exposing (defaultTree)
import Doc.TreeUtils as TreeUtils exposing (..)
import Html exposing (Html, a, br, button, datalist, del, div, fieldset, h1, h2, h3, h4, h5, hr, iframe, img, input, ins, label, li, option, pre, select, small, span, text, ul)
import Html.Attributes as A exposing (..)
import Html.Attributes.Extra exposing (attributeIf)
import Html.Events exposing (keyCode, on, onBlur, onCheck, onClick, onFocus, onInput, onMouseEnter, onMouseLeave, onSubmit, stopPropagationOn)
import Html.Extra exposing (viewIf)
import Import.Template exposing (Template(..))
import Json.Decode as Dec
import List.Extra as ListExtra exposing (getAt)
import Octicons as Icon exposing (defaultOptions, fillRule)
import Page.Doc.Export exposing (ExportFormat(..), ExportSelection(..))
import Page.Doc.Theme exposing (Theme(..))
import Regex exposing (Regex, replace)
import Route
import Session exposing (PaymentStatus(..), Session)
import SharedUI exposing (modalWrapper)
import Svg exposing (g, svg)
import Svg.Attributes exposing (d, fill, fontFamily, fontSize, fontWeight, preserveAspectRatio, stroke, strokeDasharray, strokeDashoffset, strokeLinecap, strokeLinejoin, strokeMiterlimit, strokeWidth, textAnchor, version, viewBox)
import Time exposing (posixToMillis)
import Translation exposing (Language(..), TranslationId(..), datetimeFormat, langFromString, langToString, languageName, timeDistInWords, tr)
import Types exposing (Children(..), CursorPosition(..), HeaderMenuState(..), SidebarMenuState(..), SidebarState(..), SortBy(..), TextCursorInfo, TooltipPosition(..), ViewMode(..), ViewState)
import Utils exposing (onClickStop)



-- HEADER


viewHeader :
    { noOp : msg
    , titleFocused : msg
    , titleFieldChanged : String -> msg
    , titleEdited : msg
    , titleEditCanceled : msg
    , tooltipRequested : String -> TooltipPosition -> String -> msg
    , tooltipClosed : msg
    , toggledHistory : Bool -> msg
    , checkoutCommit : String -> msg
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
    -> Maybe String
    ->
        { m
            | titleField : Maybe String
            , data : Data.Model
            , headerMenu : HeaderMenuState
            , exportSettings : ( ExportSelection, ExportFormat )
            , dirty : Bool
            , lastLocalSave : Maybe Time.Posix
            , lastRemoteSave : Maybe Time.Posix
            , session : Session
        }
    -> Html msg
viewHeader msgs title_ model =
    let
        language =
            Session.language model.session

        currentTime =
            Session.currentTime model.session

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
                    model.titleField |> Maybe.withDefault "Untitled"
            in
            span [ id "title" ]
                [ div [ class "title-grow-wrap" ]
                    [ div [ class "shadow" ]
                        [ text <|
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
                , viewSaveIndicator language model (Session.currentTime model.session)
                ]

        isHistoryView =
            case model.headerMenu of
                HistoryView _ ->
                    True

                _ ->
                    False

        isSelected expSel =
            (model.exportSettings |> Tuple.first) == expSel

        exportSelectionBtnAttributes expSel expSelString =
            [ id <| "export-select-" ++ expSelString
            , onClick <| msgs.exportSelectionChanged expSel
            , classList [ ( "selected", isSelected expSel ) ]
            ]

        isFormat expFormat =
            (model.exportSettings |> Tuple.second) == expFormat

        exportFormatBtnAttributes expFormat expFormatString =
            [ id <| "export-format-" ++ expFormatString
            , onClick <| msgs.exportFormatChanged expFormat
            , classList [ ( "selected", isFormat expFormat ) ]
            ]
    in
    div [ id "document-header" ]
        [ titleArea
        , div
            [ id "history-icon"
            , class "header-button"
            , classList [ ( "open", isHistoryView ) ]
            , onClick <| msgs.toggledHistory (not isHistoryView)
            , attributeIf (not isHistoryView) <| onMouseEnter <| msgs.tooltipRequested "history-icon" BelowTooltip "Version History"
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.historyOutlined [] ]
        , case model.headerMenu of
            HistoryView historyState ->
                viewHistory language
                    { noOp = msgs.noOp
                    , checkout = msgs.checkoutCommit
                    , restore = msgs.restore
                    , cancel = msgs.cancelHistory
                    , tooltipRequested = msgs.tooltipRequested
                    , tooltipClosed = msgs.tooltipClosed
                    }
                    currentTime
                    model.data
                    historyState

            _ ->
                text ""
        , div
            [ id "doc-settings-icon"
            , class "header-button"
            , classList [ ( "open", model.headerMenu == Settings ) ]
            , onClick msgs.toggledDocSettings
            , attributeIf (model.headerMenu /= Settings) <| onMouseEnter <| msgs.tooltipRequested "doc-settings-icon" BelowTooltip "Document Settings"
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.controlOutlined [] ]
        , viewIf (model.headerMenu == Settings) <|
            div [ id "doc-settings-menu", class "header-menu" ]
                [ div [ id "wordcount-menu-item", onClick msgs.wordCountClicked ] [ text "Word count..." ]
                , h4 [] [ text "Document Theme" ]
                , div [ onClick <| msgs.themeChanged Default ] [ text "Default" ]
                , div [ onClick <| msgs.themeChanged Gray ] [ text "Gray" ]
                , div [ onClick <| msgs.themeChanged Green ] [ text "Green" ]
                , div [ onClick <| msgs.themeChanged Turquoise ] [ text "Turquoise" ]
                ]
        , viewIf (model.headerMenu == Settings) <| div [ id "doc-settings-menu-exit-left", onMouseEnter msgs.toggledDocSettings ] []
        , viewIf (model.headerMenu == Settings) <| div [ id "doc-settings-menu-exit-bottom", onMouseEnter msgs.toggledDocSettings ] []
        , div
            [ id "export-icon"
            , class "header-button"
            , classList [ ( "open", model.headerMenu == ExportPreview ) ]
            , onClick msgs.toggledExport
            , attributeIf (model.headerMenu /= ExportPreview) <| onMouseEnter <| msgs.tooltipRequested "export-icon" BelowTooltip "Export or Print"
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.fileDoneOutlined [] ]
        , viewUpgradeButton
            msgs.toggledUpgradeModal
            model.session
        , viewIf (model.headerMenu == ExportPreview) <|
            div [ id "export-menu" ]
                [ div [ id "export-selection", class "toggle-button" ]
                    [ div (exportSelectionBtnAttributes ExportEverything "all") [ text "Everything" ]
                    , div (exportSelectionBtnAttributes ExportSubtree "subtree") [ text "Current Subtree" ]
                    , div (exportSelectionBtnAttributes ExportCurrentColumn "column") [ text "Current Column" ]
                    ]
                , div [ id "export-format", class "toggle-button" ]
                    [ div (exportFormatBtnAttributes DOCX "word") [ text "Word" ]
                    , div (exportFormatBtnAttributes PlainText "text") [ text "Plain Text" ]
                    , div (exportFormatBtnAttributes JSON "json") [ text "JSON" ]
                    ]
                ]
        ]


viewSaveIndicator :
    Language
    -> { m | dirty : Bool, lastLocalSave : Maybe Time.Posix, lastRemoteSave : Maybe Time.Posix }
    -> Time.Posix
    -> Html msg
viewSaveIndicator language { dirty, lastLocalSave, lastRemoteSave } currentTime =
    let
        lastChangeString =
            timeDistInWords
                language
                (lastLocalSave |> Maybe.withDefault (Time.millisToPosix 0))
                currentTime

        saveStateSpan =
            if dirty then
                span [ title (tr language LastSaved ++ " " ++ lastChangeString) ] [ text <| tr language UnsavedChanges ]

            else
                case ( lastLocalSave, lastRemoteSave ) of
                    ( Nothing, Nothing ) ->
                        span [] [ text <| tr language NeverSaved ]

                    ( Just time, Nothing ) ->
                        if Time.posixToMillis time == 0 then
                            span [] [ text <| tr language NeverSaved ]

                        else
                            span [ title (tr language LastEdit ++ " " ++ lastChangeString) ] [ text <| tr language SavedInternally ]

                    ( Just commitTime, Just fileTime ) ->
                        if posixToMillis commitTime <= posixToMillis fileTime then
                            span [ title (tr language LastEdit ++ " " ++ lastChangeString) ]
                                [ text <| tr language ChangesSynced ]

                        else
                            span [ title (tr language LastEdit ++ " " ++ lastChangeString) ] [ text <| tr language SavedInternally ]

                    ( Nothing, Just _ ) ->
                        span [ title (tr language LastEdit ++ " " ++ lastChangeString) ] [ text <| tr language DatabaseError ]
    in
    div
        [ id "save-indicator", classList [ ( "inset", True ), ( "saving", dirty ) ] ]
        [ saveStateSpan
        ]


viewUpgradeButton :
    (Bool -> msg)
    -> Session
    -> Html msg
viewUpgradeButton toggledUpgradeModal session =
    let
        currentTime =
            Session.currentTime session

        lang =
            Session.language session

        upgradeButton =
            div [ id "upgrade-button", onClick <| toggledUpgradeModal True ] [ text "Upgrade" ]

        maybeUpgrade =
            case Session.paymentStatus session of
                Customer _ ->
                    text ""

                Trial expiry ->
                    let
                        daysLeft =
                            ((Time.posixToMillis expiry - Time.posixToMillis currentTime) |> toFloat)
                                / (1000 * 3600 * 24)
                                |> round

                        trialClass =
                            if daysLeft <= 7 && daysLeft > 5 then
                                "trial-light"

                            else if daysLeft <= 5 && daysLeft > 3 then
                                "trial-medium"

                            else
                                "trial-dark"
                    in
                    if daysLeft <= 7 then
                        upgradeButton

                    else
                        upgradeButton

                Unknown ->
                    upgradeButton
    in
    maybeUpgrade



-- SIDEBAR


type alias SidebarMsgs msg =
    { sidebarStateChanged : SidebarState -> msg
    , noOp : msg
    , clickedNew : msg
    , tooltipRequested : String -> TooltipPosition -> String -> msg
    , tooltipClosed : msg
    , clickedSwitcher : msg
    , clickedHelp : msg
    , toggledShortcuts : msg
    , clickedEmailSupport : msg
    , languageMenuRequested : Maybe String -> msg
    , toggledAccount : Bool -> msg
    , logout : msg
    , fileSearchChanged : String -> msg
    , changeSortBy : SortBy -> msg
    , contextMenuOpened : String -> ( Float, Float ) -> msg
    , languageChanged : Language -> msg
    , fullscreenRequested : msg
    }


viewSidebar :
    Session
    -> SidebarMsgs msg
    -> Metadata
    -> SortBy
    -> String
    -> DocList.Model
    -> String
    -> Maybe String
    -> SidebarMenuState
    -> SidebarState
    -> Html msg
viewSidebar session msgs currentDocument sortCriteria fileFilter docList accountEmail contextTarget_ dropdownState sidebarState =
    let
        lang =
            Session.language session

        custId_ =
            case Session.paymentStatus session of
                Customer custId ->
                    Just custId

                _ ->
                    Nothing

        isOpen =
            not (sidebarState == SidebarClosed)

        helpOpen =
            dropdownState == Help

        accountOpen =
            case dropdownState of
                Account _ ->
                    True

                _ ->
                    False

        toggle menu =
            if sidebarState == menu then
                msgs.sidebarStateChanged <| SidebarClosed

            else
                msgs.sidebarStateChanged <| menu

        viewIf cond v =
            if cond then
                v

            else
                text ""
    in
    div [ id "sidebar", onClick <| toggle File, classList [ ( "open", isOpen ) ] ]
        ([ div [ id "brand" ]
            ([ img [ src "../gingko-leaf-logo.svg", width 28 ] [] ]
                ++ (if isOpen then
                        [ h2 [ id "brand-name" ] [ text "Gingko Writer" ]
                        , div [ id "sidebar-collapse-icon" ] [ AntIcons.leftOutlined [] ]
                        ]

                    else
                        [ text "" ]
                   )
                ++ [ div [ id "hamburger-icon" ] [ AntIcons.menuOutlined [] ] ]
            )
         , div
            [ id "new-icon"
            , class "sidebar-button"
            , onClickStop msgs.clickedNew
            , onMouseEnter <| msgs.tooltipRequested "new-icon" RightTooltip "New Document"
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.fileAddOutlined []
            , div [ id "welcome-step-7", class "tour-step" ]
                [ text "Click for New Document"
                , div [ class "arrow" ] [ text "◀" ]
                , div [ id "progress-step-7", class "tour-step-progress" ]
                    [ div [ class "bg-line", class "on" ] []
                    , div [ class "bg-line", class "off" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    ]
                ]
            ]
         , div
            [ id "documents-icon"
            , class "sidebar-button"
            , classList [ ( "open", isOpen ) ]
            , attributeIf (not isOpen) <| onMouseEnter <| msgs.tooltipRequested "documents-icon" RightTooltip "Show Document List"
            , attributeIf (not isOpen) <| onMouseLeave msgs.tooltipClosed
            ]
            [ if isOpen then
                AntIcons.folderOpenOutlined []

              else
                AntIcons.folderOutlined []
            ]
         , viewIf isOpen <|
            DocList.viewSidebarList
                { noOp = msgs.noOp
                , filter = msgs.fileSearchChanged
                , changeSortBy = msgs.changeSortBy
                , contextMenu = msgs.contextMenuOpened
                , tooltipRequested = msgs.tooltipRequested
                , tooltipClosed = msgs.tooltipClosed
                }
                currentDocument
                sortCriteria
                contextTarget_
                fileFilter
                docList
         , div
            [ id "document-switcher-icon"
            , onClickStop msgs.clickedSwitcher
            , onMouseEnter <| msgs.tooltipRequested "document-switcher-icon" RightTooltip "Open Quick Switcher"
            , onMouseLeave msgs.tooltipClosed
            , class "sidebar-button"
            , attributeIf (docList == Success []) (class "disabled")
            ]
            [ AntIcons.fileSearchOutlined [] ]
         , div
            [ id "help-icon"
            , class "sidebar-button"
            , classList [ ( "open", helpOpen ) ]
            , onClickStop msgs.clickedHelp
            , attributeIf (dropdownState /= Help) <| onMouseEnter <| msgs.tooltipRequested "help-icon" RightTooltip "Help"
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.questionCircleOutlined []
            , div [ id "welcome-step-6", class "tour-step" ]
                [ text "Click to see Help Menu"
                , div [ class "arrow" ] [ text "◀" ]
                , div [ id "progress-step-6", class "tour-step-progress" ]
                    [ div [ class "bg-line", class "on" ] []
                    , div [ class "bg-line", class "off" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [] []
                    ]
                ]
            ]
         , div
            [ id "account-icon"
            , class "sidebar-button"
            , classList [ ( "open", accountOpen ) ]
            , onClickStop <| msgs.toggledAccount (not accountOpen)
            , attributeIf (not accountOpen) <| onMouseEnter <| msgs.tooltipRequested "account-icon" RightTooltip "Account"
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.userOutlined [] ]
         ]
            ++ viewSidebarMenu lang
                custId_
                { toggledShortcuts = msgs.toggledShortcuts
                , clickedEmailSupport = msgs.clickedEmailSupport
                , helpClosed = msgs.clickedHelp
                , languageMenuRequested = msgs.languageMenuRequested
                , languageChanged = msgs.languageChanged
                , logout = msgs.logout
                , toggledAccount = msgs.toggledAccount
                , noOp = msgs.noOp
                }
                accountEmail
                dropdownState
        )


viewSidebarMenu :
    Language
    -> Maybe String
    ->
        { toggledShortcuts : msg
        , clickedEmailSupport : msg
        , helpClosed : msg
        , languageMenuRequested : Maybe String -> msg
        , languageChanged : Language -> msg
        , logout : msg
        , toggledAccount : Bool -> msg
        , noOp : msg
        }
    -> String
    -> SidebarMenuState
    -> List (Html msg)
viewSidebarMenu lang custId_ msgs accountEmail dropdownState =
    case dropdownState of
        Help ->
            [ div [ id "help-menu", class "sidebar-menu" ]
                [ a [ href "https://docs.gingkowriter.com", target "_blank", onClickStop msgs.noOp ] [ text "FAQ" ]
                , div [ onClickStop msgs.toggledShortcuts ] [ text <| tr lang KeyboardHelp ]
                , div [ id "email-support", onClickStop msgs.clickedEmailSupport ] [ text <| tr lang EmailSupport ]
                ]
            , div [ id "help-menu-exit-top", onMouseEnter msgs.helpClosed ] []
            , div [ id "help-menu-exit-right", onMouseEnter msgs.helpClosed ] []
            ]

        Account langMenuEl_ ->
            let
                manageSubBtn =
                    case custId_ of
                        Just custId ->
                            Html.form [ method "POST", action "/create-portal-session" ]
                                [ input [ type_ "hidden", name "customer_id", value custId ] []
                                , button [ id "manage-subscription-button", type_ "submit" ] [ text "Manage Subscription" ]
                                ]

                        Nothing ->
                            text ""
            in
            [ div [ id "account-menu", class "sidebar-menu" ]
                [ div [ onClickStop msgs.noOp, class "no-action" ] [ text accountEmail ]
                , manageSubBtn
                , div
                    [ id "language-option"
                    , if langMenuEl_ == Nothing then
                        onClickStop <| msgs.languageMenuRequested (Just "language-option")

                      else
                        onClickStop <| msgs.languageMenuRequested Nothing
                    , onMouseEnter <| msgs.languageMenuRequested (Just "language-option")
                    ]
                    [ text <| tr lang Language, div [ class "right-icon" ] [ AntIcons.rightOutlined [] ] ]
                , div [ id "logout-button", onClickStop msgs.logout ] [ text <| tr lang Logout ]
                ]
            , case langMenuEl_ of
                Just langMenuEl ->
                    div
                        [ id "language-menu"
                        , class "sidebar-menu"
                        , style "left" ((langMenuEl.element.x + langMenuEl.element.width |> String.fromFloat) ++ "px")
                        , style "bottom" ((langMenuEl.scene.height - langMenuEl.element.y - langMenuEl.element.height |> String.fromFloat) ++ "px")
                        ]
                        ((Translation.activeLanguages
                            |> List.map
                                (\( langOpt, langName ) ->
                                    div
                                        [ id <| "lang-" ++ langToString langOpt
                                        , onClickStop <| msgs.languageChanged langOpt
                                        , classList [ ( "selected", langOpt == lang ) ]
                                        ]
                                        [ text langName ]
                                )
                         )
                            ++ [ a
                                    [ href "https://poeditor.com/join/project?hash=k8Br3k0JVz"
                                    , target "_blank"
                                    , onClickStop <| msgs.toggledAccount False
                                    ]
                                    [ text <| tr lang ContributeTranslations ]
                               ]
                        )

                Nothing ->
                    text ""
            , viewIf (langMenuEl_ == Nothing) <| div [ id "help-menu-exit-top", onMouseEnter <| msgs.toggledAccount False ] []
            , viewIf (langMenuEl_ == Nothing) <| div [ id "help-menu-exit-right", onMouseEnter <| msgs.toggledAccount False ] []
            ]

        NoSidebarMenu ->
            [ text "" ]


viewSidebarStatic : Bool -> List (Html msg)
viewSidebarStatic sidebarOpen =
    [ div [ id "sidebar", classList [ ( "open", sidebarOpen ) ], class "static" ]
        [ div [ id "brand" ]
            ([ img [ src "../gingko-leaf-logo.svg", width 28 ] [] ]
                ++ (if sidebarOpen then
                        [ h2 [ id "brand-name" ] [ text "Gingko Writer" ]
                        , div [ id "sidebar-collapse-icon" ] [ AntIcons.leftOutlined [] ]
                        ]

                    else
                        [ text "" ]
                   )
            )
        , viewIf sidebarOpen <| div [ id "sidebar-document-list-wrap" ] []
        , div [ id "new-icon", class "sidebar-button" ] [ AntIcons.fileAddOutlined [] ]
        , div [ id "documents-icon", class "sidebar-button", classList [ ( "open", sidebarOpen ) ] ]
            [ if sidebarOpen then
                AntIcons.folderOpenOutlined []

              else
                AntIcons.folderOutlined []
            ]
        , div [ id "document-switcher-icon", class "sidebar-button", class "disabled" ] [ AntIcons.fileSearchOutlined [] ]
        , div
            [ id "help-icon", class "sidebar-button" ]
            [ AntIcons.questionCircleOutlined [] ]
        , div [ id "account-icon", class "sidebar-button" ] [ AntIcons.userOutlined [] ]
        ]
    ]


viewLoadingSpinner : Bool -> Html msg
viewLoadingSpinner sidebarOpen =
    div [ id "app-root", class "loading" ]
        ([ div [ id "document-header" ] []
         , div [ id "loading-overlay" ] []
         , div [ class "spinner" ] [ div [ class "bounce1" ] [], div [ class "bounce2" ] [], div [ class "bounce3" ] [] ]
         ]
            ++ viewSidebarStatic sidebarOpen
        )



-- MODALS


viewTemplateSelector :
    Language
    -> { modalClosed : msg, importBulkClicked : msg, importJSONRequested : msg }
    -> List (Html msg)
viewTemplateSelector language msgs =
    [ div [ id "templates-block" ]
        [ a [ id "template-new", class "template-item", href (Route.toString Route.DocNew) ]
            [ div [ classList [ ( "template-thumbnail", True ), ( "new", True ) ] ] []
            , div [ class "template-title" ] [ text <| tr language HomeBlank ]
            ]
        , div [ id "template-import-bulk", class "template-item", onClick msgs.importBulkClicked ]
            [ div [ classList [ ( "template-thumbnail", True ) ] ] [ Icon.fileZip (Icon.defaultOptions |> Icon.size 48) ]
            , div [ class "template-title" ] [ text <| tr language HomeImportLegacy ]
            , div [ class "template-description" ]
                [ text <| tr language HomeLegacyFrom ]
            ]
        , div [ id "template-import", class "template-item", onClick msgs.importJSONRequested ]
            [ div [ classList [ ( "template-thumbnail", True ) ] ] [ Icon.fileCode (Icon.defaultOptions |> Icon.size 48) ]
            , div [ class "template-title" ] [ text <| tr language HomeImportJSON ]
            , div [ class "template-description" ]
                [ text <| tr language HomeJSONFrom ]
            ]
        , a [ id "template-timeline", class "template-item", href <| Route.toString (Route.Import Timeline) ]
            [ div [ classList [ ( "template-thumbnail", True ) ] ] [ Icon.lightBulb (Icon.defaultOptions |> Icon.size 48) ]
            , div [ class "template-title" ] [ text "Timeline 2021" ]
            , div [ class "template-description" ]
                [ text "A tree-based calendar" ]
            ]
        , a [ id "template-academic", class "template-item", href <| Route.toString (Route.Import AcademicPaper) ]
            [ div [ classList [ ( "template-thumbnail", True ) ] ] [ Icon.lightBulb (Icon.defaultOptions |> Icon.size 48) ]
            , div [ class "template-title" ] [ text "Academic Paper" ]
            , div [ class "template-description" ]
                [ text "Starting point for journal paper" ]
            ]
        , a [ id "template-project", class "template-item", href <| Route.toString (Route.Import ProjectBrainstorming) ]
            [ div [ classList [ ( "template-thumbnail", True ) ] ] [ Icon.lightBulb (Icon.defaultOptions |> Icon.size 48) ]
            , div [ class "template-title" ] [ text "Project Brainstorming" ]
            , div [ class "template-description" ]
                [ text "Clarify project goals" ]
            ]
        , a [ id "template-heros-journey", class "template-item", href <| Route.toString (Route.Import HerosJourney) ]
            [ div [ classList [ ( "template-thumbnail", True ) ] ] [ Icon.lightBulb (Icon.defaultOptions |> Icon.size 48) ]
            , div [ class "template-title" ] [ text "Hero's Journey" ]
            , div [ class "template-description" ]
                [ text "A framework for fictional stories" ]
            ]
        ]
    ]
        |> modalWrapper msgs.modalClosed Nothing "New Document"


viewWordCount :
    { m
        | viewState : ViewState
        , workingTree : TreeStructure.Model
        , startingWordcount : Int
        , wordcountTrayOpen : Bool
        , session : Session
    }
    -> { modalClosed : msg }
    -> List (Html msg)
viewWordCount model msgs =
    let
        language =
            Session.language model.session

        stats =
            getStats model

        current =
            stats.documentWords

        session =
            current - model.startingWordcount
    in
    [ span [] [ text (tr language (WordCountSession session)) ]
    , span [] [ text (tr language (WordCountTotal current)) ]
    , span [] [ text (tr language (WordCountCard stats.cardWords)) ]
    , span [] [ text (tr language (WordCountSubtree stats.subtreeWords)) ]
    , span [] [ text (tr language (WordCountGroup stats.groupWords)) ]
    , span [] [ text (tr language (WordCountColumn stats.columnWords)) ]
    , hr [] []
    , span [] [ text ("Total Cards in Tree : " ++ String.fromInt stats.cards) ]
    ]
        |> modalWrapper msgs.modalClosed Nothing "Word Counts"



-- DOCUMENT


viewSearchField : (String -> msg) -> { m | viewState : ViewState, session : Session } -> Html msg
viewSearchField searchFieldMsg { viewState, session } =
    let
        language =
            Session.language session

        maybeSearchIcon =
            if viewState.searchField == Nothing then
                Icon.search (defaultOptions |> Icon.color "#445" |> Icon.size 12)

            else
                text ""
    in
    case viewState.viewMode of
        Normal ->
            div
                [ id "search-field" ]
                [ input
                    [ type_ "search"
                    , id "search-input"
                    , required True
                    , title (tr language PressToSearch)
                    , onInput searchFieldMsg
                    ]
                    []
                , maybeSearchIcon
                ]

        _ ->
            div
                [ id "search-field" ]
                []


viewMobileButtons :
    { edit : msg
    , save : msg
    , cancel : msg
    , plusRight : msg
    , plusDown : msg
    , plusUp : msg
    , navLeft : msg
    , navUp : msg
    , navDown : msg
    , navRight : msg
    }
    -> Bool
    -> Html msg
viewMobileButtons msgs isEditing =
    if isEditing then
        div [ id "mobile-buttons", class "footer" ]
            [ span [ id "mbtn-cancel", class "mobile-button", onClick msgs.cancel ] [ AntIcons.stopOutlined [ width 18 ] ]
            , span [ id "mbtn-save", class "mobile-button", onClick msgs.save ] [ AntIcons.checkOutlined [ width 18 ] ]
            ]

    else
        div [ id "mobile-buttons", class "footer" ]
            [ span [ id "mbtn-edit", class "mobile-button", onClick msgs.edit ] [ AntIcons.editTwoTone [ width 18 ] ]
            , span [ id "mbtn-add-right", class "mobile-button", onClick msgs.plusRight ] [ AntIcons.plusSquareTwoTone [ width 18 ], AntIcons.rightOutlined [ width 14 ] ]
            , span [ id "mbtn-add-down", class "mobile-button", onClick msgs.plusDown ] [ AntIcons.plusSquareTwoTone [ width 18 ], AntIcons.downOutlined [ width 14 ] ]
            , span [ id "mbtn-add-up", class "mobile-button", onClick msgs.plusUp ] [ AntIcons.plusSquareTwoTone [ width 18 ], AntIcons.upOutlined [ width 14 ] ]
            , span [ id "mbtn-nav-left", class "mobile-button", onClick msgs.navLeft ] [ AntIcons.caretLeftOutlined [ width 18 ] ]
            , span [ id "mbtn-nav-up", class "mobile-button", onClick msgs.navUp ] [ AntIcons.caretUpOutlined [ width 18 ] ]
            , span [ id "mbtn-nav-down", class "mobile-button", onClick msgs.navDown ] [ AntIcons.caretDownOutlined [ width 18 ] ]
            , span [ id "mbtn-nav-right", class "mobile-button", onClick msgs.navRight ] [ AntIcons.caretRightOutlined [ width 18 ] ]
            ]


viewHistory :
    Translation.Language
    ->
        { noOp : msg
        , checkout : String -> msg
        , restore : msg
        , cancel : msg
        , tooltipRequested : String -> TooltipPosition -> String -> msg
        , tooltipClosed : msg
        }
    -> Time.Posix
    -> Data.Model
    -> { start : String, currentView : String }
    -> Html msg
viewHistory lang msgs currentTime dataModel historyState =
    let
        historyList =
            Data.historyList historyState.start dataModel

        maybeTimeDisplay =
            case Data.getCommit historyState.currentView dataModel of
                Just commit ->
                    let
                        commitPosix =
                            commit.timestamp |> Time.millisToPosix
                    in
                    div
                        [ id "history-time-info"
                        , onMouseEnter <| msgs.tooltipRequested "history-time-info" BelowTooltip (timeDistInWords lang commitPosix currentTime)
                        , onMouseLeave msgs.tooltipClosed
                        ]
                        [ text <| datetimeFormat lang commitPosix ]

                Nothing ->
                    text ""

        maxIdx =
            historyList
                |> List.length
                |> (\x -> x - 1)
                |> String.fromInt

        checkoutCommit idxStr =
            case String.toInt idxStr of
                Just idx ->
                    case getAt idx historyList of
                        Just commit ->
                            msgs.checkout (Tuple.first commit)

                        Nothing ->
                            msgs.noOp

                Nothing ->
                    msgs.noOp
    in
    div [ id "history-menu" ]
        [ input [ id "history-slider", type_ "range", A.min "0", A.max maxIdx, step "1", onInput checkoutCommit ] []
        , maybeTimeDisplay
        , button [ id "history-restore", onClick msgs.restore ] [ text <| tr lang RestoreThisVersion ]
        , div
            [ id "history-close-button"
            , onClick msgs.cancel
            , onMouseEnter <| msgs.tooltipRequested "history-close-button" BelowLeftTooltip "Cancel"
            , onMouseLeave <| msgs.tooltipClosed
            ]
            [ AntIcons.closeOutlined [] ]
        ]


viewVideo : (Bool -> msg) -> { m | videoModalOpen : Bool } -> Html msg
viewVideo modalMsg { videoModalOpen } =
    if videoModalOpen then
        div [ class "modal-container" ]
            [ div [ class "modal" ]
                [ div [ class "modal-header" ]
                    [ h1 [] [ text "Learning Videos" ]
                    , a [ onClick (modalMsg False) ] [ text "×" ]
                    ]
                , iframe
                    [ width 650
                    , height 366
                    , src "https://www.youtube.com/embed/ZOGgwKAU3vg?rel=0&amp;showinfo=0"
                    , attribute "frameborder" "0"
                    , attribute "allowfullscreen" ""
                    ]
                    []
                ]
            ]

    else
        div [] []


viewShortcuts :
    { toggledShortcutTray : msg, tooltipRequested : String -> TooltipPosition -> String -> msg, tooltipClosed : msg }
    -> Language
    -> Bool
    -> Bool
    -> Children
    -> TextCursorInfo
    -> ViewState
    -> List (Html msg)
viewShortcuts msgs lang isOpen isMac children textCursorInfo vs =
    let
        isTextSelected =
            textCursorInfo.selected

        isOnly =
            case children of
                Children [ singleRoot ] ->
                    if singleRoot.children == Children [] then
                        True

                    else
                        False

                _ ->
                    False

        viewIfNotOnly content =
            if not isOnly then
                content

            else
                text ""

        addInsteadOfSplit =
            textCursorInfo.position == End || textCursorInfo.position == Empty

        spanSplit key descAdd descSplit =
            if addInsteadOfSplit then
                shortcutSpan [ ctrlOrCmd, key ] descAdd

            else
                shortcutSpan [ ctrlOrCmd, key ] descSplit

        splitChild =
            spanSplit "L" (tr lang AddChildAction) (tr lang SplitChildAction)

        splitBelow =
            spanSplit "J" (tr lang AddBelowAction) (tr lang SplitBelowAction)

        splitAbove =
            spanSplit "K" (tr lang AddAboveAction) (tr lang SplitUpwardAction)

        shortcutSpanEnabled enabled keys desc =
            let
                keySpans =
                    keys
                        |> List.map (\k -> span [ class "shortcut-key" ] [ text k ])
            in
            span
                [ classList [ ( "disabled", not enabled ) ] ]
                (keySpans
                    ++ [ text (" " ++ desc) ]
                )

        shortcutSpan =
            shortcutSpanEnabled True

        formattingSpan markup =
            span [] [ pre [ class "formatting-text" ] [ text markup ] ]

        ctrlOrCmd =
            if isMac then
                "⌘"

            else
                "Ctrl"
    in
    if isOpen then
        let
            iconColor =
                Icon.color "#445"
        in
        case vs.viewMode of
            Normal ->
                [ div
                    [ id "shortcuts-tray", classList [ ( "open", isOpen ) ], onClick msgs.toggledShortcutTray ]
                    [ div [ id "shortcuts" ]
                        [ h3 [] [ text "Keyboard Shortcuts" ]
                        , h5 [] [ text "Edit Cards" ]
                        , shortcutSpan [ tr lang EnterKey ] (tr lang EnterAction)
                        , shortcutSpan [ "Shift", tr lang EnterKey ] (tr lang EditFullscreenAction)
                        , viewIfNotOnly <| h5 [] [ text "Navigate" ]
                        , viewIfNotOnly <| shortcutSpan [ "↑", "↓", "←", "→" ] (tr lang ArrowsAction)
                        , h5 [] [ text "Add New Cards" ]
                        , shortcutSpan [ ctrlOrCmd, "→" ] (tr lang AddChildAction)
                        , shortcutSpan [ ctrlOrCmd, "↓" ] (tr lang AddBelowAction)
                        , shortcutSpan [ ctrlOrCmd, "↑" ] (tr lang AddAboveAction)
                        , viewIfNotOnly <| h5 [] [ text "Move Cards" ]
                        , viewIfNotOnly <| shortcutSpan [ "Alt", tr lang ArrowKeys ] (tr lang MoveAction)
                        , viewIfNotOnly <| shortcutSpan [ ctrlOrCmd, tr lang Backspace ] (tr lang DeleteAction)
                        , viewIfNotOnly <| h5 [] [ text "Merge Cards" ]
                        , viewIfNotOnly <| shortcutSpan [ ctrlOrCmd, "Shift", "↓" ] (tr lang MergeDownAction)
                        , viewIfNotOnly <| shortcutSpan [ ctrlOrCmd, "Shift", "↑" ] (tr lang MergeUpAction)
                        , hr [] []
                        , h5 [] [ text "Other Shortcuts" ]
                        , shortcutSpan [ "w" ] "Display word counts"
                        , shortcutSpan [ ctrlOrCmd, "O" ] (tr lang QuickDocumentSwitcher)
                        ]
                    ]
                ]

            _ ->
                [ div
                    [ id "shortcuts-tray", classList [ ( "open", isOpen ) ], onClick msgs.toggledShortcutTray ]
                    [ div [ id "shortcuts" ]
                        [ h3 [] [ text "Keyboard Shortcuts" ]
                        , h3 [] [ text "(Edit Mode)" ]
                        , h5 [] [ text "Save/Cancel Changes" ]
                        , shortcutSpan [ ctrlOrCmd, tr lang EnterKey ] (tr lang ToSaveChanges)
                        , shortcutSpan [ tr lang EscKey ] (tr lang ToCancelChanges)
                        , if addInsteadOfSplit then
                            h5 [] [ text "Add New Cards" ]

                          else
                            h5 [] [ text "Split At Cursor" ]
                        , splitChild
                        , splitBelow
                        , splitAbove
                        , h5 [] [ text "Formatting" ]
                        , shortcutSpanEnabled isTextSelected [ ctrlOrCmd, "B" ] (tr lang ForBold)
                        , shortcutSpanEnabled isTextSelected [ ctrlOrCmd, "I" ] (tr lang ForItalic)
                        , shortcutSpan [ "Alt", "(number)" ] "Set heading level (0-6)"
                        , formattingSpan "# Title\n## Subtitle"
                        , formattingSpan "- List item\n  - Subitem"
                        , formattingSpan "[link](http://t.co)"
                        , span [ class "markdown-guide" ]
                            [ a [ href "http://commonmark.org/help", target "_blank" ]
                                [ text <| tr lang FormattingGuide
                                , span [ class "icon-container" ] [ Icon.linkExternal (defaultOptions |> iconColor |> Icon.size 14) ]
                                ]
                            ]
                        ]
                    ]
                ]

    else
        [ div
            [ id "shortcuts-tray"
            , onClick msgs.toggledShortcutTray
            , onMouseEnter <| msgs.tooltipRequested "shortcuts-tray" LeftTooltip "Keyboard Shortcuts"
            , onMouseLeave msgs.tooltipClosed
            ]
            [ keyboardIconSvg 24 ]
        ]



-- Word count


type alias Stats =
    { cardWords : Int
    , subtreeWords : Int
    , groupWords : Int
    , columnWords : Int
    , documentWords : Int
    , cards : Int
    }


viewWordcountProgress : Int -> Int -> Html msg
viewWordcountProgress current session =
    let
        currW =
            1 / (1 + toFloat session / toFloat current)

        sessW =
            1 - currW
    in
    div [ id "wc-progress" ]
        [ div [ id "wc-progress-wrap" ]
            [ span [ style "flex" (String.fromFloat currW), id "wc-progress-bar" ] []
            , span [ style "flex" (String.fromFloat sessW), id "wc-progress-bar-session" ] []
            ]
        ]


viewTooltip : ( Element, TooltipPosition, String ) -> Html msg
viewTooltip ( el, tipPos, content ) =
    let
        posAttributes =
            case tipPos of
                RightTooltip ->
                    [ style "left" <| ((el.element.x + el.element.width + 5) |> String.fromFloat) ++ "px"
                    , style "top" <| ((el.element.y + el.element.height * 0.5) |> String.fromFloat) ++ "px"
                    , style "transform" "translateY(-50%)"
                    , class "tip-right"
                    ]

                LeftTooltip ->
                    [ style "left" <| ((el.element.x - 5) |> String.fromFloat) ++ "px"
                    , style "top" <| ((el.element.y + el.element.height * 0.5) |> String.fromFloat) ++ "px"
                    , style "transform" "translate(-100%, -50%)"
                    , class "tip-left"
                    ]

                AboveTooltip ->
                    [ style "left" <| ((el.element.x + el.element.width * 0.5) |> String.fromFloat) ++ "px"
                    , style "top" <| ((el.element.y + 5) |> String.fromFloat) ++ "px"
                    , style "transform" "translate(-50%, calc(-100% - 10px))"
                    , class "tip-above"
                    ]

                BelowTooltip ->
                    [ style "left" <| ((el.element.x + el.element.width * 0.5) |> String.fromFloat) ++ "px"
                    , style "top" <| ((el.element.y + el.element.height + 5) |> String.fromFloat) ++ "px"
                    , style "transform" "translateX(-50%)"
                    , class "tip-below"
                    ]

                BelowLeftTooltip ->
                    [ style "left" <| ((el.element.x + el.element.width * 0.5) |> String.fromFloat) ++ "px"
                    , style "top" <| ((el.element.y + el.element.height + 5) |> String.fromFloat) ++ "px"
                    , style "transform" "translateX(calc(-100% + 5px))"
                    , class "tip-below-left"
                    ]
    in
    div ([ class "tooltip" ] ++ posAttributes)
        [ text content, div [ class "tooltip-arrow" ] [] ]


getStats : { m | viewState : ViewState, workingTree : TreeStructure.Model } -> Stats
getStats model =
    let
        activeCardId =
            model.viewState.active

        tree =
            model.workingTree.tree

        cardsTotal =
            (model.workingTree.tree
                |> TreeUtils.preorderTraversal
                |> List.length
            )
                -- Don't count hidden root
                - 1

        currentTree =
            getTree activeCardId tree
                |> Maybe.withDefault defaultTree

        currentGroup =
            getSiblings activeCardId tree

        cardCount =
            countWords currentTree.content

        subtreeCount =
            cardCount + countWords (treeToMarkdownString False currentTree)

        groupCount =
            currentGroup
                |> List.map .content
                |> String.join "\n\n"
                |> countWords

        columnCount =
            getColumn (getDepth 0 tree activeCardId) tree
                -- Maybe (List (List Tree))
                |> Maybe.withDefault [ [] ]
                |> List.concat
                |> List.map .content
                |> String.join "\n\n"
                |> countWords

        treeCount =
            countWords (treeToMarkdownString False tree)
    in
    Stats
        cardCount
        subtreeCount
        groupCount
        columnCount
        treeCount
        cardsTotal


countWords : String -> Int
countWords str =
    let
        punctuation =
            Regex.fromString "[!@#$%^&*():;\"',.]+"
                |> Maybe.withDefault Regex.never
    in
    str
        |> String.toLower
        |> replace punctuation (\_ -> "")
        |> String.words
        |> List.filter ((/=) "")
        |> List.length


viewConflict : (String -> Selection -> String -> msg) -> (String -> msg) -> Conflict -> Html msg
viewConflict setSelectionMsg resolveMsg { id, opA, opB, selection, resolved } =
    let
        withManual cardId oursElement theirsElement =
            li
                []
                [ fieldset []
                    [ radio (setSelectionMsg id Original cardId) (selection == Original) (text "Original")
                    , radio (setSelectionMsg id Ours cardId) (selection == Ours) oursElement
                    , radio (setSelectionMsg id Theirs cardId) (selection == Theirs) theirsElement
                    , radio (setSelectionMsg id Manual cardId) (selection == Manual) (text "Merged")
                    , label []
                        [ input [ checked resolved, type_ "checkbox", onClick (resolveMsg id) ] []
                        , text "Resolved"
                        ]
                    ]
                ]

        withoutManual cardIdA cardIdB =
            li
                []
                [ fieldset []
                    [ radio (setSelectionMsg id Original "") (selection == Original) (text "Original")
                    , radio (setSelectionMsg id Ours cardIdA) (selection == Ours) (text ("Ours:" ++ (opString opA |> String.left 3)))
                    , radio (setSelectionMsg id Theirs cardIdB) (selection == Theirs) (text ("Theirs:" ++ (opString opB |> String.left 3)))
                    , label []
                        [ input [ checked resolved, type_ "checkbox", onClick (resolveMsg id) ] []
                        , text "Resolved"
                        ]
                    ]
                ]

        newConflictView cardId ourChanges theirChanges =
            div [ class "flex-row" ]
                [ div [ class "conflict-container flex-column" ]
                    [ div
                        [ classList [ ( "row option", True ), ( "selected", selection == Original ) ]
                        , onClick (setSelectionMsg id Original cardId)
                        ]
                        [ text "Original" ]
                    , div [ class "row flex-row" ]
                        [ div
                            [ classList [ ( "option", True ), ( "selected", selection == Ours ) ]
                            , onClick (setSelectionMsg id Ours cardId)
                            ]
                            [ text "Ours"
                            , ul [ class "changelist" ] ourChanges
                            ]
                        , div
                            [ classList [ ( "option", True ), ( "selected", selection == Theirs ) ]
                            , onClick (setSelectionMsg id Theirs cardId)
                            ]
                            [ text "Theirs"
                            , ul [ class "changelist" ] theirChanges
                            ]
                        ]
                    , div
                        [ classList [ ( "row option", True ), ( "selected", selection == Manual ) ]
                        , onClick (setSelectionMsg id Manual cardId)
                        ]
                        [ text "Merged" ]
                    ]
                , button [ onClick (resolveMsg id) ] [ text "Resolved" ]
                ]
    in
    case ( opA, opB ) of
        ( Mod idA _ _ _, Mod _ _ _ _ ) ->
            let
                diffLinesString l r =
                    diffLines l r
                        |> List.filterMap
                            (\c ->
                                case c of
                                    NoChange s ->
                                        Nothing

                                    Added s ->
                                        Just (li [] [ ins [ class "diff" ] [ text s ] ])

                                    Removed s ->
                                        Just (li [] [ del [ class "diff" ] [ text s ] ])
                            )
            in
            newConflictView idA [] []

        ( Conflict.Ins idA _ _ _, Del idB _ ) ->
            withoutManual idA idB

        ( Del idA _, Conflict.Ins idB _ _ _ ) ->
            withoutManual idA idB

        _ ->
            withoutManual "" ""


radio : msg -> Bool -> Html msg -> Html msg
radio msg bool labelElement =
    label []
        [ input [ type_ "radio", checked bool, onClick msg ] []
        , labelElement
        ]


keyboardIconSvg w =
    svg [ version "1.1", viewBox "0 0 172 172", width w ] [ g [ fill "none", Svg.Attributes.fillRule "nonzero", stroke "none", strokeWidth "1", strokeLinecap "butt", strokeLinejoin "miter", strokeMiterlimit "10", strokeDasharray "", strokeDashoffset "0", fontFamily "none", fontWeight "none", fontSize "none", textAnchor "none", Svg.Attributes.style "mix-blend-mode: normal" ] [ Svg.path [ d "M0,172v-172h172v172z", fill "none" ] [], g [ id "original-icon", fill "#000000" ] [ Svg.path [ d "M16.125,32.25c-8.86035,0 -16.125,7.26465 -16.125,16.125v64.5c0,8.86035 7.26465,16.125 16.125,16.125h129c8.86035,0 16.125,-7.26465 16.125,-16.125v-64.5c0,-8.86035 -7.26465,-16.125 -16.125,-16.125zM16.125,43h129c3.02344,0 5.375,2.35156 5.375,5.375v64.5c0,3.02344 -2.35156,5.375 -5.375,5.375h-129c-3.02344,0 -5.375,-2.35156 -5.375,-5.375v-64.5c0,-3.02344 2.35156,-5.375 5.375,-5.375zM21.5,53.75v10.75h10.75v-10.75zM43,53.75v10.75h10.75v-10.75zM64.5,53.75v10.75h10.75v-10.75zM86,53.75v10.75h10.75v-10.75zM107.5,53.75v10.75h10.75v-10.75zM129,53.75v10.75h10.75v-10.75zM21.5,75.25v10.75h10.75v-10.75zM43,75.25v10.75h10.75v-10.75zM64.5,75.25v10.75h10.75v-10.75zM86,75.25v10.75h10.75v-10.75zM107.5,75.25v10.75h10.75v-10.75zM129,75.25v10.75h10.75v-10.75zM53.75,96.75v10.75h53.75v-10.75zM21.5,96.83399v10.79199h21.5v-10.79199zM118.41797,96.83399v10.79199h21.5v-10.79199z" ] [] ] ] ]


fillet posStr =
    svg [ Svg.Attributes.class "fillet", Svg.Attributes.class posStr, preserveAspectRatio "none", viewBox "0 0 30 30" ]
        [ g [] [ Svg.path [ d "M 30 0 A 30 30 0 0 1 0 30 L 30 30 L 30 0 z " ] [] ] ]
