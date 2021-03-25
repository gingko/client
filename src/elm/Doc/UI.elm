module Doc.UI exposing (countWords, viewConflict, viewFooter, viewHeader, viewHistory, viewHomeLink, viewLoadingSpinner, viewMobileButtons, viewSaveIndicator, viewSearchField, viewShortcuts, viewSidebar, viewSidebarStatic, viewTemplateSelector, viewVideo, viewWordCount)

import Ant.Icons.Svg as AntIcons
import Coders exposing (treeToMarkdownString)
import Diff exposing (..)
import Doc.Data as Data
import Doc.Data.Conflict as Conflict exposing (Conflict, Op(..), Selection(..), opString)
import Doc.List as DocList
import Doc.Metadata as Metadata exposing (Metadata)
import Doc.TreeStructure as TreeStructure exposing (defaultTree)
import Doc.TreeUtils as TreeUtils exposing (..)
import Html exposing (Html, a, br, button, del, div, fieldset, h1, h2, h3, h4, h5, hr, iframe, img, input, ins, label, li, option, pre, select, small, span, text, ul)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onBlur, onCheck, onClick, onInput, onSubmit)
import Html.Events.Extra exposing (onChange)
import Import.Template exposing (Template(..))
import List.Extra as ListExtra exposing (getAt)
import Octicons as Icon exposing (defaultOptions)
import Page.Doc.Export exposing (ExportFormat(..), ExportSelection(..))
import Page.Doc.Theme exposing (Theme(..))
import Regex exposing (Regex, replace)
import Route
import Session exposing (PaymentStatus(..), Session)
import SharedUI exposing (modalWrapper)
import Time exposing (posixToMillis)
import Translation exposing (Language(..), TranslationId(..), langFromString, langToString, languageName, timeDistInWords, tr)
import Types exposing (Children(..), CursorPosition(..), DropdownState(..), SidebarState(..), TextCursorInfo, ViewMode(..), ViewState)



-- HEADER


viewHomeLink : msg -> Bool -> Html msg
viewHomeLink toggleSidebar sidebarOpen =
    div [ id "brand", onClick toggleSidebar ]
        [ img [ src "../gingko-leaf-logo.svg", width 28 ]
            []
        , if sidebarOpen then
            h2 [ id "brand-name" ] [ text "Gingko Writer" ]

          else
            text ""
        ]


type alias HeaderMsgs msg =
    { toggledTitleEdit : Bool -> msg
    , titleFieldChanged : String -> msg
    , titleEdited : msg
    , titleEditCanceled : msg
    , toggledHelpMenu : Bool -> msg
    , clickedEmailSupport : msg
    , logoutRequested : msg
    , toggledAccountMenu : Bool -> msg
    , toggledUpgradeModal : Bool -> msg
    }


viewHeader :
    HeaderMsgs msg
    -> Maybe String
    ->
        { m
            | titleField : Maybe String
            , dropdownState : DropdownState
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

        titleArea =
            case model.titleField of
                Just editingField ->
                    span [ id "title" ]
                        [ Html.form
                            [ onSubmit msgs.titleEdited ]
                            [ input
                                [ id "title-rename"
                                , onInput msgs.titleFieldChanged
                                , onBlur msgs.titleEdited
                                , value editingField
                                , attribute "data-private" "lipsum"
                                ]
                                []
                            , button [ type_ "submit" ] [ text "Rename" ]
                            , button [ onClick msgs.titleEditCanceled ] [ text "Cancel" ]
                            ]
                        ]

                Nothing ->
                    span [ id "title" ]
                        [ h1 [ onClick (msgs.toggledTitleEdit True), attribute "data-private" "lipsum" ]
                            [ text (title_ |> Maybe.withDefault "Untitled")
                            ]
                        , viewSaveIndicator language model (Session.currentTime model.session)
                        ]
    in
    div [ id "document-header" ]
        [ titleArea
        , viewTopRightButtons
            { toggledHelpMenu = msgs.toggledHelpMenu
            , clickedEmailSupport = msgs.clickedEmailSupport
            , logoutRequested = msgs.logoutRequested
            , toggledAccountMenu = msgs.toggledAccountMenu
            , toggledUpgradeModal = msgs.toggledUpgradeModal
            }
            model.dropdownState
            model.session
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


viewTopRightButtons :
    { toggledHelpMenu : Bool -> msg
    , clickedEmailSupport : msg
    , logoutRequested : msg
    , toggledAccountMenu : Bool -> msg
    , toggledUpgradeModal : Bool -> msg
    }
    -> DropdownState
    -> Session
    -> Html msg
viewTopRightButtons msgs dropdownState session =
    let
        currentTime =
            Session.currentTime session

        lang =
            Session.language session

        upgradeButton =
            div [ id "upgrade-button", onClick <| msgs.toggledUpgradeModal True ] [ text "Upgrade" ]

        maybeUpgrade =
            case Session.paymentStatus session of
                Customer _ ->
                    [ text "" ]

                Trial expiry ->
                    let
                        daysLeft =
                            ((Time.posixToMillis expiry - Time.posixToMillis currentTime) |> toFloat)
                                / (1000 * 3600 * 24)
                                |> round

                        trialClass =
                            if daysLeft <= 14 && daysLeft > 7 then
                                "trial-light"

                            else if daysLeft <= 7 && daysLeft > 3 then
                                "trial-medium"

                            else
                                "trial-dark"
                    in
                    if daysLeft <= 30 then
                        [ span [ class "trial", class trialClass, onClick <| msgs.toggledUpgradeModal True ] [ text (String.fromInt daysLeft ++ " days left in Free Trial") ], upgradeButton ]

                    else
                        [ upgradeButton ]

                Unknown ->
                    [ upgradeButton ]

        isHelpDropdown =
            dropdownState == Help

        isAccountDropdown =
            dropdownState == Account

        helpIcon =
            Icon.question (defaultOptions |> Icon.color "#333" |> Icon.size 18)

        userIcon =
            Icon.person (defaultOptions |> Icon.color "#333" |> Icon.size 18)

        logoutIcon =
            Icon.signOut (defaultOptions |> Icon.color "#333" |> Icon.size 18)
    in
    div [ id "top-right-buttons" ]
        (maybeUpgrade
            ++ [ div [ id "help-icon", onClick (msgs.toggledHelpMenu (not isHelpDropdown)) ]
                    [ helpIcon
                    , div [ id "welcome-step-6", class "tour-step" ]
                        [ text "Click to see Help Menu"
                        , div [ class "arrow" ] [ text "▲" ]
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
                    , if isHelpDropdown then
                        ul [ id "help-dropdown", class "dropdown" ]
                            [ li [] [ a [ href "https://docs.gingkowriter.com", target "_blank" ] [ text "FAQ" ] ]
                            , li [] [ span [ id "email-support", onClick msgs.clickedEmailSupport ] [ text <| tr lang EmailSupport ] ]
                            ]

                      else
                        text ""
                    ]
               , div [ id "account", onClick (msgs.toggledAccountMenu (not isAccountDropdown)) ]
                    [ userIcon
                    , if isAccountDropdown then
                        ul [ id "account-dropdown", class "dropdown" ]
                            [ li [] [ span [ class "no-interaction" ] [ text (Session.name session |> Maybe.withDefault "") ] ]
                            , li [] [ span [ id "logout-button", onClick msgs.logoutRequested ] [ logoutIcon, text <| tr lang Logout ] ]
                            ]

                      else
                        text ""
                    ]
               ]
        )



-- SIDEBAR


type alias SidebarMsgs msg =
    { sidebarStateChanged : SidebarState -> msg
    , templateSelectorOpened : msg
    , fileSearchChanged : String -> msg
    , contextMenuOpened : String -> ( Float, Float ) -> msg
    , exportPreviewToggled : Bool -> msg
    , exportSelectionChanged : ExportSelection -> msg
    , exportFormatChanged : ExportFormat -> msg
    , export : msg
    , importJSONRequested : msg
    , languageChanged : String -> msg
    , themeChanged : Theme -> msg
    , fullscreenRequested : msg
    }


viewSidebar : Language -> SidebarMsgs msg -> Metadata -> String -> DocList.Model -> ( ExportSelection, ExportFormat ) -> SidebarState -> List (Html msg)
viewSidebar modelLanguage msgs currentDocument fileFilter docList ( exportSelection, exportFormat ) sidebarState =
    let
        isOpen =
            not (sidebarState == SidebarClosed)

        exportSelectionRadio selection domId labelText =
            [ input [ id domId, type_ "radio", onInput (always <| msgs.exportSelectionChanged selection), checked (exportSelection == selection) ] []
            , label [ for domId ] [ text labelText ]
            ]

        exportFormatRadio selection domId labelText =
            [ input [ id domId, type_ "radio", onInput (always <| msgs.exportFormatChanged selection), checked (exportFormat == selection) ] []
            , label [ for domId ] [ text labelText ]
            ]

        sidebarMenu =
            case sidebarState of
                File ->
                    let
                        filteredList =
                            DocList.filter fileFilter docList
                    in
                    div [ id "sidebar-menu" ]
                        [ h3 [] [ text "File" ]
                        , button
                            [ id "new-button", onClick msgs.templateSelectorOpened, class "sidebar-item" ]
                            [ text "New" ]
                        , div [ id "welcome-step-7", class "tour-step" ]
                            [ text "Click here for New Document"
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
                        , hr [ style "width" "80%" ] []
                        , input [ type_ "search", onInput msgs.fileSearchChanged ] []
                        , DocList.viewSmall msgs.contextMenuOpened currentDocument filteredList
                        ]

                Export ->
                    div [ id "sidebar-menu" ]
                        [ h3 [] [ text "Export" ]
                        , label [] [ text "Toggle export preview", input [ type_ "checkbox", id "export-preview-checkbox", onCheck msgs.exportPreviewToggled ] [] ]
                        , hr [] []
                        , div [ id "export-selection" ]
                            (exportSelectionRadio ExportEverything "export-everything" "Whole tree"
                                ++ [ br [] [] ]
                                ++ exportSelectionRadio ExportSubtree "export-subtree" "Current card & Subtree"
                                ++ [ br [] [] ]
                                ++ exportSelectionRadio ExportCurrentColumn "export-current-column" "Current column"
                            )
                        , hr [] []
                        , div [ id "export-selection" ]
                            (exportFormatRadio DOCX "export-word" "Word format"
                                ++ [ br [] [] ]
                                ++ exportFormatRadio PlainText "export-plain" "Plain text"
                                ++ [ br [] [] ]
                                ++ exportFormatRadio JSON "export-json" "JSON format"
                            )
                        , button [ onClick msgs.export, class "sidebar-item" ] [ text "Export" ]
                        ]

                Import ->
                    div [ id "sidebar-menu" ]
                        [ h3 [] [ text "Import" ]
                        , button [ onClick msgs.importJSONRequested ] [ text "Import JSON" ]
                        ]

                Settings ->
                    div [ id "sidebar-menu" ]
                        [ h2 [] [ text "Settings" ]
                        , h5 [] [ text <| tr modelLanguage Language ]
                        , select [ onChange msgs.languageChanged ]
                            (Translation.activeLanguages
                                |> List.map
                                    (\( lang, langName ) ->
                                        option [ value <| langToString lang, selected (lang == modelLanguage) ] [ text langName ]
                                    )
                            )
                        , small []
                            [ text <| "(" ++ tr modelLanguage ContributeTranslations ++ " "
                            , a [ href "https://poeditor.com/join/project?hash=k8Br3k0JVz" ] [ text <| tr modelLanguage Here ]
                            , text ")"
                            ]
                        , br [] []
                        , h5 [] [ text "Themes" ]
                        , button [ onClick <| msgs.themeChanged Default ] [ text "Set Default" ]
                        , button [ onClick <| msgs.themeChanged Dark ] [ text "Set Dark Mode" ]
                        , button [ onClick <| msgs.themeChanged Gray ] [ text "Set Gray" ]
                        , button [ onClick <| msgs.themeChanged Turquoise ] [ text "Set Turquoise" ]
                        ]

                SidebarClosed ->
                    text ""

        fileIconColor =
            if isOpen then
                "hsl(202 22% 44%)"

            else
                "hsl(202 22% 66%)"

        toggle menu =
            if sidebarState == menu then
                msgs.sidebarStateChanged <| SidebarClosed

            else
                msgs.sidebarStateChanged <| menu

        sidebarButton menu =
            case menu of
                File ->
                    div
                        [ id "file-button"
                        , title "File"
                        , classList [ ( "sidebar-button", True ), ( "open", sidebarState == menu ) ]
                        , onClick <| toggle menu
                        ]
                        [ Icon.fileDirectory (defaultOptions |> Icon.color fileIconColor |> Icon.size 18) ]

                SidebarClosed ->
                    div
                        [ classList [ ( "sidebar-button", True ), ( "open", sidebarState == menu ) ], onClick <| toggle menu ]
                        []

                Export ->
                    div
                        [ id "export-button"
                        , title "Export"
                        , classList [ ( "sidebar-button", True ), ( "open", sidebarState == menu ) ]
                        , onClick <| toggle menu
                        ]
                        [ Icon.signOut (defaultOptions |> Icon.color fileIconColor |> Icon.size 18) ]

                Import ->
                    div
                        [ classList [ ( "sidebar-button", True ), ( "open", sidebarState == menu ) ], onClick <| toggle menu ]
                        [ Icon.signIn (defaultOptions |> Icon.color fileIconColor |> Icon.size 18) ]

                Settings ->
                    div
                        [ id "settings-button"
                        , title "Settings"
                        , classList [ ( "sidebar-button", True ), ( "open", sidebarState == menu ) ]
                        , onClick <| toggle menu
                        ]
                        [ Icon.settings (defaultOptions |> Icon.color fileIconColor |> Icon.size 18) ]
    in
    [ div [ id "sidebar", classList [ ( "open", isOpen ) ] ]
        [ sidebarButton File
        , sidebarButton Export

        --, sidebarButton Import importIcon -- TODO: Removed temporarily
        , sidebarButton Settings
        , div
            [ id "fullscreen-button"
            , title "Fullscreen"
            , class "sidebar-button"
            , onClick msgs.fullscreenRequested
            ]
            [ Icon.screenFull (defaultOptions |> Icon.size 18) ]
        ]
    , sidebarMenu
    ]


viewSidebarStatic : Bool -> List (Html msg)
viewSidebarStatic sidebarOpen =
    [ div [ id "sidebar", classList [ ( "open", sidebarOpen ) ] ]
        [ div [ classList [ ( "sidebar-button", True ) ] ] [ text " " ]
        ]
    , if sidebarOpen then
        div [ id "sidebar-menu" ]
            [ h3 [] [ text "File" ]
            , a [ href (Route.toString Route.DocNew), class "sidebar-item" ] [ text "New" ]
            , hr [ style "width" "80%" ] []
            ]

      else
        text ""
    ]


viewLoadingSpinner : msg -> Bool -> Html msg
viewLoadingSpinner toggleSidebarMsg sidebarOpen =
    div [ id "app-root", class "loading" ]
        ([ viewHomeLink toggleSidebarMsg False
         , div [ id "document-header" ] []
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


viewFooter : msg -> Html msg
viewFooter wordCountToggle =
    div
        [ class "footer" ]
        [ div [ id "wordcount", onClick wordCountToggle ] [ text "Word Counts" ] ]


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


viewHistory : msg -> (String -> msg) -> msg -> msg -> Translation.Language -> String -> Data.Model -> Html msg
viewHistory noopMsg checkoutMsg restoreMsg cancelMsg lang currHead dataModel =
    let
        master =
            Data.head "heads/master" dataModel

        historyList =
            Data.historyList currHead dataModel

        maxIdx =
            historyList
                |> List.length
                |> (\x -> x - 1)
                |> String.fromInt

        currIdx =
            historyList
                |> ListExtra.elemIndex currHead
                |> Maybe.map String.fromInt
                |> Maybe.withDefault maxIdx

        checkoutCommit idxStr =
            case String.toInt idxStr of
                Just idx ->
                    case getAt idx historyList of
                        Just commit ->
                            checkoutMsg commit

                        Nothing ->
                            noopMsg

                Nothing ->
                    noopMsg
    in
    div [ id "history" ]
        [ input [ type_ "range", A.min "0", A.max maxIdx, step "1", onInput checkoutCommit ] []
        , button [ id "history-restore", onClick restoreMsg ] [ text <| tr lang RestoreThisVersion ]
        , button [ onClick cancelMsg ] [ text <| tr lang Cancel ]
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


viewShortcuts : msg -> Language -> Bool -> Bool -> Children -> TextCursorInfo -> ViewState -> List (Html msg)
viewShortcuts trayToggleMsg lang isOpen isMac children textCursorInfo vs =
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

        tourTooltip str =
            div [ id "welcome-step-5", class "tour-step" ]
                [ text "Shortcuts List"
                , div [ class "arrow" ] [ text "▶" ]
                , div [ id "progress-step-5", class "tour-step-progress" ]
                    [ div [ class "bg-line", class "on" ] []
                    , div [ class "bg-line", class "off" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [ class "on" ] []
                    , div [] []
                    , div [] []
                    ]
                ]
    in
    if isOpen then
        let
            iconColor =
                Icon.color "#445"
        in
        case vs.viewMode of
            Normal ->
                [ div
                    [ id "shortcuts-tray", classList [ ( "open", isOpen ) ], onClick trayToggleMsg ]
                    [ div [ id "shortcuts" ]
                        [ h3 [] [ text "Keyboard Shortcuts", tourTooltip "Shortcuts List" ]
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
                    [ id "shortcuts-tray", classList [ ( "open", isOpen ) ], onClick trayToggleMsg ]
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
        let
            iconColor =
                Icon.color "#6c7c84"
        in
        [ div
            [ id "shortcuts-tray", onClick trayToggleMsg, title <| tr lang KeyboardHelp ]
            [ div [ classList [ ( "icon-stack", True ), ( "open", isOpen ) ] ]
                [ Icon.keyboard (defaultOptions |> iconColor) ]
            ]
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
