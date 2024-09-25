module Doc.UI exposing (countWords, fillet, renderToast, viewAIButton, viewAIPrompt, viewAppLoadingSpinner, viewBreadcrumbs, viewDocumentLoadingSpinner, viewMobileButtons, viewSaveIndicator, viewSearchField, viewShortcuts, viewTemplateSelector, viewTooltip, viewWordCount)

import Ant.Icons.Svg as AntIcons
import Browser.Dom exposing (Element)
import Coders exposing (treeToMarkdownString)
import Doc.TreeStructure as TreeStructure exposing (defaultTree)
import Doc.TreeUtils as TreeUtils exposing (..)
import GlobalData exposing (GlobalData)
import Html exposing (Html, a, div, h2, h3, h5, hr, input, li, pre, span, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Html.Extra exposing (viewIf)
import Import.Template exposing (Template(..))
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer exposing (Renderer)
import Octicons as Icon exposing (defaultOptions)
import Regex exposing (Regex, replace)
import Route
import SharedUI exposing (ctrlOrCmdText, modalWrapper)
import Svg exposing (g, svg)
import Svg.Attributes exposing (d, fill, fontFamily, fontSize, fontWeight, preserveAspectRatio, stroke, strokeDasharray, strokeDashoffset, strokeLinecap, strokeLinejoin, strokeMiterlimit, strokeWidth, textAnchor, version, viewBox)
import Time exposing (posixToMillis)
import Toast
import Translation exposing (Language(..), TranslationId(..), timeDistInWords, tr)
import Types exposing (Children(..), CursorPosition(..), SortBy(..), TextCursorInfo, Toast, ToastRole(..), TooltipPosition(..), ViewMode(..), ViewState)
import UI.Sidebar exposing (viewSidebarStatic)
import Utils exposing (emptyText, text, textNoTr)


viewSaveIndicator :
    Language
    -> { m | dirty : Bool, lastLocalSave : Maybe Time.Posix, lastRemoteSave : Maybe Time.Posix }
    -> Time.Posix
    -> Html msg
viewSaveIndicator language { dirty, lastLocalSave, lastRemoteSave } currentTime =
    let
        timeDistPast t1 t2 =
            if Time.posixToMillis t1 < Time.posixToMillis t2 then
                timeDistInWords language t1 t2

            else
                timeDistInWords language t2 t1

        lastChangeString =
            timeDistPast
                (lastLocalSave |> Maybe.withDefault (Time.millisToPosix 0))
                currentTime

        lastSyncString =
            timeDistPast
                (lastRemoteSave |> Maybe.withDefault (Time.millisToPosix 0))
                currentTime

        ( saveStateSpan, saveStateIcon, name ) =
            if dirty then
                ( span [ title (tr language LastSaved ++ " " ++ lastChangeString) ] [ text language UnsavedChanges ]
                , AntIcons.infoCircleOutlined [ width 16, height 16 ]
                , "unsaved"
                )

            else
                case ( lastLocalSave, lastRemoteSave ) of
                    ( Nothing, Nothing ) ->
                        ( span [] [ text language Loading ]
                        , AntIcons.loading3QuartersOutlined [ width 16, height 16 ]
                        , "never-saved"
                        )

                    ( Just time, Nothing ) ->
                        if Time.posixToMillis time == 0 then
                            ( span [] [ text language Loading ]
                            , AntIcons.loading3QuartersOutlined [ width 16, height 16 ]
                            , "never-saved"
                            )

                        else
                            ( span [ title (tr language LastSynced ++ " " ++ lastSyncString) ] [ text language SavedInternally ]
                            , AntIcons.warningFilled [ width 16, height 16 ]
                            , "saved-offline"
                            )

                    ( Just commitTime, Just fileTime ) ->
                        if posixToMillis commitTime <= posixToMillis fileTime then
                            ( span [ title (tr language LastEdit ++ " " ++ lastChangeString) ]
                                [ text language ChangesSynced ]
                            , AntIcons.checkCircleFilled [ width 16, height 16 ]
                            , "synced"
                            )

                        else
                            ( span [ title (tr language LastSynced ++ " " ++ lastSyncString) ] [ text language SavedInternally ]
                            , AntIcons.warningFilled [ width 16, height 16 ]
                            , "saved-offline"
                            )

                    ( Nothing, Just _ ) ->
                        ( span [ title (tr language LastSynced ++ " " ++ lastSyncString) ] [ text language DatabaseError ]
                        , AntIcons.closeCircleFilled [ width 16, height 16 ]
                        , "database-error"
                        )
    in
    div
        [ id "save-indicator", classList [ ( "inset", True ), ( "saving", dirty ), ( name, True ) ] ]
        [ saveStateIcon, saveStateSpan ]


viewBreadcrumbs : (String -> msg) -> List ( String, String ) -> Html msg
viewBreadcrumbs clickedCrumbMsg cardIdsAndTitles =
    let
        defaultMarkdown =
            Markdown.Renderer.defaultHtmlRenderer

        firstElementOnly : a -> List a -> a
        firstElementOnly d l =
            List.head l |> Maybe.withDefault d

        markdownParser tag =
            Markdown.Html.tag tag (\rc -> List.head rc |> Maybe.withDefault emptyText)

        textRenderer : Renderer (Html msg)
        textRenderer =
            { defaultMarkdown
                | text = Html.text
                , codeSpan = Html.text
                , image = always emptyText
                , heading = \{ rawText } -> Html.text (String.trim rawText)
                , paragraph = firstElementOnly emptyText
                , blockQuote = firstElementOnly emptyText
                , orderedList = \i l -> List.map (firstElementOnly emptyText) l |> firstElementOnly emptyText
                , unorderedList =
                    \l ->
                        List.map
                            (\li ->
                                case li of
                                    Markdown.Block.ListItem _ children ->
                                        children |> firstElementOnly emptyText
                            )
                            l
                            |> firstElementOnly emptyText
                , html = Markdown.Html.oneOf ([ "p", "h1", "h2", "h3", "h4", "h5", "h6", "style", "code", "span", "pre" ] |> List.map markdownParser)
            }

        renderedContent : String -> List (Html msg)
        renderedContent content =
            content
                |> Markdown.Parser.parse
                |> Result.mapError deadEndsToString
                |> Result.andThen (\ast -> Markdown.Renderer.render textRenderer ast)
                |> Result.withDefault [ Html.text "<parse error>" ]

        deadEndsToString deadEnds =
            deadEnds
                |> List.map Markdown.Parser.deadEndToString
                |> String.join "\n"

        viewCrumb ( id, content ) =
            div [ onClick <| clickedCrumbMsg id ] [ span [] (renderedContent content) ]
    in
    div [ id "breadcrumbs", attribute "data-private" "lipsum" ] (List.map viewCrumb cardIdsAndTitles)



-- SIDEBAR


viewAppLoadingSpinner : Bool -> Html msg
viewAppLoadingSpinner sidebarOpen =
    div [ id "app-root", class "loading" ]
        ([ div [ id "document-header" ] []
         , div [ id "loading-overlay" ] []
         , div [ class "spinner" ] [ div [ class "bounce1" ] [], div [ class "bounce2" ] [], div [ class "bounce3" ] [] ]
         ]
            ++ viewSidebarStatic sidebarOpen
        )


viewDocumentLoadingSpinner : List (Html msg)
viewDocumentLoadingSpinner =
    [ div [ id "document-header" ] []
    , div [ id "loading-overlay" ] []
    , div [ class "spinner" ] [ div [ class "bounce1" ] [], div [ class "bounce2" ] [], div [ class "bounce3" ] [] ]
    ]



-- MODALS


viewTemplateSelector :
    Language
    ->
        { modalClosed : msg
        , importBulkClicked : msg
        , importTextClicked : msg
        , importOpmlRequested : msg
        , importJSONRequested : msg
        }
    -> List (Html msg)
viewTemplateSelector language msgs =
    [ div [ id "templates-block" ]
        [ h2 [] [ text language New ]
        , div [ class "template-row" ]
            [ a [ id "template-new", class "template-item", href (Route.toString Route.DocNew) ]
                [ div [ classList [ ( "template-thumbnail", True ), ( "new", True ) ] ] []
                , div [ class "template-title" ] [ text language HomeBlank ]
                ]
            ]
        , h2 [] [ text language ImportSectionTitle ]
        , div [ class "template-row" ]
            [ div [ id "template-import-text", class "template-item", onClick msgs.importTextClicked ]
                [ div [ classList [ ( "template-thumbnail", True ) ] ] [ Icon.file (Icon.defaultOptions |> Icon.size 48) ]
                , div [ class "template-title" ] [ text language ImportTextFiles ]
                , div [ class "template-description" ]
                    [ text language ImportTextFilesDesc ]
                ]
            , div [ id "template-import", class "template-item", onClick msgs.importJSONRequested ]
                [ div [ classList [ ( "template-thumbnail", True ) ] ] [ Icon.fileCode (Icon.defaultOptions |> Icon.size 48) ]
                , div [ class "template-title" ] [ text language HomeImportJSON ]
                , div [ class "template-description" ]
                    [ text language HomeJSONFrom ]
                ]
            , div [ id "template-import-opml", class "template-item", onClick msgs.importOpmlRequested ]
                [ div [ classList [ ( "template-thumbnail", True ) ] ] [ Icon.fileCode (Icon.defaultOptions |> Icon.size 48) ]
                , div [ class "template-title" ] [ text language ImportOpmlFiles ]
                , div [ class "template-description" ]
                    [ text language ImportOpmlFilesDesc ]
                ]
            ]
        , h2 [] [ text language TemplatesAndExamples ]
        , div [ class "template-row" ]
            [ a [ id "template-timeline", class "template-item", href <| Route.toString (Route.Import Timeline) ]
                [ div [ classList [ ( "template-thumbnail", True ) ] ] [ AntIcons.calendarOutlined [ width 48, height 48 ] ]
                , div [ class "template-title" ] [ text language TimelineTemplate ]
                , div [ class "template-description" ]
                    [ text language TimelineTemplateDesc ]
                ]
            , a [ id "template-academic", class "template-item", href <| Route.toString (Route.Import AcademicPaper) ]
                [ div [ classList [ ( "template-thumbnail", True ) ] ] [ AntIcons.experimentOutlined [ width 48, height 48 ] ]
                , div [ class "template-title" ] [ text language AcademicPaperTemplate ]
                , div [ class "template-description" ]
                    [ text language AcademicPaperTemplateDesc ]
                ]
            , a [ id "template-project", class "template-item", href <| Route.toString (Route.Import ProjectBrainstorming) ]
                [ div [ classList [ ( "template-thumbnail", True ) ] ] [ AntIcons.bulbOutlined [ width 48, height 48 ] ]
                , div [ class "template-title" ] [ text language ProjectBrainstormingTemplate ]
                , div [ class "template-description" ]
                    [ text language ProjectBrainstormingTemplateDesc ]
                ]
            , a [ id "template-heros-journey", class "template-item", href <| Route.toString (Route.Import HerosJourney) ]
                [ div [ classList [ ( "template-thumbnail", True ) ] ] [ AntIcons.thunderboltOutlined [ width 48, height 48 ] ]
                , div [ class "template-title" ] [ text language HerosJourneyTemplate ]
                , div [ class "template-description" ]
                    [ text language HerosJourneyTemplateDesc ]
                ]
            ]
        ]
    ]
        |> modalWrapper msgs.modalClosed Nothing Nothing (tr language NewDocument)


viewWordCount :
    { activeCardId : String
    , workingTree : TreeStructure.Model
    , startingWordcount : Int
    , globalData : GlobalData
    }
    -> { modalClosed : msg }
    -> List (Html msg)
viewWordCount model msgs =
    let
        language =
            GlobalData.language model.globalData

        stats =
            getStats model

        current =
            stats.documentWords

        session =
            current - model.startingWordcount
    in
    [ div [ id "word-count-table" ]
        [ div [ class "word-count-column" ]
            [ span [] [ text language (WordCountCard stats.cardWords) ]
            , span [] [ text language (WordCountSubtree stats.subtreeWords) ]
            , span [] [ text language (WordCountGroup stats.groupWords) ]
            , span [] [ text language (WordCountColumn stats.columnWords) ]
            , span [] [ text language (WordCountSession session) ]
            , span [] [ text language (WordCountTotal current) ]
            ]
        , div [ class "word-count-column" ]
            [ span [] [ text language (CharacterCountCard stats.cardChars) ]
            , span [] [ text language (CharacterCountSubtree stats.subtreeChars) ]
            , span [] [ text language (CharacterCountGroup stats.groupChars) ]
            , span [] [ text language (CharacterCountColumn stats.columnChars) ]
            , span [] [ text language (CharacterCountTotal stats.documentChars) ]
            ]
        ]
    , span [ style "text-align" "center" ] [ text language (WordCountTotalCards stats.cards) ]
    ]
        |> modalWrapper msgs.modalClosed Nothing Nothing "Word & Character Counts"



-- DOCUMENT


viewSearchField : (String -> msg) -> { m | viewState : ViewState, globalData : GlobalData } -> Html msg
viewSearchField searchFieldMsg { viewState, globalData } =
    let
        language =
            GlobalData.language globalData

        maybeSearchIcon =
            if viewState.searchField == Nothing then
                Icon.search (defaultOptions |> Icon.color "#445" |> Icon.size 12)

            else
                emptyText
    in
    case viewState.viewMode of
        Normal _ ->
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


viewShortcuts :
    { toggledShortcutTray : msg, tooltipRequested : String -> TooltipPosition -> TranslationId -> msg, tooltipClosed : msg }
    ->
        { lang : Language
        , isOpen : Bool
        , isMac : Bool
        , aiFeaturesEnabled : Bool
        , isAIPromptOpen : Bool
        , children : Children
        , textCursorInfo : TextCursorInfo
        , viewMode : ViewMode
        }
    -> List (Html msg)
viewShortcuts msgs { lang, isOpen, isMac, aiFeaturesEnabled, isAIPromptOpen, children, textCursorInfo, viewMode } =
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
                emptyText

        addInsteadOfSplit =
            textCursorInfo.position == End || textCursorInfo.position == Empty

        spanSplit key descAdd descSplit =
            if addInsteadOfSplit then
                shortcutSpan [ NoTr ctrlOrCmd, NoTr key ] descAdd

            else
                shortcutSpan [ NoTr ctrlOrCmd, NoTr key ] descSplit

        splitChild =
            spanSplit "L" AddChildAction SplitChildAction

        splitBelow =
            spanSplit "J" AddBelowAction SplitBelowAction

        splitAbove =
            spanSplit "K" AddAboveAction SplitUpwardAction

        shortcutSpanEnabled enabled keys desc =
            let
                keySpans =
                    keys
                        |> List.map (\k -> span [ class "shortcut-key" ] [ text lang k ])
            in
            span
                [ classList [ ( "disabled", not enabled ) ] ]
                (keySpans
                    ++ [ textNoTr (" " ++ tr lang desc) ]
                )

        shortcutSpan =
            shortcutSpanEnabled True

        formattingSpan markup =
            span [] [ pre [ class "formatting-text" ] [ text lang markup ] ]

        ctrlOrCmd =
            ctrlOrCmdText isMac
    in
    if isOpen then
        let
            iconColor =
                Icon.color "#445"
        in
        case viewMode of
            Normal _ ->
                [ div
                    [ id "shortcuts-tray", classList [ ( "open", isOpen ) ], onClick msgs.toggledShortcutTray ]
                    [ div [ id "shortcuts" ]
                        ([ h3 [] [ text lang KeyboardShortcuts ]
                         , h5 [] [ text lang EditCards ]
                         , shortcutSpan [ EnterKey ] EnterAction
                         , shortcutSpan [ ShiftKey, EnterKey ] EditFullscreenAction
                         , viewIfNotOnly <| h5 [] [ text lang Navigate ]
                         , viewIfNotOnly <| shortcutSpan [ NoTr "↑", NoTr "↓", NoTr "←", NoTr "→" ] ArrowsAction
                         , h5 [] [ text lang AddNewCards ]
                         , shortcutSpan [ NoTr ctrlOrCmd, NoTr "→" ] AddChildAction
                         , shortcutSpan [ NoTr ctrlOrCmd, NoTr "↓" ] AddBelowAction
                         , shortcutSpan [ NoTr ctrlOrCmd, NoTr "↑" ] AddAboveAction
                         ]
                            ++ (if aiFeaturesEnabled then
                                    [ h5 [] [ text lang AIFeatures ]
                                    , shortcutSpan [ AltKey, NoTr "I" ] ToOpenAIPrompt
                                    , viewIf isAIPromptOpen <| shortcutSpan [ NoTr ctrlOrCmd, NoTr "J" ] AIGenerateBelow
                                    , viewIf isAIPromptOpen <| shortcutSpan [ NoTr ctrlOrCmd, NoTr "L" ] AIGenerateChildren
                                    ]

                                else
                                    []
                               )
                            ++ [ viewIfNotOnly <| h5 [] [ text lang MoveAndDelete ]
                               , viewIfNotOnly <| shortcutSpan [ AltKey, ArrowKeys ] MoveAction
                               , viewIfNotOnly <| shortcutSpan [ NoTr ctrlOrCmd, Backspace ] DeleteAction
                               , viewIfNotOnly <| h5 [] [ text lang MergeCards ]
                               , viewIfNotOnly <| shortcutSpan [ NoTr ctrlOrCmd, ShiftKey, NoTr "↓" ] MergeDownAction
                               , viewIfNotOnly <| shortcutSpan [ NoTr ctrlOrCmd, ShiftKey, NoTr "↑" ] MergeUpAction
                               , hr [] []
                               , h5 [] [ text lang OtherShortcuts ]
                               , shortcutSpan [ NoTr "w" ] DisplayWordCounts
                               , shortcutSpan [ NoTr ctrlOrCmd, NoTr "O" ] QuickDocumentSwitcher
                               ]
                        )
                    ]
                ]

            _ ->
                [ div
                    [ id "shortcuts-tray", classList [ ( "open", isOpen ) ], onClick msgs.toggledShortcutTray ]
                    [ div [ id "shortcuts" ]
                        [ h3 [] [ text lang KeyboardShortcuts ]
                        , h3 [] [ text lang EditMode ]
                        , h5 [] [ text lang SaveOrCancelChanges ]
                        , shortcutSpan [ NoTr ctrlOrCmd, EnterKey ] ToSaveChanges
                        , shortcutSpan [ EscKey ] ToCancelChanges
                        , if addInsteadOfSplit then
                            h5 [] [ text lang AddNewCards ]

                          else
                            h5 [] [ text lang SplitAtCursor ]
                        , splitChild
                        , splitBelow
                        , splitAbove
                        , h5 [] [ text lang Formatting ]
                        , shortcutSpanEnabled isTextSelected [ NoTr ctrlOrCmd, NoTr "B" ] ForBold
                        , shortcutSpanEnabled isTextSelected [ NoTr ctrlOrCmd, NoTr "I" ] ForItalic
                        , shortcutSpan [ AltKey, ParenNumber ] SetHeadingLevel
                        , formattingSpan FormattingTitle
                        , formattingSpan FormattingList
                        , formattingSpan FormattingLink
                        , span [ class "markdown-guide" ]
                            [ a [ href "http://commonmark.org/help", target "_blank" ]
                                [ text lang FormattingGuide
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
            , onMouseEnter <| msgs.tooltipRequested "shortcuts-tray" LeftTooltip KeyboardShortcuts
            , onMouseLeave msgs.tooltipClosed
            ]
            [ keyboardIconSvg 24 ]
        ]



-- Word count


type alias Stats =
    { cardWords : Int
    , cardChars : Int
    , subtreeWords : Int
    , subtreeChars : Int
    , groupWords : Int
    , groupChars : Int
    , columnWords : Int
    , columnChars : Int
    , documentWords : Int
    , documentChars : Int
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


toastConfig : (Toast.Msg -> msg) -> Toast.Config msg
toastConfig msg =
    Toast.config msg
        |> Toast.withTrayAttributes [ class "flex flex-column gap-4 fixed top-14 right-4 z-10" ]
        |> Toast.withTransitionAttributes [ class "translate-x-96 opacity-0" ]


viewToast : List (Html.Attribute msg) -> Toast.Info Toast -> Html msg
viewToast toastAttr toast =
    let
        roleClass =
            case toast.content.role of
                Info ->
                    [ class "bg-blue-400" ]

                Warning ->
                    [ class "bg-yellow-400" ]

                Error ->
                    [ class "bg-red-400" ]

                SuccessToast ->
                    [ class "bg-green-400" ]

        deadEndsToString deadEnds =
            deadEnds
                |> List.map Markdown.Parser.deadEndToString
                |> String.join "\n"

        toastRenderedMarkdown =
            toast.content.message
                |> Markdown.Parser.parse
                |> Result.mapError deadEndsToString
                |> Result.andThen (\ast -> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer ast)
                |> Result.withDefault [ Html.text "<parse error>" ]

        sharedClasses =
            [ class "rounded-lg max-w-xs p-6 py-4 drop-shadow-lg transition duration-500" ]
    in
    div
        (toastAttr ++ roleClass ++ sharedClasses)
        toastRenderedMarkdown


viewToastFrame : List (Html.Attribute msg) -> Toast.Info Toast -> Html msg
viewToastFrame toastAttrs toast =
    div
        []
        [ viewToast toastAttrs toast ]


renderToast : (Toast.Msg -> msg) -> Toast.Tray Toast -> Html msg
renderToast msg tray =
    Toast.render viewToastFrame tray (toastConfig msg)


viewTooltip : Language -> ( Element, TooltipPosition, TranslationId ) -> Html msg
viewTooltip lang ( el, tipPos, content ) =
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
                    , style "transform" "translateX(calc(-100% + 10px))"
                    , class "tip-below-left"
                    ]
    in
    div ([ class "tooltip" ] ++ posAttributes)
        [ text lang content, div [ class "tooltip-arrow" ] [] ]


viewAIPrompt : String -> Bool -> (String -> msg) -> Html msg
viewAIPrompt ctrlOrCmd isWaiting promptInputMsg =
    let
        shortCutKey k =
            span [ class "shortcut-key" ] [ textNoTr k ]

        shortCut keys str =
            div [ class "ai-prompt-instruction" ]
                (List.map shortCutKey keys
                    ++ [ textNoTr str ]
                )
    in
    div [ id "ai-prompt-container" ]
        [ div [ id "ai-prompt" ]
            [ textarea
                [ id "ai-prompt-textarea"
                , disabled isWaiting
                , classList [ ( "mousetrap", True ) ]
                , onInput promptInputMsg
                ]
                []
            , if not isWaiting then
                div [ class "ai-prompt-instructions" ]
                    [ shortCut [ "Esc" ] " Cancel"
                    , shortCut [ ctrlOrCmd, "J" ] " Generate Below"
                    , shortCut [ ctrlOrCmd, "L" ] " Generate Children"
                    ]

              else
                div [ class "switcher-instructions" ]
                    [ div [ class "switcher-instruction" ] [ textNoTr "Generating..." ]
                    ]
            ]
        ]


viewAIButton : msg -> Html msg
viewAIButton aiPromptMsg =
    div [ id "ai-prompt-button", onClick aiPromptMsg ]
        [ AntIcons.robotOutlined [ width 12, height 12 ] ]


getStats : { m | activeCardId : String, workingTree : TreeStructure.Model } -> Stats
getStats { activeCardId, workingTree } =
    let
        tree =
            workingTree.tree

        cardsTotal =
            (workingTree.tree
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

        ( cardWords, cardChars ) =
            count currentTree.content

        ( subtreeWords, subtreeChars ) =
            count (treeToMarkdownString False currentTree)
                |> Tuple.mapBoth (\w -> w + cardWords) (\c -> c + cardChars)

        ( groupWords, groupChars ) =
            currentGroup
                |> List.map .content
                |> String.join "\n\n"
                |> count

        ( columnWords, columnChars ) =
            getColumn (getDepth 0 tree activeCardId) tree
                -- Maybe (List (List Tree))
                |> Maybe.withDefault [ [] ]
                |> List.concat
                |> List.map .content
                |> String.join "\n\n"
                |> count

        ( treeWords, treeChars ) =
            count (treeToMarkdownString False tree)
    in
    Stats
        cardWords
        cardChars
        subtreeWords
        subtreeChars
        groupWords
        groupChars
        columnWords
        columnChars
        treeWords
        treeChars
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


count : String -> ( Int, Int )
count str =
    let
        punctuation =
            Regex.fromString "[!@#$%^&*():;\"',.]+"
                |> Maybe.withDefault Regex.never

        wordCounts =
            str
                |> String.toLower
                |> replace punctuation (\_ -> "")
                |> String.words
                |> List.filter ((/=) "")
                |> List.length

        charCounts =
            str
                |> String.toList
                |> List.length
    in
    ( wordCounts, charCounts )


keyboardIconSvg w =
    svg [ version "1.1", viewBox "0 0 172 172", width w ] [ g [ fill "none", Svg.Attributes.fillRule "nonzero", stroke "none", strokeWidth "1", strokeLinecap "butt", strokeLinejoin "miter", strokeMiterlimit "10", strokeDasharray "", strokeDashoffset "0", fontFamily "none", fontWeight "none", fontSize "none", textAnchor "none", Svg.Attributes.style "mix-blend-mode: normal" ] [ Svg.path [ d "M0,172v-172h172v172z", fill "none" ] [], g [ id "original-icon", fill "#000000" ] [ Svg.path [ d "M16.125,32.25c-8.86035,0 -16.125,7.26465 -16.125,16.125v64.5c0,8.86035 7.26465,16.125 16.125,16.125h129c8.86035,0 16.125,-7.26465 16.125,-16.125v-64.5c0,-8.86035 -7.26465,-16.125 -16.125,-16.125zM16.125,43h129c3.02344,0 5.375,2.35156 5.375,5.375v64.5c0,3.02344 -2.35156,5.375 -5.375,5.375h-129c-3.02344,0 -5.375,-2.35156 -5.375,-5.375v-64.5c0,-3.02344 2.35156,-5.375 5.375,-5.375zM21.5,53.75v10.75h10.75v-10.75zM43,53.75v10.75h10.75v-10.75zM64.5,53.75v10.75h10.75v-10.75zM86,53.75v10.75h10.75v-10.75zM107.5,53.75v10.75h10.75v-10.75zM129,53.75v10.75h10.75v-10.75zM21.5,75.25v10.75h10.75v-10.75zM43,75.25v10.75h10.75v-10.75zM64.5,75.25v10.75h10.75v-10.75zM86,75.25v10.75h10.75v-10.75zM107.5,75.25v10.75h10.75v-10.75zM129,75.25v10.75h10.75v-10.75zM53.75,96.75v10.75h53.75v-10.75zM21.5,96.83399v10.79199h21.5v-10.79199zM118.41797,96.83399v10.79199h21.5v-10.79199z" ] [] ] ] ]


fillet posStr =
    svg [ Svg.Attributes.class "fillet", Svg.Attributes.class posStr, preserveAspectRatio "none", viewBox "0 0 30 30" ]
        [ g [] [ Svg.path [ d "M 30 0 A 30 30 0 0 1 0 30 L 30 30 L 30 0 z " ] [] ] ]
