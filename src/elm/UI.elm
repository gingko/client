module UI exposing (countWords, viewFooter, viewSaveIndicator, viewSearchField, viewVideo)

import Coders exposing (treeToMarkdownString)
import Date
import Dict
import Diff exposing (..)
import Html exposing (Html, a, button, del, div, fieldset, h1, iframe, input, ins, label, li, span, text, ul)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra as ListExtra exposing (getAt)
import Objects
import Octicons as Icon exposing (defaultOptions)
import Regex exposing (Regex, replace)
import Time
import Translation exposing (Language, TranslationId(..), timeDistInWords, tr)
import TreeUtils exposing (..)
import Trees exposing (defaultTree)
import Types exposing (..)


viewSaveIndicator : { m | docState : DocState, dirty : Bool, currentTime : Time.Posix, language : Translation.Language } -> Html Msg
viewSaveIndicator { docState, dirty, currentTime, language } =
    let
        saveStateSpan =
            case docState of
                FileDoc (SavedDoc { lastSaved }) ->
                    let
                        lastChangeString =
                            timeDistInWords
                                language
                                lastSaved
                                currentTime
                    in
                    if dirty then
                        span [ title (tr language LastSaved ++ " " ++ lastChangeString) ] [ text <| tr language UnsavedChanges ]

                    else
                        span [ title (tr language LastEdit ++ " " ++ lastChangeString) ] [ text <| tr language ChangesSaved ]

                FileDoc (NewDoc _) ->
                    span [] [ text <| tr language NeverSaved ]

                CloudDoc _ ->
                    -- TODO: states and translations
                    span [] [ text "Cloud Document" ]
    in
    div
        [ id "save-indicator", classList [ ( "inset", True ), ( "saving", dirty ) ] ]
        [ saveStateSpan ]


viewSearchField : { m | viewState : ViewState, language : Language } -> Html Msg
viewSearchField { viewState, language } =
    let
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
                    , onInput SearchFieldUpdated
                    ]
                    []
                , maybeSearchIcon
                ]

        _ ->
            div
                [ id "search-field" ]
                []


viewFooter : { m | viewState : ViewState, workingTree : Trees.Model, startingWordcount : Int, shortcutTrayOpen : Bool, wordcountTrayOpen : Bool, language : Language, isMac : Bool, textCursorInfo : TextCursorInfo } -> Html Msg
viewFooter model =
    let
        isTextSelected =
            model.textCursorInfo.selected

        wordCounts =
            getWordCounts model

        current =
            wordCounts.document

        session =
            current - model.startingWordcount

        viewWordCount =
            case model.viewState.viewMode of
                Normal ->
                    [ div
                        [ id "wordcount"
                        , classList [ ( "inset", True ), ( "open", model.wordcountTrayOpen ) ]
                        , onClick WordcountTrayToggle
                        ]
                        [ span [] [ text (tr model.language (WordCountSession session)) ]
                        , span [] [ text (tr model.language (WordCountTotal current)) ]
                        , span [] [ text (tr model.language (WordCountCard wordCounts.card)) ]
                        , span [] [ text (tr model.language (WordCountSubtree wordCounts.subtree)) ]
                        , span [] [ text (tr model.language (WordCountGroup wordCounts.group)) ]
                        , span [] [ text (tr model.language (WordCountColumn wordCounts.column)) ]
                        ]
                    ]

                _ ->
                    []

        isOnly =
            case model.workingTree.tree.children of
                Children [ singleRoot ] ->
                    if singleRoot.children == Children [] then
                        True

                    else
                        False

                _ ->
                    False

        hoverHeight n =
            14
                * n
                + 6
                |> Debug.toString
                |> (\s -> s ++ "px")
    in
    div
        [ class "footer" ]
        ([ viewShortcutsToggle model.language model.shortcutTrayOpen model.isMac isOnly model.textCursorInfo model.viewState ]
            ++ viewWordCount
        )


viewVideo : { m | videoModalOpen : Bool } -> Html Msg
viewVideo { videoModalOpen } =
    if videoModalOpen then
        div [ class "modal-container" ]
            [ div [ class "modal" ]
                [ div [ class "modal-header" ]
                    [ h1 [] [ text "Learning Videos" ]
                    , a [ onClick (VideoModal False) ] [ text "×" ]
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


viewShortcutsToggle : Language -> Bool -> Bool -> Bool -> TextCursorInfo -> ViewState -> Html Msg
viewShortcutsToggle lang isOpen isMac isOnly textCursorInfo vs =
    let
        isTextSelected =
            textCursorInfo.selected

        viewIf cond content =
            if cond then
                content

            else
                text ""

        spanSplit key descAdd descSplit =
            if textCursorInfo.position == End || textCursorInfo.position == Empty then
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
                div
                    [ id "shortcuts-tray", class "inset", onClick ShortcutTrayToggle ]
                    [ div [ class "popup" ]
                        [ shortcutSpan [ tr lang EnterKey ] (tr lang EnterAction)
                        , viewIf (not isOnly) <| shortcutSpan [ "↑", "↓", "←", "→" ] (tr lang ArrowsAction)
                        , shortcutSpan [ ctrlOrCmd, "→" ] (tr lang AddChildAction)
                        , shortcutSpan [ ctrlOrCmd, "↓" ] (tr lang AddBelowAction)
                        , shortcutSpan [ ctrlOrCmd, "↑" ] (tr lang AddAboveAction)
                        , viewIf (not isOnly) <| shortcutSpan [ "Alt", tr lang ArrowKeys ] (tr lang MoveAction)
                        , viewIf (not isOnly) <| shortcutSpan [ ctrlOrCmd, tr lang Backspace ] (tr lang DeleteAction)
                        , shortcutSpan [ ctrlOrCmd, "Shift", "↓" ] (tr lang MergeDownAction)
                        , shortcutSpan [ ctrlOrCmd, "Shift", "↑" ] (tr lang MergeUpAction)
                        ]
                    , div [ class "icon-stack" ]
                        [ Icon.keyboard (defaultOptions |> iconColor)
                        , Icon.question (defaultOptions |> iconColor |> Icon.size 14)
                        ]
                    ]

            _ ->
                div
                    [ id "shortcuts-tray", class "inset", onClick ShortcutTrayToggle ]
                    [ div [ class "popup" ]
                        [ shortcutSpan [ ctrlOrCmd, tr lang EnterKey ] (tr lang ToSaveChanges)
                        , shortcutSpan [ tr lang EscKey ] (tr lang ToCancelChanges)
                        , splitChild
                        , splitBelow
                        , splitAbove
                        , shortcutSpanEnabled isTextSelected [ ctrlOrCmd, "B" ] (tr lang ForBold)
                        , shortcutSpanEnabled isTextSelected [ ctrlOrCmd, "I" ] (tr lang ForItalic)
                        , span [ class "markdown-guide" ]
                            [ a [ href "http://commonmark.org/help" ]
                                [ text <| tr lang FormattingGuide
                                , span [ class "icon-container" ] [ Icon.linkExternal (defaultOptions |> iconColor |> Icon.size 14) ]
                                ]
                            ]
                        ]
                    , div [ class "icon-stack" ]
                        [ Icon.keyboard (defaultOptions |> iconColor)
                        , Icon.question (defaultOptions |> iconColor |> Icon.size 14)
                        ]
                    ]

    else
        let
            iconColor =
                Icon.color "#6c7c84"
        in
        div
            [ id "shortcuts-tray", class "inset", onClick ShortcutTrayToggle, title <| tr lang KeyboardHelp ]
            [ div [ class "icon-stack" ]
                [ Icon.keyboard (defaultOptions |> iconColor)
                , Icon.question (defaultOptions |> iconColor |> Icon.size 14)
                ]
            ]


viewWordcountProgress : Int -> Int -> Html Msg
viewWordcountProgress current session =
    let
        currW =
            1 / (1 + toFloat session / toFloat current)

        sessW =
            1 - currW
    in
    div [ id "wc-progress" ]
        [ div [ id "wc-progress-wrap" ]
            [ span [ style "flex" (Debug.toString currW), id "wc-progress-bar" ] []
            , span [ style "flex" (Debug.toString sessW), id "wc-progress-bar-session" ] []
            ]
        ]


getWordCounts : { m | viewState : ViewState, workingTree : Trees.Model } -> WordCount
getWordCounts model =
    let
        activeCardId =
            model.viewState.active

        tree =
            model.workingTree.tree

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
    WordCount
        cardCount
        subtreeCount
        groupCount
        columnCount
        treeCount


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


radio : msg -> Bool -> Html msg -> Html msg
radio msg bool labelElement =
    label []
        [ input [ type_ "radio", checked bool, onClick msg ] []
        , labelElement
        ]
