module UI exposing (countWords, viewConflict, viewFooter, viewHistory, viewSaveIndicator, viewSearchField, viewVideo)

import Coders exposing (treeToMarkdownString)
import Date
import Date.Distance as DateDist
import Dict
import Diff exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra as ListExtra exposing ((!!))
import Objects
import Octicons as Icon exposing (defaultOptions)
import Regex exposing (Regex, regex, replace)
import Time exposing (Time)
import TreeUtils exposing (..)
import Trees exposing (defaultTree)
import Types exposing (..)


viewSaveIndicator : { m | changed : Bool, objects : Objects.Model, currentTime : Time } -> Html Msg
viewSaveIndicator model =
    let
        lastCommitTime =
            model.objects.commits
                |> Dict.toList
                |> List.sortBy (\( k, v ) -> -v.timestamp)
                |> List.map (\( k, v ) -> v.timestamp)
                |> List.head
                |> Maybe.withDefault 0
                |> toFloat

        lastChangeString =
            DateDist.inWords
                (model.currentTime |> Date.fromTime)
                (lastCommitTime |> Date.fromTime)
                ++ " ago"
    in
    div
        [ id "save-indicator", classList [ ( "inset", True ), ( "saving", model.changed ) ] ]
        [ if model.changed then
            span [ title ("Last saved " ++ lastChangeString) ] [ text "Unsaved changes..." ]

          else
            span [ title ("Last edit " ++ lastChangeString) ] [ text "All changes saved" ]
        ]


viewSearchField : { m | viewState : ViewState } -> Html Msg
viewSearchField model =
    let
        maybeSearchIcon =
            if model.viewState.searchField == Nothing then
                Icon.search (defaultOptions |> Icon.color "#445" |> Icon.size 12)

            else
                text ""
    in
    if model.viewState.editing == Nothing then
        div
            [ id "search-field" ]
            [ input
                [ type_ "search"
                , id "search-input"
                , title "Press '/' to search"
                , onInput SearchFieldUpdated
                ]
                []
            , maybeSearchIcon
            ]

    else
        div
            [ id "search-field" ]
            []


viewFooter : { m | viewState : ViewState, workingTree : Trees.Model, startingWordcount : Int, shortcutTrayOpen : Bool, wordcountTrayOpen : Bool, isMac : Bool, isTextSelected : Bool, changed : Bool } -> Html Msg
viewFooter model =
    let
        wordCounts =
            getWordCounts model

        current =
            wordCounts.document

        session =
            current - model.startingWordcount

        viewWordCount =
            if model.viewState.editing == Nothing then
                [ div
                    [ id "wordcount"
                    , classList [ ( "inset", True ), ( "open", model.wordcountTrayOpen ) ]
                    , onClick WordcountTrayToggle
                    ]
                    [ span [] [ text ("Session: " ++ (session |> toWordsString)) ]
                    , span [] [ text ("Total: " ++ (current |> toWordsString)) ]
                    , span [] [ text ("Card: " ++ (wordCounts.card |> toWordsString)) ]
                    , span [] [ text ("Subtree: " ++ (wordCounts.subtree |> toWordsString)) ]
                    , span [] [ text ("Group: " ++ (wordCounts.group |> toWordsString)) ]
                    , span [] [ text ("Column: " ++ (wordCounts.column |> toWordsString)) ]
                    ]
                ]

            else
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
                |> toString
                |> (\s -> s ++ "px")
    in
    div
        [ class "footer" ]
        ([ viewShortcutsToggle model.shortcutTrayOpen model.isMac isOnly model.isTextSelected model.viewState ]
            ++ viewWordCount
        )


viewHistory : String -> Objects.Model -> Html Msg
viewHistory currHead objects =
    let
        master =
            Dict.get "heads/master" objects.refs

        historyList =
            case master of
                Just refObj ->
                    (refObj.value :: refObj.ancestors)
                        |> List.reverse

                _ ->
                    []

        maxIdx =
            historyList |> List.length |> (\x -> x - 1) |> toString

        currIdx =
            historyList
                |> ListExtra.elemIndex currHead
                |> Maybe.map toString
                |> Maybe.withDefault maxIdx

        checkoutCommit idxStr =
            case String.toInt idxStr of
                Ok idx ->
                    case historyList !! idx of
                        Just commit ->
                            CheckoutCommit commit

                        Nothing ->
                            NoOp

                Err _ ->
                    NoOp
    in
    div [ id "history" ]
        [ input [ type_ "range", A.min "0", A.max maxIdx, value currIdx, step "1", onInput checkoutCommit ] []
        , button [ onClick Restore ] [ text "Restore this Version" ]
        , button [ onClick CancelHistory ] [ text "Cancel" ]
        ]


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


viewShortcutsToggle : Bool -> Bool -> Bool -> Bool -> ViewState -> Html Msg
viewShortcutsToggle isOpen isMac isOnly isTextSelected vs =
    let
        viewIf cond content =
            if cond then
                content

            else
                text ""

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
        if vs.editing == Nothing then
            let
                iconColor =
                    Icon.color "#445"
            in
            div
                [ id "shortcuts-tray", class "inset", onClick ShortcutTrayToggle ]
                [ div [ class "popup" ]
                    [ shortcutSpan [ "Enter" ] "to Edit"
                    , viewIf (not isOnly) <| shortcutSpan [ "↑", "↓", "←", "→" ] "to Navigate"
                    , shortcutSpan [ ctrlOrCmd, "→" ] "to Add Child"
                    , shortcutSpan [ ctrlOrCmd, "↓" ] "to Add Below"
                    , shortcutSpan [ ctrlOrCmd, "↑" ] "to Add Above"
                    , viewIf (not isOnly) <| shortcutSpan [ "Alt", "(arrows)" ] "to Move"
                    , viewIf (not isOnly) <| shortcutSpan [ ctrlOrCmd, "Backspace" ] "to Delete"
                    ]
                , div [ class "icon-stack" ]
                    [ Icon.keyboard (defaultOptions |> iconColor)
                    , Icon.question (defaultOptions |> iconColor |> Icon.size 14)
                    ]
                ]

        else
            let
                iconColor =
                    Icon.color "#445"
            in
            div
                [ id "shortcuts-tray", class "inset", onClick ShortcutTrayToggle ]
                [ div [ class "popup" ]
                    [ shortcutSpan [ ctrlOrCmd, "Enter" ] "to Save Changes"
                    , shortcutSpan [ "Esc" ] "to Cancel Changes"
                    , shortcutSpanEnabled isTextSelected [ ctrlOrCmd, "B" ] "for Bold"
                    , shortcutSpanEnabled isTextSelected [ ctrlOrCmd, "I" ] "for Italic"
                    , span [ class "markdown-guide" ]
                        [ a [ href "http://commonmark.org/help" ]
                            [ text "Markdown Formatting Guide"
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
            [ id "shortcuts-tray", class "inset", onClick ShortcutTrayToggle, title "Keyboard Shortcuts Help" ]
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
            [ span [ style "flex" (toString currW), id "wc-progress-bar" ] []
            , span [ style "flex" (toString sessW), id "wc-progress-bar-session" ] []
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
            regex "[!@#$%^&*():;\"',.]+"
    in
    str
        |> String.toLower
        |> replace Regex.All punctuation (\_ -> "")
        |> String.words
        |> List.filter ((/=) "")
        |> List.length


toWordsString : Int -> String
toWordsString num =
    case num of
        1 ->
            "1 word"

        n ->
            toString n ++ " words"


viewConflict : Conflict -> Html Msg
viewConflict { id, opA, opB, selection, resolved } =
    let
        withManual cardId oursElement theirsElement =
            li
                []
                [ fieldset []
                    [ radio (SetSelection id Original cardId) (selection == Original) (text "Original")
                    , radio (SetSelection id Ours cardId) (selection == Ours) oursElement
                    , radio (SetSelection id Theirs cardId) (selection == Theirs) theirsElement
                    , radio (SetSelection id Manual cardId) (selection == Manual) (text "Merged")
                    , label []
                        [ input [ checked resolved, type_ "checkbox", onClick (Resolve id) ] []
                        , text "Resolved"
                        ]
                    ]
                ]

        withoutManual cardIdA cardIdB =
            li
                []
                [ fieldset []
                    [ radio (SetSelection id Original "") (selection == Original) (text "Original")
                    , radio (SetSelection id Ours cardIdA) (selection == Ours) (text ("Ours:" ++ (toString opA |> String.left 3)))
                    , radio (SetSelection id Theirs cardIdB) (selection == Theirs) (text ("Theirs:" ++ (toString opB |> String.left 3)))
                    , label []
                        [ input [ checked resolved, type_ "checkbox", onClick (Resolve id) ] []
                        , text "Resolved"
                        ]
                    ]
                ]

        newConflictView cardId ourChanges theirChanges =
            div [ class "flex-row" ]
                [ div [ class "conflict-container flex-column" ]
                    [ div
                        [ classList [ ( "row option", True ), ( "selected", selection == Original ) ]
                        , onClick (SetSelection id Original cardId)
                        ]
                        [ text "Original" ]
                    , div [ class "row flex-row" ]
                        [ div
                            [ classList [ ( "option", True ), ( "selected", selection == Ours ) ]
                            , onClick (SetSelection id Ours cardId)
                            ]
                            [ text "Ours"
                            , ul [ class "changelist" ] ourChanges
                            ]
                        , div
                            [ classList [ ( "option", True ), ( "selected", selection == Theirs ) ]
                            , onClick (SetSelection id Theirs cardId)
                            ]
                            [ text "Theirs"
                            , ul [ class "changelist" ] theirChanges
                            ]
                        ]
                    , div
                        [ classList [ ( "row option", True ), ( "selected", selection == Manual ) ]
                        , onClick (SetSelection id Manual cardId)
                        ]
                        [ text "Merged" ]
                    ]
                , button [ onClick (Resolve id) ] [ text "Resolved" ]
                ]
    in
    case ( opA, opB ) of
        ( Mod idA _ strA orig, Mod _ _ strB _ ) ->
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

        ( Types.Ins idA _ _ _, Del idB _ ) ->
            withoutManual idA idB

        ( Del idA _, Types.Ins idB _ _ _ ) ->
            withoutManual idA idB

        _ ->
            withoutManual "" ""


radio : msg -> Bool -> Html msg -> Html msg
radio msg bool labelElement =
    label []
        [ input [ type_ "radio", checked bool, onClick msg ] []
        , labelElement
        ]
