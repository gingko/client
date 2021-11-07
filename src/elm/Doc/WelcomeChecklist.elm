module Doc.WelcomeChecklist exposing (Model, Msg(..), init, update, view)

import Ant.Icons.Svg as AntIcons
import Html exposing (Html, button, div, h2, h3, li, s, span, strong, text, ul)
import Html.Attributes exposing (class, classList, height, id, style, title, width)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf)
import Session exposing (Session)



-- MODEL


type alias Model =
    Maybe State


type alias State =
    { navWithMouse : Bool
    , navWithArrows : Bool
    , editWithKeyboard : Bool
    , saveWithKeyboard : Bool
    , createWithKeyboard : Bool
    , createChildWithKeyboard : Bool
    , draggedCard : Bool
    , isMac : Bool
    }


init : Session -> Model
init session =
    let
        shouldShowWelcomeChecklist =
            Session.welcomeChecklist session
    in
    if shouldShowWelcomeChecklist then
        Just
            { navWithMouse = False
            , navWithArrows = False
            , editWithKeyboard = False
            , saveWithKeyboard = False
            , createWithKeyboard = False
            , createChildWithKeyboard = False
            , draggedCard = False
            , isMac = Session.isMac session
            }

    else
        Nothing



-- UPDATE


type Msg
    = NavigatedWithMouse
    | NavigatedWithArrows
    | EditedWithKeyboard
    | SaveWithKeyboard
    | CreateWithKeyboard
    | CreateChildWithKeyboard
    | DraggedCard


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case model of
        Just state ->
            let
                newState =
                    case msg of
                        NavigatedWithMouse ->
                            { state | navWithMouse = True }

                        NavigatedWithArrows ->
                            { state | navWithArrows = True }

                        EditedWithKeyboard ->
                            { state | editWithKeyboard = True }

                        SaveWithKeyboard ->
                            { state | saveWithKeyboard = True }

                        CreateWithKeyboard ->
                            { state | createWithKeyboard = True }

                        CreateChildWithKeyboard ->
                            { state | createChildWithKeyboard = True }

                        DraggedCard ->
                            { state | draggedCard = True }
            in
            ( Just newState, isAllDone newState )

        Nothing ->
            ( Nothing, False )


isAllDone : State -> Bool
isAllDone state =
    progressSteps state
        |> (\( d, t ) -> d == t)



-- VIEW


view : msg -> Model -> List (Html msg)
view skipMsg model =
    let
        shortcutSpan keys =
            keys
                |> List.map (\k -> span [ class "shortcut-key" ] [ text k ])
    in
    case model of
        Just state ->
            let
                ctrlOrCmd =
                    if state.isMac then
                        "⌘"

                    else
                        "Ctrl"

                fractionText =
                    progressSteps state
                        |> (\( d, t ) -> String.fromInt d ++ "/" ++ String.fromInt t)

                barWidth =
                    progressSteps state
                        |> (\( d, t ) -> toFloat d / toFloat t * 100)
                        |> String.fromFloat
                        |> (\s -> s ++ "%")
            in
            [ div [ id "welcome-checklist-container", classList [ ( "all-done", isAllDone state ) ] ]
                [ h3 [] [ text "Your First Steps", span [ id "welcome-progress-fraction" ] [ text fractionText ] ]
                , div [ id "welcome-progress-track" ] [ div [ id "welcome-progress-bar", style "width" barWidth ] [] ]
                , div [ class "skip-tutorial", onClick skipMsg, title "Skip Tutorial" ] [ AntIcons.closeCircleOutlined [ width 16, height 16 ] ]
                , ul []
                    [ viewChecklistItem
                        "nav-with-mouse"
                        state.navWithMouse
                        [ strong [] [ text "Navigate with Mouse" ], text " (click on a card to select)" ]
                    , viewChecklistItem
                        "nav-with-arrows"
                        state.navWithArrows
                        [ strong [] [ text "Navigate with Keyboard" ], text " (arrow keys or hjkl)" ]
                    , viewChecklistItem
                        "edit-with-keyboard"
                        state.editWithKeyboard
                        ([ text "Switch to ", strong [] [ text "Editing the Current Card" ], text " (" ] ++ shortcutSpan [ "Enter" ] ++ [ text ")" ])
                    , viewChecklistItem
                        "save-with-keyboard"
                        state.saveWithKeyboard
                        ([ text "Type something, then ", strong [] [ text "Save Changes" ], text " (" ] ++ shortcutSpan [ ctrlOrCmd, "Enter" ] ++ [ text ")" ])
                    , viewChecklistItem
                        "create-with-keyboard"
                        state.createWithKeyboard
                        ([ text "Create a ", strong [] [ text "New Card Below" ], text " (" ] ++ shortcutSpan [ ctrlOrCmd, "↓" ] ++ [ text ")" ])
                    , viewChecklistItem
                        "create-child-with-keyboard"
                        state.createChildWithKeyboard
                        ([ text "Create a ", strong [] [ text "New Child Card" ], text " (" ] ++ shortcutSpan [ ctrlOrCmd, "→" ] ++ [ text ")" ])
                    , viewChecklistItem
                        "drag-card"
                        state.draggedCard
                        [ strong [] [ text "Drag a Card" ], text " (by its left edge)" ]
                    ]
                , if isAllDone state then
                    div [ class "confetti" ] (List.repeat 13 (div [ class "confetti-piece" ] []))

                  else
                    div [] []
                ]
            ]

        Nothing ->
            [ div [] [] ]


viewChecklistItem : String -> Bool -> List (Html msg) -> Html msg
viewChecklistItem className isDone children =
    if isDone then
        li [ classList [ ( className, True ), ( "done", True ) ] ] [ AntIcons.checkCircleTwoTone [], span [ class "content" ] children ]

    else
        li [ classList [ ( className, True ), ( "done", False ) ] ] [ div [ class "undone-item" ] [], span [ class "content" ] children ]


progressSteps : State -> ( Int, Int )
progressSteps state =
    let
        toBit b =
            if b then
                1

            else
                0

        bits =
            [ ( toBit state.navWithMouse, 1 )
            , ( toBit state.navWithArrows, 1 )
            , ( toBit state.editWithKeyboard, 1 )
            , ( toBit state.saveWithKeyboard, 1 )
            , ( toBit state.createWithKeyboard, 1 )
            , ( toBit state.createChildWithKeyboard, 1 )
            , ( toBit state.draggedCard, 1 )
            ]
    in
    ( bits |> List.map Tuple.first |> List.sum, bits |> List.map Tuple.second |> List.sum )
