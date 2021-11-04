module Doc.WelcomeChecklist exposing (Model, Msg(..), init, update, view)

import Ant.Icons.Svg as AntIcons
import Html exposing (Html, div, h2, h3, li, s, span, strong, text, ul)
import Html.Attributes exposing (class, classList, id)
import Html.Extra exposing (viewIf)
import Session exposing (Session)



-- MODEL


type alias Model =
    Maybe State


type alias State =
    { navWithArrows : Bool
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
            { navWithArrows = False
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
    = NavigatedWithArrows
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
    state.navWithArrows
        && state.editWithKeyboard
        && state.saveWithKeyboard
        && state.createWithKeyboard
        && state.createChildWithKeyboard
        && state.draggedCard



-- VIEW


view : Model -> List (Html msg)
view model =
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
            in
            [ div [ id "welcome-checklist-container", classList [ ( "all-done", isAllDone state ) ] ]
                [ h3 [] [ text "Getting Started" ]
                , ul []
                    [ viewChecklistItem
                        "nav-with-arrows"
                        state.navWithArrows
                        [ strong [] [ text "Move Around" ], text " (arrow keys)" ]
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
                , viewIf (isAllDone state) (div [ class "confetti" ] (List.repeat 13 (div [ class "confetti-piece" ] [])))
                ]
            ]

        Nothing ->
            []


viewChecklistItem : String -> Bool -> List (Html msg) -> Html msg
viewChecklistItem className isDone children =
    if isDone then
        li [ classList [ ( className, True ), ( "done", True ) ] ] [ AntIcons.checkCircleTwoTone [], span [ class "content" ] children ]

    else
        li [ classList [ ( className, True ), ( "done", False ) ] ] [ AntIcons.borderOutlined [], span [ class "content" ] children ]
