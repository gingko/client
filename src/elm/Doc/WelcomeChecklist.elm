module Doc.WelcomeChecklist exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, div, li, span, strong, text, ul)
import Html.Attributes exposing (class, classList, id)



-- MODEL


type alias Model =
    Maybe
        { navWithArrows : Bool
        , editWithKeyboard : Bool
        , saveWithKeyboard : Bool
        , createWithKeyboard : Bool
        , draggedCard : Bool
        , isMac : Bool
        }


init : Bool -> Model
init isMac =
    Just
        { navWithArrows = False
        , editWithKeyboard = False
        , saveWithKeyboard = False
        , createWithKeyboard = False
        , draggedCard = False
        , isMac = isMac
        }



-- UPDATE


type Msg
    = NavigatedWithArrows
    | EditedWithKeyboard
    | SaveWithKeyboard
    | CreateWithKeyboard
    | DraggedCard


update : Msg -> Model -> Model
update msg model =
    case model of
        Just state ->
            case msg of
                NavigatedWithArrows ->
                    Just { state | navWithArrows = True }

                EditedWithKeyboard ->
                    Just { state | editWithKeyboard = True }

                SaveWithKeyboard ->
                    Just { state | saveWithKeyboard = True }

                CreateWithKeyboard ->
                    Just { state | createWithKeyboard = True }

                DraggedCard ->
                    Just { state | draggedCard = True }

        Nothing ->
            Nothing



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
            [ div [ id "welcome-checklist" ]
                [ ul []
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
                        "create-below-with-keyboard"
                        state.createWithKeyboard
                        ([ text "Create a ", strong [] [ text "New Card Below" ], text " (" ] ++ shortcutSpan [ ctrlOrCmd, "↓" ] ++ [ text ")" ])
                    , viewChecklistItem
                        "create-with-keyboard"
                        state.createWithKeyboard
                        ([ text "Create a ", strong [] [ text "New Child Card" ], text " (" ] ++ shortcutSpan [ ctrlOrCmd, "→" ] ++ [ text ")" ])
                    , viewChecklistItem
                        "drag-card"
                        state.draggedCard
                        [ strong [] [ text "Drag a Card" ], text " (by its left edge)" ]
                    ]
                ]
            ]

        Nothing ->
            []


viewChecklistItem : String -> Bool -> List (Html msg) -> Html msg
viewChecklistItem className isDone children =
    li [ classList [ ( className, True ), ( "done", isDone ) ] ] children
