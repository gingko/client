module Doc.WelcomeChecklist exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, div, li, span, text, ul)
import Html.Attributes exposing (class, classList, id)



-- MODEL


type alias Model =
    Maybe
        { navWithArrows : Bool
        , editWithKeyboard : Bool
        , saveWithKeyboard : Bool
        , createWithKeyboard : Bool
        , draggedCard : Bool
        }


init : Model
init =
    Just
        { navWithArrows = False
        , editWithKeyboard = False
        , saveWithKeyboard = False
        , createWithKeyboard = False
        , draggedCard = False
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
    case model of
        Just state ->
            [ div [ id "welcome-checklist" ]
                [ ul []
                    [ viewChecklistItem
                        "nav-with-arrows"
                        state.navWithArrows
                        [ text "Move around (", span [ class "shortcut-key" ] [ text "-> etc" ], text ")" ]
                    , viewChecklistItem
                        "edit-with-keyboard"
                        state.editWithKeyboard
                        [ text "Edit a card (", span [ class "shortcut-key" ] [ text "Enter" ], text ")" ]
                    , viewChecklistItem
                        "save-with-keyboard"
                        state.saveWithKeyboard
                        [ text "Save card edits (ctrl+enter)" ]
                    , viewChecklistItem
                        "create-with-keyboard"
                        state.createWithKeyboard
                        [ text "Create" ]
                    , viewChecklistItem
                        "drag-card"
                        state.draggedCard
                        [ text "Drag" ]
                    ]
                ]
            ]

        Nothing ->
            []


viewChecklistItem : String -> Bool -> List (Html msg) -> Html msg
viewChecklistItem className isDone children =
    li [ classList [ ( className, True ), ( "done", isDone ) ] ] children
