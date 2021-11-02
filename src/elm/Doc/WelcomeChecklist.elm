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

        Nothing ->
            Nothing



-- VIEW


view : Model -> List (Html msg)
view model =
    case model of
        Just state ->
            [ div [ id "welcome-checklist" ]
                [ ul []
                    [ li [ classList [ ( "nav-with-arrows", True ), ( "done", state.navWithArrows ) ] ] [ text "Move around (", span [ class "shortcut-key" ] [ text "-> etc" ], text ")" ]
                    , li [ classList [ ( "edit-with-keyboard", True ), ( "done", state.editWithKeyboard ) ] ] [ text "Edit a card (", span [ class "shortcut-key" ] [ text "Enter" ], text ")" ]
                    , li [ classList [ ( "save-with-keyboard", True ), ( "done", state.saveWithKeyboard ) ] ] [ text "Save card edits (ctrl+enter)" ]
                    , li [ classList [ ( "create-with-keyboard", True ), ( "done", state.createWithKeyboard ) ] ] [ text "Create" ]
                    ]
                ]
            ]

        Nothing ->
            []
