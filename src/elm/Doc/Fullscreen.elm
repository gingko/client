module Doc.Fullscreen exposing (Msg(..), view, viewFullscreenButtonsDesktop)

import Ant.Icons.Svg as Icons
import Doc.TreeUtils exposing (getColumnById)
import Doc.UI exposing (viewSaveIndicator)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2)
import Time
import Translation exposing (Language, TranslationId(..))
import Types exposing (..)



-- MODEL


type alias Model =
    { tree : Tree
    , columns : List Column
    }



-- UPDATE


type Msg
    = OpenCard String String
    | UpdateField String String



-- VIEW


view :
    { toSelf : Msg -> msg
    , exitFullscreenRequested : msg
    , saveChanges : msg
    , saveAndExitFullscreen : msg
    }
    ->
        { language : Language
        , isMac : Bool
        , dirty : Bool
        , lastLocalSave : Maybe Time.Posix
        , lastRemoteSave : Maybe Time.Posix
        , currentTime : Time.Posix
        , model : Model
        }
    -> String
    -> String
    -> Html msg
view msgs ({ model } as info) field activeId =
    let
        updateField c =
            if c.id == activeId then
                { c | content = field }

            else
                c

        currentColumn =
            getColumnById activeId model.tree
                |> Maybe.map (List.map (List.map updateField))
                |> Maybe.withDefault []
    in
    div
        [ id "app-fullscreen" ]
        [ viewColumn activeId currentColumn |> Html.map msgs.toSelf
        , viewFullscreenButtons msgs info
        ]


viewFullscreenButtons :
    { m
        | exitFullscreenRequested : msg
        , saveChanges : msg
        , saveAndExitFullscreen : msg
    }
    ->
        { language : Language
        , isMac : Bool
        , dirty : Bool
        , lastLocalSave : Maybe Time.Posix
        , lastRemoteSave : Maybe Time.Posix
        , currentTime : Time.Posix
        , model : Model
        }
    -> Html msg
viewFullscreenButtons msgs { language, isMac, dirty, lastLocalSave, lastRemoteSave, currentTime } =
    let
        saveShortcutTip =
            if isMac then
                "⌘+S to Save"

            else
                "Ctrl+S to Save"

        saveAndCloseTip =
            if isMac then
                "⌘+Enter to Save and Exit Fullscreen"

            else
                "Ctrl+Enter to Save and Exit Fullscreen"
    in
    div [ id "fullscreen-buttons", classList [ ( "dirty", dirty ) ] ]
        [ div
            [ id "fullscreen-exit", onClick msgs.exitFullscreenRequested, title "Exit Fullscreen Mode" ]
            [ Icons.fullscreenExitOutlined [ width 24 ] ]
        , div []
            [ div [ id "fullscreen-save-button", onClick msgs.saveChanges, title saveShortcutTip ] [ Icons.saveOutlined [ width 24 ] ]
            , viewSaveIndicator language { dirty = dirty, lastLocalSave = lastLocalSave, lastRemoteSave = lastRemoteSave } currentTime
            ]
        , div [ id "fullscreen-save-and-exit-button", onClick msgs.saveAndExitFullscreen, title saveAndCloseTip ]
            []
        ]


viewFullscreenButtonsDesktop :
    { exitFullscreenRequested : msg, saveAndExitFullscreen : msg }
    -> { isMac : Bool, dirty : Bool }
    -> Html msg
viewFullscreenButtonsDesktop msgs { isMac, dirty } =
    let
        saveAndCloseTip =
            if isMac then
                "⌘+Enter to Save and Exit Fullscreen"

            else
                "Ctrl+Enter to Save and Exit Fullscreen"
    in
    div [ id "fullscreen-buttons", classList [ ( "dirty", dirty ) ] ]
        [ div
            [ id "fullscreen-exit", onClick msgs.exitFullscreenRequested, title "Exit Fullscreen Mode" ]
            [ Icons.fullscreenExitOutlined [ width 24 ] ]
        , div [ id "fullscreen-save-and-exit-button", onClick msgs.saveAndExitFullscreen, title saveAndCloseTip ]
            []
        ]


viewColumn : String -> Column -> Html Msg
viewColumn active col =
    div
        [ id "fullscreen-main" ]
        (List.map (lazy2 viewGroup active) col)


viewGroup : String -> Group -> Html Msg
viewGroup active xs =
    let
        viewFunction t =
            ( t.id, viewCard (t.id == active) t.id t.content )
    in
    Keyed.node "div"
        [ class "group-fullscreen" ]
        (List.map viewFunction xs)


viewCard : Bool -> String -> String -> Html Msg
viewCard isActive cardId content =
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card-fullscreen", True )
            , ( "active-fullscreen", isActive )
            ]
        , attribute "data-cloned-content" content
        ]
        [ textarea
            [ id ("card-edit-" ++ cardId)
            , dir "auto"
            , classList
                [ ( "edit", True )
                , ( "mousetrap", True )
                ]
            , onFocus <| OpenCard cardId content
            , onInput <| UpdateField cardId
            , attribute "data-private" "lipsum"
            , value content
            ]
            []
        ]
