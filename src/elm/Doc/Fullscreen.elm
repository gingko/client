module Doc.Fullscreen exposing (view)

import Ant.Icons.Svg as Icons
import Doc.TreeUtils exposing (getColumnById)
import Doc.UI exposing (viewSaveIndicator)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3, lazy4)
import Time
import Translation exposing (Language, TranslationId(..))
import Types exposing (..)



-- MODEL


type alias Model =
    { tree : Tree
    , columns : List Column
    }


type alias Config msg =
    { language : Language
    , isMac : Bool
    , dirty : Bool
    , lastLocalSave : Maybe Time.Posix
    , lastRemoteSave : Maybe Time.Posix
    , currentTime : Time.Posix
    , model : Model
    , activeId : String
    , msgs : MsgConfig msg
    }


type alias MsgConfig msg =
    { fieldUpdated : String -> String -> msg
    , openCard : String -> String -> msg
    , exitFullscreenRequested : msg
    , saveChanges : msg
    , saveAndExitFullscreen : msg
    }



-- VIEW


view : Config msg -> Html msg
view ({ model, activeId, msgs } as config) =
    let
        currentColumn =
            getColumnById activeId model.tree
                |> Maybe.withDefault []
    in
    div
        [ id "app-fullscreen" ]
        [ viewColumn msgs activeId currentColumn
        , viewFullscreenButtons config
        ]


viewFullscreenButtons : Config msg -> Html msg
viewFullscreenButtons { language, isMac, dirty, lastLocalSave, lastRemoteSave, currentTime, msgs } =
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


viewColumn : MsgConfig msg -> String -> Column -> Html msg
viewColumn msgs active col =
    div
        [ id "fullscreen-main" ]
        (List.map (lazy3 viewGroup msgs active) col)


viewGroup : MsgConfig msg -> String -> Group -> Html msg
viewGroup msgs active xs =
    let
        viewFunction t =
            ( t.id, lazy4 viewCard msgs (t.id == active) t.id t.content )
    in
    Keyed.node "div"
        [ class "group-fullscreen" ]
        (List.map viewFunction xs)


viewCard : { m | openCard : String -> String -> msg, fieldUpdated : String -> String -> msg } -> Bool -> String -> String -> Html msg
viewCard msgs isActive cardId content =
    let
        _ =
            Debug.log "viewCard Fullscreen" ()
    in
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
            , onFocus <| msgs.openCard cardId content
            , onInput <| msgs.fieldUpdated cardId
            , attribute "data-private" "lipsum"
            , attribute "data-gramm" "false"
            , value content
            ]
            []
        ]
