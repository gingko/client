module Doc.Fullscreen exposing (view)

import Ant.Icons.Svg as Icons
import Doc.TreeUtils exposing (getColumnById)
import Doc.UI exposing (viewSaveIndicator)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2, lazy3, lazy4)
import Json.Encode as Enc
import Time
import Translation exposing (Language, TranslationId(..))
import Types exposing (..)
import UI.Collaborators



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
    , collaborators : List Collaborator
    , activeId : String
    , msgs : MsgConfig msg
    }


type alias MsgConfig msg =
    { exitFullscreenRequested : msg
    , saveChanges : msg
    , saveAndExitFullscreen : msg
    }



-- VIEW


view : Config msg -> Html msg
view ({ model, activeId } as config) =
    let
        currentColumn =
            getColumnById activeId model.tree
                |> Maybe.withDefault []
    in
    div
        [ id "app-fullscreen" ]
        [ viewColumn config.collaborators activeId currentColumn
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


viewColumn : List Collaborator -> String -> Column -> Html msg
viewColumn collaborators active col =
    div
        [ id "fullscreen-main" ]
        (List.map (lazy3 viewGroup collaborators active) col)


viewGroup : List Collaborator -> String -> Group -> Html msg
viewGroup collaborators active xs =
    let
        viewFunction t =
            let
                isActive =
                    t.id == active

                collabsOnCard =
                    collaborators
                        |> List.filter
                            (\c ->
                                case c.mode of
                                    CollabActive collabCardId ->
                                        collabCardId == t.id

                                    CollabEditing collabCardId ->
                                        collabCardId == t.id
                            )
            in
            ( t.id, lazy4 viewCard collabsOnCard isActive t.id t.content )
    in
    Keyed.node "div"
        [ class "group-fullscreen" ]
        (List.map viewFunction xs)


viewCard : List Collaborator -> Bool -> String -> String -> Html msg
viewCard collabsOnCard isActive cardId content =
    let
        editingByCollab =
            collabsOnCard
                |> List.filter
                    (\c ->
                        case c.mode of
                            CollabEditing collabEditingId ->
                                collabEditingId == cardId

                            _ ->
                                False
                    )
                |> (not << List.isEmpty)
    in
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card-fullscreen", True )
            , ( "active-fullscreen", isActive )
            , ( "cursor-not-allowed", editingByCollab )
            ]
        , style "position" "relative"
        ]
        [ node "gw-textarea"
            ([ attribute "card-id" cardId
             , dir "auto"
             , classList
                [ ( "edit", True )
                , ( "mousetrap", True )
                ]
             , attribute "data-private" "lipsum"
             , attribute "data-gramm" "false"
             , A.property "isFullscreen" (Enc.bool True)
             , attribute "start-value" content
             ]
                ++ (if editingByCollab then
                        [ attribute "disabled" "true" ]

                    else
                        []
                   )
            )
            [ UI.Collaborators.viewOnCard collabsOnCard ]
        ]
