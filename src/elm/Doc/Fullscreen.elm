module Doc.Fullscreen exposing (Msg(..), view)

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
    | SaveChanges
    | ExitFullscreenRequested



-- VIEW


view :
    { language : Language
    , isMac : Bool
    , dirty : Bool
    , lastLocalSave : Maybe Time.Posix
    , lastRemoteSave : Maybe Time.Posix
    , currentTime : Time.Posix
    , model : Model
    }
    -> String
    -> ViewState
    -> Html Msg
view { language, isMac, dirty, model, lastLocalSave, lastRemoteSave, currentTime } field vstate =
    let
        updateField c =
            if c.id == vstate.active then
                { c | content = field }

            else
                c

        currentColumn =
            getColumnById vstate.active model.tree
                |> Maybe.map (List.map (List.map updateField))
                |> Maybe.withDefault []

        saveShortcutTip =
            if isMac then
                "⌘+S to Save"

            else
                "Ctrl+S to Save"
    in
    div
        [ id "app-fullscreen" ]
        [ viewColumn vstate.active currentColumn
        , div [ id "fullscreen-buttons", classList [ ( "dirty", dirty ) ] ]
            [ Icons.fullscreenExitOutlined [ id "fullscreen-exit", width 24, onClick ExitFullscreenRequested, title "Exit Fullscreen Mode" ]
            , div []
                [ div [ id "fullscreen-save-button", onClick SaveChanges, title saveShortcutTip ] [ Icons.saveOutlined [ width 24 ] ]
                , viewSaveIndicator language { dirty = dirty, lastLocalSave = lastLocalSave, lastRemoteSave = lastRemoteSave } currentTime
                ]
            ]
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
