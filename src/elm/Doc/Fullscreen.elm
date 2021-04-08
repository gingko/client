module Doc.Fullscreen exposing (Msg(..), view)

import Ant.Icons.Svg as Icons
import Doc.TreeUtils exposing (getColumnById)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2)
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
    | ExitFullscreenRequested



-- VIEW


view : Language -> String -> ViewState -> Bool -> Model -> Html Msg
view _ field vstate dirty model =
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
    in
    div
        [ id "app-fullscreen" ]
        [ viewColumn vstate.active currentColumn
        , div [ id "fullscreen-buttons" ]
            [ Icons.fullscreenExitOutlined [ id "fullscreen-exit", width 24, onClick ExitFullscreenRequested ]
            , if dirty then
                div [ id "fullscreen-save-indicator" ] [ text "Unsaved changes..." ]

              else
                div [] []
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
