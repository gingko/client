module Fullscreen exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Translation exposing (Language, TranslationId(..), tr)
import TreeUtils exposing (getChildren, getColumnById, getParent, getTree)
import Types exposing (..)


type alias Model =
    { tree : Tree
    , columns : List Column
    }


view : Language -> ViewState -> Model -> Html Msg
view lang vstate model =
    let
        current_ =
            getTree vstate.active model.tree

        parent_ =
            getParent vstate.active model.tree
                |> Maybe.map (\t -> ( t.id, t.content ))

        currentColumn =
            getColumnById vstate.active model.tree
                |> Maybe.withDefault []

        children =
            current_
                |> Maybe.map getChildren
                |> Maybe.withDefault []
    in
    div
        [ id "app"
        ]
        [ viewMaybeParent parent_
        , viewColumn currentColumn
        , viewChildren children
        ]


viewMaybeParent : Maybe ( String, String ) -> Html Msg
viewMaybeParent parentTuple_ =
    case parentTuple_ of
        Just ( cardId, content ) ->
            div [ class "fullscreen-parent" ] [ text content ]

        Nothing ->
            div [ class "fullscreen-parent" ] []


viewColumn : Column -> Html Msg
viewColumn col =
    div
        [ class "fullscreen-main" ]
        (List.map (lazy viewGroup) col)


viewChildren : List Tree -> Html Msg
viewChildren xs =
    div [ class "fullscreen-children" ] []


viewGroup : Group -> Html Msg
viewGroup xs =
    let
        viewFunction t =
            ( t.id, viewCard t.id t.content )
    in
    Keyed.node "div"
        [ class "group-fullscreen" ]
        (List.map viewFunction xs)


viewCard : String -> String -> Html Msg
viewCard cardId content =
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card-fullscreen", True )
            , ( "active-fullscreen", True )
            ]
        ]
        [ textarea
            [ id ("card-edit-" ++ cardId)
            , dir "auto"
            , classList
                [ ( "edit-fullscreen", True )
                , ( "mousetrap", True )
                ]
            , value content
            ]
            []
        ]
