module Fullscreen exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onFocus)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Translation exposing (Language, TranslationId(..), tr)
import TreeUtils exposing (getChildren, getColumnById, getParent, getTree)
import Types exposing (..)


type alias Model =
    { tree : Tree
    , columns : List Column
    }


view : (String -> String -> msg) -> Language -> ViewState -> Model -> Html msg
view openCardFullscreen lang vstate model =
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
        , class "fullscreen"
        ]
        [ viewMaybeParent parent_
        , viewColumn openCardFullscreen vstate.active currentColumn
        , viewChildren children
        ]


viewMaybeParent : Maybe ( String, String ) -> Html msg
viewMaybeParent parentTuple_ =
    case parentTuple_ of
        Just ( cardId, content ) ->
            div [ class "fullscreen-parent" ] [ text content ]

        Nothing ->
            div [ class "fullscreen-parent" ] []


viewColumn : (String -> String -> msg) -> String -> Column -> Html msg
viewColumn openCardFullscreen active col =
    div
        [ class "fullscreen-main" ]
        (List.map (lazy2 (viewGroup openCardFullscreen) active) col)


viewChildren : List Tree -> Html msg
viewChildren xs =
    div [ class "fullscreen-children" ] []


viewGroup : (String -> String -> msg) -> String -> Group -> Html msg
viewGroup openCardFullscreen active xs =
    let
        viewFunction t =
            ( t.id, viewCard openCardFullscreen (t.id == active) t.id t.content )
    in
    Keyed.node "div"
        [ class "group-fullscreen" ]
        (List.map viewFunction xs)


viewCard : (String -> String -> msg) -> Bool -> String -> String -> Html msg
viewCard openCardFullscreen isActive cardId content =
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card-fullscreen", True )
            , ( "active-fullscreen", isActive )
            ]
        ]
        [ textarea
            [ id ("card-edit-" ++ cardId)
            , dir "auto"
            , onFocus <| openCardFullscreen cardId content
            , classList
                [ ( "edit", True )
                , ( "mousetrap", True )
                ]
            , value content
            ]
            []
        ]
