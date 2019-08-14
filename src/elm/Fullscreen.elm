module Fullscreen exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Translation exposing (Language, TranslationId(..), tr)
import Types exposing (..)


type alias Model =
    { tree : Tree
    , columns : List Column
    }


view : Language -> ViewState -> Model -> Html Msg
view lang vstate model =
    div
        [ id "app"
        ]
        (List.map viewColumn model.columns)


viewColumn : Column -> Html Msg
viewColumn col =
    div
        [ class "column-fullscreen" ]
        (List.map (lazy viewGroup) col)


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
