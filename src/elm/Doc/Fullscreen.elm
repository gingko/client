module Doc.Fullscreen exposing (Msg(..), view)

import Doc.TreeUtils exposing (getColumnById)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onFocus)
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



-- VIEW


view : Language -> ViewState -> Bool -> Model -> Html Msg
view _ vstate dirty model =
    let
        currentColumn =
            getColumnById vstate.active model.tree
                |> Maybe.withDefault []
    in
    div
        [ id "app"
        , class "fullscreen"
        ]
        [ viewColumn OpenCard vstate.active currentColumn
        , if dirty then
            div [ id "fullscreen-save-indicator" ] [ text "Unsaved changes..." ]

          else
            div [] []
        ]


viewColumn : (String -> String -> msg) -> String -> Column -> Html msg
viewColumn openCardFullscreen active col =
    div
        [ id "fullscreen-main" ]
        (List.map (lazy2 (viewGroup openCardFullscreen) active) col)


viewChildren : List Tree -> Html msg
viewChildren _ =
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
