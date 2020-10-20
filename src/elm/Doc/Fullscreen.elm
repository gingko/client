module Doc.Fullscreen exposing (Msg(..), view)

import Doc.TreeUtils exposing (getChildren, getColumnById, getParent, getTree)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onFocus)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2)
import Markdown
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


view : Language -> ViewState -> Model -> Html Msg
view _ vstate model =
    let
        parent_ =
            getParent vstate.active model.tree
                |> Maybe.map (\t -> ( t.id, t.content ))

        currentColumn =
            getColumnById vstate.active model.tree
                |> Maybe.withDefault []
    in
    div
        [ id "app"
        , class "fullscreen"
        ]
        [ div [ class "fullscreen-parent" ] [ viewMaybeParent parent_ ]
        , viewColumn OpenCard vstate.active currentColumn
        ]


viewMaybeParent : Maybe ( String, String ) -> Html msg
viewMaybeParent parentTuple_ =
    let
        options =
            { githubFlavored = Just { tables = True, breaks = True }
            , defaultHighlighting = Nothing
            , sanitize = False
            , smartypants = False
            }
    in
    case parentTuple_ of
        Just ( cardId, content ) ->
            div
                [ id ("card-" ++ cardId)
                , class "card"
                , dir "auto"
                ]
                [ Markdown.toHtmlWith options
                    []
                    content
                ]

        Nothing ->
            div [ class "fullscreen-parent" ] []


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
