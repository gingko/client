module Trees exposing (..)

import String
import List.Extra as ListExtra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import Json.Decode as Json
import Markdown

import Types exposing (..)



-- MODEL

defaultTree =
  { id = ""
  , content = defaultContent
  , parentId = Nothing
  , position = 0
  , children = Children []
  }



-- UPDATE

type TreeMsg
  = NoOp
  | Apply Op


update : TreeMsg -> Tree -> Tree
update msg tree =
  let
    children =
      case tree.children of
        Children trees -> trees

  in
  case msg of
    NoOp -> tree

    Apply op ->
      case op of
        Ins id content parentId_ position updated ->
          tree

        Del id ->
          tree



applyOperations : List Op -> Tree -> Tree
applyOperations ops tree =
  List.foldl applyOp tree ops


applyOp : Op -> Tree -> Tree
applyOp op tree =
  update (Apply op) tree




-- VIEW

view : ViewState -> List Tree -> Html Msg
view vstate trees =
  div [ id "app" 
      , classList [ ("editing", vstate.editing /= Nothing) ]
      ]
      []



viewColumn : ViewState -> Column -> Html Msg
viewColumn vstate col =
  let
    buffer =
      [div [ class "buffer" ][]]
  in
  div
    [ class "column" ]
    ( buffer ++
      buffer
    )
    


viewGroup : ViewState -> Group -> Html Msg
viewGroup vstate xs =
  let
    firstChild = 
      xs
        |> List.head
        |> Maybe.withDefault defaultTree
        |> .id

    isActiveDescendant =
      vstate.descendants
        |> List.member firstChild
  in
    div [ classList [ ("group", True)
                    , ("active-descendant", isActiveDescendant)
                    ]
        ]
        []


viewCard : ViewState -> Tree -> Html Msg
viewCard vstate tree =
  let
    isEditing = vstate.editing == Just tree.id
    isRoot = tree.id == "0"
    isActive = vstate.active == tree.id

    options =
      { githubFlavored = Just { tables = True, breaks = True }
      , defaultHighlighting = Nothing
      , sanitize = False
      , smartypants = False
      }

    hasChildren =
      case tree.children of
        Children c ->
          ( c
              |> List.length
          ) /= 0

    tarea content =
      textarea
        [ id ( "card-edit-" ++ tree.id )
        , classList [ ("edit", True)
                    , ("mousetrap", True)
                    ]
        , value content
        --, onBlur CancelCard
        , onInput UpdateField
        ]
        []

    normalControls =
      if isActive && not isRoot then
        [ div [ class "flex-row card-top-overlay" ]
              [ span
                [ class "card-btn ins-above"
                , title "Insert Above (Ctrl+K)"
                , onClick (InsertAbove tree.id)
                ]
                [ text "+" ]
              ]
        , div [ class "flex-column card-right-overlay"]
              [ span 
                [ class "card-btn delete"
                , title "Delete Card (Ctrl+Backspace)"
                , onClick (DeleteCard tree.id)
                ]
                []
              , span
                [ class "card-btn ins-right"
                , title "Add Child (Ctrl+L)"
                , onClick (InsertChild tree.id)
                ]
                [ text "+" ]
              , span 
                [ class "card-btn edit"
                , title "Edit Card (Enter)"
                , onClick (OpenCard tree.id tree.content.content)
                ]
                []
              ]
        , div [ class "flex-row card-bottom-overlay" ]
              [ span
                [ class "card-btn ins-below"
                , title "Insert Below (Ctrl+J)"
                , onClick (InsertBelow tree.id)
                ]
                [ text "+" ]
              ]
        ]
      else if isRoot then
        [ div [ class "flex-column card-right-overlay"]
              [ span
                [ class "card-btn ins-right"
                , title "Add Child (Ctrl+L)"
                , onClick (InsertChild tree.id)
                ]
                [ text "+" ]
              , span 
                [ class "card-btn edit"
                , title "Edit Card (Enter)"
                , onClick (OpenCard tree.id tree.content.content)
                ]
                []
              ]
        ]
      else
        []
  in
  if isEditing then
    div [ id ("card-" ++ tree.id)
        , classList [ ("card", True)
                    , ("active", True)
                    , ("editing", isEditing)
                    , ("root", isRoot)
                    , ("has-children", hasChildren)
                    ]
        ]
        [ tarea vstate.field
        , div [ class "flex-column card-right-overlay"]
              [ span 
                [ class "card-btn save"
                , title "Save Changes (Ctrl+Enter)"
                , onClick (UpdateCard tree.id vstate.field)
                ]
                []
              ]
        ]
  else
    div [ id ("card-" ++ tree.id)
        , classList [ ("card", True)
                    , ("active", isActive)
                    , ("editing", isEditing)
                    , ("root", isRoot)
                    , ("has-children", hasChildren)
                    ]
        ]
        (
          [ div [ class "view" 
                , onClick (Activate tree.id)
                , onDoubleClick (OpenCard tree.id tree.content.content)
                ] [ Markdown.toHtmlWith options [] tree.content.content ]
          , tarea tree.content.content
          ] ++
          normalControls
        )

