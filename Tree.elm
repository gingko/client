module Tree exposing (..)

import String
import List.Extra as ListExtra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import Json.Decode as Json
import Markdown

import Types exposing (..)
import TreeUtils exposing (..)




-- MODEL

default : Tree
default =
  { uid = "0"
  , content = Content "" "" "" |> withContentId
  , parentId = Nothing
  , children = Children [] 
  , next = Nothing
  , prev = Nothing 
  , visible = True 
  }

blankTree : String -> Tree
blankTree uid =
  { default | uid = uid }




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
        Ins oid parentId_ prevId_ nextId_ ts ->
          let
            newTree =
              { uid = newUid parentId_ prevId_ nextId_ ts
              , parentId = parentId_
              , prev = prevId_
              , next = nextId_
              , content = (Content "" "" "" |> withContentId)
              , visible = True
              , children = Children []
              }
          in
          insertTree newTree parentId_ prevId_ nextId_ tree

        Upd oid uid str ts ->
          if tree.uid == uid then
            { tree | content = Content "" "" str |> withContentId }
          else
            { tree | children = Children (List.map (update (Apply (Upd oid uid str ts))) children) }

        Del oid uid ts ->
          if tree.uid == uid then
            { tree | visible = False }
          else
            { tree | children = Children (List.map (update (Apply (Del oid uid ts))) children) }

        Cpy oid uid parentId_ prevId_ nextId_ ts ->
          let
            oldTree =
              getTree tree uid ? default

            oldChildren = 
             case oldTree.children of
               Children c -> c

            newTree =
              { uid = newUid parentId_ prevId_ nextId_ ts
              , parentId = parentId_
              , prev = prevId_
              , next = nextId_
              , content = oldTree.content
              , visible = True
              , children =
                  oldChildren
                    |> List.map (\c -> { c | uid = newUid c.parentId c.prev c.next ts })
                    |> Children
              }
          in
          insertTree newTree parentId_ prevId_ nextId_ tree

        Mov oid uid parentId_ prevId_ nextId_ ts ->
          let
            treeToMove_ =
              getTree tree uid
          in
          case treeToMove_ of
            Nothing -> tree
            Just treeToMove ->
              tree
                |> pruneTree uid
                |> insertTree treeToMove parentId_ prevId_ nextId_ 




applyOperations : List Op -> Tree -> Tree
applyOperations ops tree =
  List.foldl applyOp tree ops


applyOp : Op -> Tree -> Tree
applyOp op tree =
  update (Apply op) tree




-- VIEW

view : ViewState -> Tree -> Html Msg
view vstate tree =
  let
    columns =
      [[[]]] ++ 
      getColumns([[[ tree ]]]) ++
      [[[]]]
        |> List.map (viewColumn vstate)
  in
  div [id "app" ]
    ( columns
    )




viewColumn : ViewState -> Column -> Html Msg
viewColumn vstate col =
  let
    buffer =
      [div [ class "buffer" ][]]
  in
  div
    [ class "column" ]
    ( buffer ++
      (List.map (lazy (viewGroup vstate)) col) ++
      buffer
    )
    


viewGroup : ViewState -> Group -> Html Msg
viewGroup vstate xs =
  let
    firstChild = 
      xs
        |> List.head
        |> Maybe.withDefault default
        |> .uid

    isActiveDescendant =
      vstate.descendants
        |> List.member firstChild
  in
    div [ classList [ ("group", True)
                    , ("active-descendant", isActiveDescendant)
                    ]
        ]
        (List.map (lazy (viewCard vstate)) xs)


viewCard : ViewState -> Tree -> Html Msg
viewCard vstate tree =
  let
    isEditing = vstate.editing == Just tree.uid
    isRoot = tree.uid == "0"
    isActive = vstate.active == tree.uid

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
              |> filterByVisible
              |> List.length
          ) /= 0

    tarea content =
      div 
        [ class "edit-wrap"]
        [ textarea
            [ id ( "card-edit-" ++ tree.uid )
            , classList [ ("edit", True)
                        , ("mousetrap", True)
                        ]
            , value content
            , onInput UpdateField
            ]
            []
        ]

    normalControls =
      if isActive then
        [ div [ class "flex-row card-top-overlay" ]
              [ span
                [ class "card-btn ins-above"
                , title "Insert Above (Ctrl+K)"
                , onClick (InsertAbove tree.uid)
                ]
                [ text "+" ]
              ]
        , div [ class "flex-column card-right-overlay"]
              [ span 
                [ class "card-btn delete"
                , title "Delete Card (Ctrl+Backspace)"
                , onClick (DeleteCard tree.uid)
                ]
                [ text "×" ]
              , span
                [ class "card-btn ins-right"
                , title "Add Child (Ctrl+L)"
                , onClick (InsertChild tree.uid)
                ]
                [ text "+" ]
              , span 
                [ class "card-btn edit"
                , title "Edit Card (Enter)"
                , onClick (OpenCard tree.uid tree.content.content)
                ]
                [ text "∆" ]
              ]
        , div [ class "flex-row card-bottom-overlay" ]
              [ span
                [ class "card-btn ins-below"
                , title "Insert Below (Ctrl+J)"
                , onClick (InsertBelow tree.uid)
                ]
                [ text "+" ]
              ]
        ]
      else
        []
  in
  if isEditing then
    div [ id ("card-" ++ tree.uid)
        , classList [ ("card", True)
                    , ("active", True)
                    , ("editing", isEditing)
                    , ("has-children", hasChildren)
                    ]
        ]
        [ tarea vstate.field
        , div [ class "flex-column card-right-overlay"]
              [ span 
                [ class "card-btn save"
                , title "Save Changes (Ctrl+Enter)"
                , onClick (UpdateCard tree.uid vstate.field)
                ]
                [ text "✔" ]
              ]
        ]
  else
    div [ id ("card-" ++ tree.uid)
        , classList [ ("card", True)
                    , ("active", isActive)
                    , ("editing", isEditing)
                    , ("has-children", hasChildren)
                    ]
        ]
        (
          [ div [ class "view" 
                , onClick (Activate tree.uid)
                , onDoubleClick (OpenCard tree.uid tree.content.content)
                ] [ Markdown.toHtmlWith options [] tree.content.content ]
          , tarea tree.content.content
          ] ++
          normalControls
        )

