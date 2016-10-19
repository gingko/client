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
import TreeSort exposing (..)




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
        Ins oid parentId prevId_ nextId_ ts ->
          if Just tree.uid == parentId then
            let
              newTree =
                { uid = newUid parentId prevId_ nextId_
                , parentId = parentId
                , prev = prevId_
                , next = nextId_
                , content = (Content "" "" "" |> withContentId)
                , visible = True
                , children = Children []
                }

              sortedChildren = Children (sortTrees (children ++ [newTree]))
            in
              { tree
                | children = sortedChildren
              }
          else
              { tree | children = Children (List.map (update msg) children) }

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

    hasChildren =
      case tree.children of
        Children c ->
          ( c
              |> filterByVisible
              |> List.length
          ) /= 0
  in
  if isEditing then
    div [ id ("card-" ++ tree.uid)
        , classList [ ("card", True)
                    , ("active", True)
                    , ("editing", True)
                    , ("has-children", hasChildren)
                    ]
        ]
        [ div  [ class "view" 
                , onClick (Activate tree.uid)
                , onDoubleClick (OpenCard tree.uid tree.content.content)
                ] [ Markdown.toHtml [] vstate.field ]
        , textarea
            [ id ( "card-edit-" ++ tree.uid )
            , classList [ ("edit", True)
                        , ("mousetrap", True)
                        ]
            , value vstate.field
            --, onBlur CancelCard
            , onInput UpdateField
            ]
            []
        ]
  else
    div [ id ("card-" ++ tree.uid)
        , classList [ ("card", True)
                    , ("active", vstate.active == tree.uid)
                    , ("editing", False)
                    , ("has-children", hasChildren)
                    ]
        ]
        [ div  [ class "view" 
                , onClick (Activate tree.uid)
                , onDoubleClick (OpenCard tree.uid tree.content.content)
                ] [ Markdown.toHtml [] tree.content.content ]
        , textarea
            [ id ( "card-edit-" ++ tree.uid )
            , classList [ ("edit", True)
                        , ("mousetrap", True)
                        ]
            , value tree.content.content
            --, onBlur CancelCard
            , onInput UpdateField
            ]
            []
        ]

