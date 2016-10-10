module Tree exposing (..)

import Array exposing (Array)
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

type Msg
  = NoOp
  | Apply Op
  | Activate String
  | OpenCard String String
  | CancelCard
  | UpdateField String


update : Msg -> Tree -> Tree
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
        Ins oid parentId prevId_ nextId_ ->
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

        Upd oid uid str ->
          if tree.uid == uid then
            { tree | content = Content "" "" str |> withContentId }
          else
            { tree | children = Children (List.map (update (Apply (Upd oid uid str))) children) }

        Del oid uid ->
          if tree.uid == uid then
            { tree | visible = False }
          else
            { tree | children = Children (List.map (update (Apply (Del oid uid))) children) }

    _ ->
      tree


applyOperations : Array Op -> Tree -> Tree
applyOperations ops tree =
  Array.foldl applyOp tree ops


applyOp : Op -> Tree -> Tree
applyOp op tree =
  update (Apply op) tree




-- VIEW

viewColumn : ViewState -> Column -> Html Msg
viewColumn vstate col =
  div
    [ class "column" ]
    [ div
        [ class "buffer" ][]
    , div [](List.map (lazy (viewGroup vstate)) col)
    , div
        [ class "buffer" ][]
    ]


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
    hasChildren =
      case tree.children of
        Children [] -> False
        _ -> True
  in
    div [ id ("card-" ++ tree.uid)
        , classList [ ("card", True)
                    , ("active", vstate.active == tree.uid)
                    , ("editing", vstate.editing == Just tree.uid)
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
            , value vstate.field
            -- , onBlur CancelCard
            , onInput UpdateField
            ]
            []
        ]




--HELPERS

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then
        msg
      else NoOp
  in
    on "keydown" (Json.map tagger keyCode)
