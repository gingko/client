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

type Msg
  = NoOp
  | Activate String
  | UpdateCard String String
  | DeleteCard String
  | OpenCard String String
  | CancelCard
  | InsertBelow String
  | InsertChild String
  | Insert String (Maybe String) (Maybe String)
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

    UpdateCard uid str ->
      if tree.uid == uid then
        { tree | content = Content "" "" str |> withContentId }
      else
        { tree | children = Children (List.map (update (UpdateCard uid str)) children) }

    DeleteCard uid ->
      if tree.uid == uid then
        { tree | visible = False }
      else
        { tree | children = Children (List.map (update (DeleteCard uid)) children) }

    Insert parentId prevId_ nextId_ ->
      if tree.uid == parentId then
        let
          newTree =
            { uid = newUid (Just parentId) prevId_ nextId_
            , parentId = Just parentId
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
        

    InsertBelow uid ->
      update (Insert (getParent tree uid |> Maybe.withDefault "0" ) (Just uid) (getNext tree uid)) tree
    
    InsertChild uid ->
      if tree.uid == uid then
        { tree
          | children =
              children
                |> List.length
                |> nextUid uid
                |> blankTree
                |> ListExtra.singleton
                |> List.append children
                |> Children
        }
      else
        { tree
          | children = Children (List.map (update (InsertChild uid)) children) 
        }


    _ ->
      tree




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
  div [ class "group" ]
      (List.map (lazy (viewCard vstate)) xs)


viewCard : ViewState -> Tree -> Html Msg
viewCard vstate tree =
  let
    buttons =
      if (tree.uid /= "0") then
        [ button [ onClick (DeleteCard tree.uid) ][text "x"]
        , button [ onClick (InsertBelow tree.uid) ][text "+ below"]
        , button [ onClick (InsertChild tree.uid) ][text "+ child"]
        ]
      else
        [ button [ onClick (InsertChild tree.uid) ][text "+ child"]
        ]
  in
    div [ id ("card-" ++ tree.uid)
        , classList [ ("card", True)
                    , ("active", vstate.active == tree.uid)
                    , ("editing", vstate.editing == Just tree.uid)
                    ]
        , onClick (Activate tree.uid)
        , onDoubleClick (OpenCard tree.uid tree.content.content)
        ]
        ([ div [ class "view" ] [ Markdown.toHtml [] tree.content.content ]
        , textarea
            [ id ( "card-edit-" ++ tree.uid )
            , class "edit"
            , value vstate.field
            , onBlur CancelCard
            , onInput UpdateField
            , onEnter (UpdateCard tree.uid vstate.field)
            ]
            []
        ] ++ buttons)




-- SORTING WITH PARTIAL ORDER

-- ID GENERATING FUNCTIONS

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
