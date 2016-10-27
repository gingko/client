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
import TreeUtils exposing (getColumns, getChildren, getParent)



-- MODEL

defaultTree =
  { id = "defaultTree"
  , content = "Content fo defaultTree"
  , children = Children []
  }




-- UPDATE

type TreeMsg
  = NoOp
  | Ins Tree String Int
  | Del String



update : TreeMsg -> Tree -> Tree
update msg tree =
  case msg of
    NoOp -> tree

    Ins newTree parentId idx ->
      insertSubtree newTree parentId idx tree

    Del id ->
      pruneSubtree id tree

insertSubtree : Tree -> String -> Int -> Tree -> Tree
insertSubtree subtree parentId idx tree =
  let
    fn = (\c -> (List.take idx c) ++ [subtree] ++ (List.drop idx c))
  in
  modifyChildren parentId fn tree


pruneSubtree : String -> Tree -> Tree
pruneSubtree id tree =
  modifySiblings id (\c -> List.filter (\x -> x.id /= id) c) tree


modifyTree : String -> (Tree -> Tree) -> Tree -> Tree
modifyTree id upd tree =
  if tree.id == id then
    upd tree
  else
    { tree
      | children =
          getChildren tree
            |> List.map (modifyTree id upd)
            |> Children
    }


modifyChildren : String -> (List Tree -> List Tree) -> Tree -> Tree
modifyChildren pid upd tree =
  if tree.id == pid then
    { tree
      | children =
          getChildren tree
            |> upd
            |> Children
    }
  else
    { tree
      | children =
          getChildren tree
            |> List.map (modifyChildren pid upd)
            |> Children
    }


modifySiblings : String -> (List Tree -> List Tree) -> Tree -> Tree
modifySiblings id upd tree =
  case getParent id tree of
    Nothing ->
      tree
    Just parentTree ->
      modifyChildren parentTree.id upd tree




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
  div [ id "app" 
      , classList [ ("editing", vstate.editing /= Nothing) ]
      ]
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
        (List.map (lazy (viewCard vstate)) xs)


viewCard : ViewState -> Tree -> Html Msg
viewCard vstate tree =
  let
    isEditing = vstate.editing == Just tree.id
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
      if isActive then
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
                , onClick (OpenCard tree.id tree.content)
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
      else
        []
  in
  if isEditing then
    div [ id ("card-" ++ tree.id)
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
                    , ("has-children", hasChildren)
                    ]
        ]
        (
          [ div [ class "view" 
                , onClick (Activate tree.id)
                , onDoubleClick (OpenCard tree.id tree.content)
                ] [ Markdown.toHtmlWith options [] tree.content ]
          , tarea tree.content
          ] ++
          normalControls
        )

