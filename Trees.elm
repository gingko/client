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
  { id = "0"
  , content = ""
  , children = Children []
  }


blankTree : Int -> Tree
blankTree id =
  { id = toString id
  , content = ""
  , children = Children []
  }




-- UPDATE

type TreeMsg
  = NoOp
  | Ins Tree String Int
  | Upd String String
  | Mov Tree String Int
  | Del String



update : TreeMsg -> Tree -> Tree
update msg tree =
  case msg of
    NoOp -> tree

    Ins newTree parentId idx ->
      insertSubtree newTree parentId idx tree

    Upd id str ->
      modifyTree id (\t -> { t | content = str} ) tree

    Mov newTree parentId idx ->
      apply 
        [ Del newTree.id
        , Ins newTree parentId idx
        ]
      tree

    Del id ->
      pruneSubtree id tree


apply : List TreeMsg -> Tree -> Tree
apply msgs tree =
  List.foldl (\m t -> update m t) tree msgs


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
    isRoot = tree.id == "0"

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
        , onInput UpdateField
        ]
        []

    buttons =
      case (isEditing, isActive, isRoot) of
        ( False, True, False ) ->
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

        ( False, True, True ) ->
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
                  , onClick (OpenCard tree.id tree.content)
                  ]
                  []
                ]
          ]

        ( True, _, _ ) ->
          [ div [ class "flex-column card-right-overlay"]
                [ span 
                  [ class "card-btn save"
                  , title "Save Changes (Ctrl+Enter)"
                  , onClick (UpdateCard tree.id vstate.field)
                  ]
                  []
                ]
          ]

        _ ->
          []

    cardAttributes =
      [ id ("card-" ++ tree.id)
      , classList [ ("card", True)
                  , ("root", isRoot)
                  , ("active", isActive)
                  , ("editing", isEditing)
                  , ("has-children", hasChildren)
                  ]
      ]
  in
  if isEditing then
    div cardAttributes
        (
          [ tarea vstate.field ]
          ++
          buttons
        )
  else
    div cardAttributes
        (
          [ Markdown.toHtmlWith options 
                [ class "view" 
                , onClick (Activate tree.id)
                , onDoubleClick (OpenCard tree.id tree.content)
                ] tree.content
          , tarea tree.content
          ] ++
          buttons
        )

