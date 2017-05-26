module Trees exposing (..)

import Dict exposing (Dict)
import Tuple exposing (first, second)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Html.Keyed as Keyed
import Markdown
import Diff exposing (..)
import Regex

import Types exposing (..)
import TreeUtils exposing (getColumns, getParent, getChildren)
import List.Extra as ListExtra
import Sha1 exposing (Diff, diff3Merge)



-- MODEL

type alias Model =
  { tree : Tree
  , columns : List Column
  }


defaultModel : Model
defaultModel =
  { tree = defaultTree
  , columns = [[[defaultTree]]]
  }


defaultTree : Tree
defaultTree =
  { id = "0"
  , content = ""
  , children = Children []
  }





-- UPDATE

type TreeMsg
  = Nope
  | Ins String String String Int
  | Upd String String
  | Mov Tree String Int
  | Rmv String



update : TreeMsg -> Model -> Model
update msg model =
  setTree (updateTree msg model.tree) model


updateTree : TreeMsg -> Tree -> Tree
updateTree msg tree =
  case msg of
    Ins newId newContent parentId idx ->
      insertSubtree (Tree newId newContent (Children [])) parentId idx tree

    Upd id str ->
      modifyTree id (\t -> { t | content = str} ) tree

    Mov newTree parentId idx ->
      tree
        |> pruneSubtree newTree.id 
        |> insertSubtree newTree parentId idx

    Rmv id ->
      pruneSubtree id tree

    Nope -> tree


setTree : Tree -> Model -> Model
setTree newTree model =
  let
    newColumns =
      if newTree /= model.tree then
        getColumns [[[newTree]]]
      else
        model.columns
  in
  { model
    | tree = newTree
    , columns = newColumns
  }


setTreeWithConflicts : List Conflict -> Tree -> Model -> Model
setTreeWithConflicts conflicts originalTree model =
  let
    newTree =
      originalTree
        |> apply (List.map conflictToTreeMsg conflicts)

    newColumns =
      if newTree /= model.tree then
        getColumns [[[newTree]]]
      else
        model.columns
  in
  { model
    | tree = newTree
    , columns = newColumns
  }


conflictToTreeMsg : Conflict -> TreeMsg
conflictToTreeMsg {id, opA, opB, selection, resolved} =
  case (id, opA, opB, selection, resolved) of
    (_, opA, _, Ours, False) ->
      opToTreeMsg opA

    (_, _, opB, Theirs, False) ->
      opToTreeMsg opB

    (_, Mod tid _ strA orig, Mod _ _ strB _, Manual, False) ->
      let
        tokenize s =
          Regex.split Regex.All (Regex.regex "(\\s+|\\b)") s -- List String

        changeMerge d ds =
          case (d, ds) of
            (NoChange a, (NoChange b) :: tail) ->
              NoChange (a ++ b) :: tail

            (Added a, (Added b) :: tail) ->
              Added (a ++ b) :: tail

            (Removed a, (Removed b) :: tail) ->
              Removed (a ++ b) :: tail

            (ch, list) ->
              ch :: list

        diffWords l r =
          diff (tokenize l) (tokenize r)
            |> List.foldr changeMerge []
            |> List.map
              (\c ->
                case c of
                  NoChange s -> s
                  Added s -> "{++" ++ s ++ "++}"
                  Removed s -> "{--" ++ s ++ "--}"
              )
            |> String.join ""

        diffLinesString l r =
          diffLines l r
            |> List.map
              (\c ->
                case c of
                  NoChange s -> s
                  Added s -> "{++" ++ s ++ "++}"
                  Removed s -> "{--" ++ s ++ "--}"
              )
            |> String.join "\n"

        mergedString : String
        mergedString =
          diff3Merge (String.lines strA) (String.lines orig) (String.lines strB)
            |> List.map
              (\c ->
                case c of
                  Sha1.Ok strings ->
                    String.join "\n" strings

                  Sha1.Conflict (strAs, strOs, strBs) ->
                    "\n`>>>>>>>`\n" ++
                    (String.join "\n" strAs) ++
                    "\n`=======`\n" ++
                    (String.join "\n" strBs) ++
                    "\n`<<<<<<<`\n"
              )
            |> String.join "\n"

        manualString =
          "`Your version:`\n" ++
          (diffLinesString orig strA) ++
          "\n\n--------\n`Their version:`\n" ++
          (diffLinesString orig strB)
      in
      Upd tid mergedString

    _ ->
      Nope


opToTreeMsg : Op -> TreeMsg
opToTreeMsg op =
  case op of
    Mod tid _ str _ ->
      Upd tid str

    Del tid _ ->
      Rmv tid

    Types.Ins id str pids idx ->
      case ListExtra.last pids of
        Just pid ->
          Ins id str pid idx

        Nothing ->
          Nope

    -- TODO: Mov Op -> Mov TreeMsg
    _ ->
      Nope





-- TREE TRANSFORMATIONS

apply : List TreeMsg -> Tree -> Tree
apply msgs tree =
  List.foldl (\m t -> updateTree m t) tree msgs


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

view : ViewState -> Model -> Html Msg
view vstate model =
  let
    columnsWithDepth =
      model.columns
        |> List.indexedMap (\i c -> (c, i))

    getViewArgs cwd =
      let
        editing_ =
          case vstate.editing of
            Nothing ->
              Nothing

            Just editId ->
              if (first cwd |> List.concat |> List.map .id |> List.member editId ) then
                Just editId
              else
                Nothing
      in
      VisibleViewState
        vstate.active
        editing_
        vstate.descendants
        vstate.collaborators

    columns =
      [([[]], -1)] ++
      columnsWithDepth ++
      [([[]], List.length columnsWithDepth)]
        |> List.map (\t -> lazy3 viewColumn (getViewArgs t) (second t) (first t))
  in
  div [ id "app"
      ]
    ( columns
    )


viewColumn : VisibleViewState -> Int -> Column -> Html Msg
viewColumn vstate depth col =
  let
    buffer =
      [div [ class "buffer" ][]]
  in
  div
    [ class "column" ]
    ( buffer ++
      (List.map (lazy3 viewGroup vstate depth) col) ++
      buffer
    )


viewGroup : VisibleViewState -> Int -> Group -> Html Msg
viewGroup vstate depth xs =
  let
    firstChild =
      xs
        |> List.head
        |> Maybe.withDefault defaultTree
        |> .id

    isActiveDescendant =
      vstate.descendants
        |> List.member firstChild

    viewFunction t =
      let
        isActive =
          t.id == vstate.active

        isEditing =
          case vstate.editing of
            Just editId ->
              t.id == editId

            Nothing ->
              False

        isCollabActive =
          vstate.collaborators
            |> List.map .active
            |> List.member t.id

        collabsEditing =
          vstate.collaborators
            |> List.filter .editing
            |> List.filter (\c -> c.active == t.id)
            |> List.map .uid

        collaborators =
          if isCollabActive then
            vstate.collaborators
              |> List.filter (\c -> c.active == t.id)
              |> List.map .uid
          else
            []
      in
      viewKeyedCard (isActive, isEditing, depth, collaborators, collabsEditing) t
  in
    Keyed.node "div"
      [ classList [ ("group", True)
                  , ("active-descendant", isActiveDescendant)
                  ]
      ]
      (List.map viewFunction xs)


viewKeyedCard : (Bool, Bool, Int, List String, List String) -> Tree -> (String, Html Msg)
viewKeyedCard tup tree =
  (tree.id, lazy2 viewCard tup tree)


viewCard : (Bool, Bool, Int, List String, List String) -> Tree -> Html Msg
viewCard (isActive, isEditing, depth, collaborators, collabsEditing) tree =
  let
    isRoot = tree.id == "0"


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
        , defaultValue content
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
                  , onClick (GetContentToSave tree.id)
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
                  , ("collab-active", not isEditing && not (List.isEmpty collaborators) )
                  , ("collab-editing", not isEditing && not (List.isEmpty collabsEditing))
                  , ("has-children", hasChildren)
                  ]
      ]
  in
  if isEditing then
    div cardAttributes
      (
        [ tarea tree.content ]
        ++
        buttons
      )
  else
    let
      collabsString =
        collaborators
          |> List.map (\c -> if List.member c collabsEditing then c ++ " is editing" else c)
          |> String.join(", ")
    in
    div cardAttributes
      (
        buttons ++
        [ div
            [ class "view"
            , onClick (Activate tree.id)
            , onDoubleClick (OpenCard tree.id tree.content)
            ]
            [( lazy viewContent tree.content )]
        , span [ class "collaborators" ] [text collabsString]
        ]
      )


viewContent : String -> Html Msg
viewContent content =
  let
    options =
      { githubFlavored = Just { tables = True, breaks = True }
      , defaultHighlighting = Nothing
      , sanitize = False
      , smartypants = False
      }

    processedContent =
      content
        |> Regex.replace Regex.All (Regex.regex "{\\+\\+") (\_ -> "<ins class='diff'>")
        |> Regex.replace Regex.All (Regex.regex "\\+\\+}") (\_ -> "</ins>")
        |> Regex.replace Regex.All (Regex.regex "{--") (\_ -> "<del class='diff'>")
        |> Regex.replace Regex.All (Regex.regex "--}") (\_ -> "</del>")
  in
  Markdown.toHtmlWith options
    [] processedContent

