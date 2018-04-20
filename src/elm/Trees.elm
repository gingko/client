module Trees exposing (..)

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
import TreeUtils exposing (getTree, getColumns, getParent, getChildren)
import List.Extra as ListExtra
import Sha1 exposing (Diff, diff3Merge)
import Html5.DragDrop as DragDrop



-- MODEL

type alias Model =
  { tree : Tree
  , columns : List Column
  }


defaultModel : Model
defaultModel =
  { tree = defaultTree
  , columns = [[[defaultTree]], [getChildren defaultTree] ]
  }


defaultTree : Tree
defaultTree =
  { id = "0"
  , content = ""
  , children = Children [Tree "1" "" (Children [])]
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
        |> apply (List.map (conflictToTreeMsg originalTree) conflicts)

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


conflictToTreeMsg : Tree -> Conflict -> TreeMsg
conflictToTreeMsg tree {id, opA, opB, selection, resolved} =
  case (id, opA, opB, selection, resolved) of
    (_, opA, _, Ours, False) ->
      opToTreeMsg tree opA

    (_, _, opB, Theirs, False) ->
      opToTreeMsg tree opB

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
                  Sha1.DiffOk strings ->
                    String.join "\n" strings

                  Sha1.DiffConflict (strAs, strOs, strBs) ->
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


opToTreeMsg : Tree -> Op -> TreeMsg
opToTreeMsg origTree op =
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

    Types.Mov tid opids oidx npids nidx ->
      case (getTree tid origTree, ListExtra.last npids) of
        (Just tree, Just pid) ->
          Mov tree pid nidx

        _ -> Nope





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
        |> List.drop 1

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
        vstate.parent
        vstate.dragModel
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

    lastChild =
      xs
        |> List.reverse
        |> List.head
        |> Maybe.withDefault defaultTree
        |> .id

    hasActive =
      xs
        |> List.map .id
        |> List.member vstate.active

    isActiveDescendant =
      vstate.descendants
        |> List.member firstChild

    viewFunction t =
      let
        isActive =
          t.id == vstate.active

        isParent =
          t.id == vstate.parent

        isEditing =
          case vstate.editing of
            Just editId ->
              t.id == editId

            Nothing ->
              False

        isLast =
          t.id == lastChild

        isCollabActive =
          vstate.collaborators
            |> List.map .mode
            |> List.member (Active t.id)

        collabsEditing =
          vstate.collaborators
            |> List.filter (\c -> c.mode == Editing t.id)
            |> List.map .uid

        collaborators =
          vstate.collaborators
            |> List.filter (\c -> c.mode == Active t.id || c.mode == Editing t.id)
            |> List.map .uid
      in
      viewKeyedCard (isActive, isParent, isEditing, depth, isLast, collaborators, collabsEditing, vstate.dragModel) t
  in
    Keyed.node "div"
      [ classList [ ("group", True)
                  , ("has-active", hasActive)
                  , ("active-descendant", isActiveDescendant)
                  ]
      ]
      (List.map viewFunction xs)


viewKeyedCard : (Bool, Bool, Bool, Int, Bool, List String, List String, DragDrop.Model String DropId) -> Tree -> (String, Html Msg)
viewKeyedCard tup tree =
  (tree.id, lazy2 viewCard tup tree)


viewCard : (Bool, Bool, Bool, Int, Bool, List String, List String, DragDrop.Model String DropId) -> Tree -> Html Msg
viewCard (isActive, isParent, isEditing, depth, isLast, collaborators, collabsEditing, dragModel) tree =
  let
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
      case (isEditing, isActive) of
        ( False, True ) ->
          [ div [ class "flex-row card-top-overlay"]
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

        ( True, _ ) ->
          [ div [ class "flex-column card-right-overlay"]
                [ span
                  [ class "card-btn save"
                  , title "Save Changes (Ctrl+Enter)"
                  , onClick (Port (Keyboard "mod+enter"))
                  ]
                  []
                ]
          ]

        _ ->
          []


    dropRegions =
      let
        dragId_ = DragDrop.getDragId dragModel

        dropId_ = DragDrop.getDropId dragModel

        dropDiv str dId =
          div
            ( [ classList
                  [ ("drop-region-"++str, True)
                  , ("drop-hover", dropId_ == Just dId )
                  ]
              ]
              ++ ( DragDrop.droppable DragDropMsg dId )
            )
            []
      in
      case dragId_ of
        Just dragId ->
          [ dropDiv "above" (Above tree.id)
          , dropDiv "into" (Into tree.id)
          ]
          ++ (if isLast then [ dropDiv "below" (Below tree.id) ] else [])

        Nothing ->
          []


    cardAttributes =
      [ id ("card-" ++ tree.id)
      , classList [ ("card", True)
                  , ("active", isActive)
                  , ("parent", isParent)
                  , ("editing", isEditing)
                  , ("collab-active", not isEditing && not (List.isEmpty collaborators) )
                  , ("collab-editing", not isEditing && not (List.isEmpty collabsEditing))
                  , ("has-children", hasChildren)
                  ]
      ]
      ++ (if not isEditing then DragDrop.draggable DragDropMsg tree.id else [])
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
        dropRegions ++
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

