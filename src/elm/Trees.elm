module Trees exposing (..)

import Dict exposing (Dict)
import Tuple exposing (first, second)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Html.Keyed as Keyed
import Markdown

import Types exposing (..)
import TreeUtils exposing (getColumns, nodesToTree, dictUpdate, (?))
import List.Extra as ListExtra



-- MODEL

type alias Model =
  { tree : Tree
  , columns : List Column
  , nodes : Dict String TreeNode
  , pending : List (String, TreeNode)
  }


defaultModel : Model
defaultModel =
  { tree = defaultTree
  , columns = [[[defaultTree]]]
  , nodes = Dict.fromList [("0", TreeNode "" [] Nothing Nothing False)]
  , pending = []
  }


defaultTree : Tree
defaultTree =
  { id = "0"
  , content = ""
  , children = Children []
  , rev = Nothing
  }





-- UPDATE

type NodeMsg
  = Nope
  | Add String String Int
  | Rmv String (List String)
  | Mod String String
  | Mv String String Int
  | Responses (List (Result ResErr ResOk, (String, TreeNode)))
  | Change (String, TreeNode)
  | Conflicts (List (String, TreeNode))



update : NodeMsg -> Model -> Model
update msg model =
  case msg of
    Add id pid pos ->
      let
        parent_ = Dict.get pid model.nodes
      in
      case parent_ of
        Just parent ->
          { model
            | pending = model.pending
                ++ [(id, TreeNode "" [] Nothing Nothing False)]
                ++ [(pid, insertChild id pos parent)]
          }
            |> updateData

        Nothing ->
          model

    Rmv id descIds ->
      let
        node_ = Dict.get id model.nodes

        fMapFunc i =
          Dict.get i model.nodes
            |> Maybe.map (\tn -> (i, tn))

        allIds =
          id :: descIds

        desc = 
          descIds
            |> List.filterMap fMapFunc
            |> List.map (\(i, tn) -> (i, { tn | deletedWith = Just allIds }))
      in
      case node_ of
        Just node ->
          { model
            | pending = model.pending
                ++ [(id, { node | deletedWith = Just allIds })]
                ++ desc
          }
            |> updateData

        _ ->
          model

    Mod id str ->
      let
        node_ = Dict.get id model.nodes
      in
      case node_ of
        Just node ->
          { model
            | pending = model.pending
                ++ [(id, { node | content = str })]
          }
            |> updateData

        Nothing ->
          model

    Mv id newPid pos ->
      let
        newParent_ = Dict.get newPid model.nodes

        oldParent_ =
          getParentNodeEntry id model.nodes

        removeChild idToRemove treeNode =
          { treeNode
            | children =
                treeNode.children
                  |> List.filter (\(id, _) -> id /= idToRemove)
          }
      in
      case (oldParent_, newParent_) of
        (Just (oldPid, oldParent), Just newParent)->
          if (oldPid /= newPid) then
            { model
              | pending = model.pending
                  ++ [(oldPid, removeChild id oldParent)]
                  ++ [(newPid, insertChild id pos newParent)]
            }
              |> updateData
          else
            { model
              | pending = model.pending
                  ++ [(newPid, newParent |> removeChild id |> insertChild id pos)]
            }
              |> updateData

        _ ->
          model

    Responses responses ->
      let
        getNode (res, (id, tn)) =
          case res of
            Ok resOk -> Just (id, tn)
            Err _ -> Nothing

        getNewNode (res, (id, tn)) =
          case res of
            Ok resOk ->
              Just (id, {tn | rev = Just resOk.rev} )

            Err _ ->
              Nothing

        nodesToInsert =
          responses
            |> List.filterMap getNewNode
            |> Dict.fromList

        pendingToRemove =
          responses
            |> List.filterMap getNode
      in
      { model
        | nodes = Dict.union nodesToInsert model.nodes
        , pending = model.pending
            |> List.filter (\ptn -> not <| List.member ptn pendingToRemove)
      }
        |> updateData

    Change (id, node) ->
      let
        newNodes =
          Dict.insert id node model.nodes
      in
      if newNodes /= model.nodes then
        { model
          | nodes = newNodes
        }
          |> updateData
      else
        model

    Conflicts nodeList ->
      let
        resolvedConflicts =
          nodeList
            |> resolve model.nodes
      in
      { model
        | pending =
            model.pending ++ resolvedConflicts
      }
        |> updateData

    _ ->
      model


updateData : Model -> Model
updateData model =
  let
    tempNodes =
      Dict.union (model.pending |> Dict.fromList) model.nodes

    newTree =
      nodesToTree tempNodes "0"
        |> Maybe.withDefault defaultTree

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




-- ====== CONFLICT RESOLUTION ======

resolve : Dict String TreeNode -> List (String, TreeNode) -> List (String, TreeNode)
resolve nodes conflicts =
  case conflicts of
    head :: tail ->
      let
        anyNotDeleted =
          conflicts
            |> List.any (\(_, tn) -> tn.deletedWith == Nothing)

        (winnerId, winner) =
          List.foldl resolvePair head tail

        losingRevs =
          conflicts
            |> List.filter (\(_, tn) -> tn.rev /= winner.rev )
            |> List.map (\(i, tn) -> (i, { tn | deleted_ = True}) )

        toRestore =
          case (anyNotDeleted, winner.deletedWith) of
            (True, Just delIds) ->
              nodes
                |> Dict.filter (\i _ -> List.member i delIds)
                |> Dict.insert winnerId winner
                |> Dict.map (\_ tn -> { tn | deletedWith = Nothing } )
                |> Dict.toList

            _ ->
              [(winnerId, winner)]
      in
      toRestore ++ losingRevs

    [] ->
      []


resolvePair : (String, TreeNode) -> (String, TreeNode) -> (String, TreeNode)
resolvePair (aid, a) (bid, b) =
  case (a.deletedWith, b.deletedWith) of
    (Nothing, Just delIds) ->
      (aid, { a | deletedWith = Just delIds })

    (Just delIds, Nothing) ->
      (aid, { b | deletedWith = Just delIds })

    (Just delIdsA, Just delIdsB) ->
      (aid, TreeNode
        ( if a.content /= b.content then
            a.content ++ "\n=====CONFLICT=====\n" ++ b.content
          else
            a.content
        )
        ( a.children |> List.append b.children |> ListExtra.uniqueBy first )
        ( a.rev )
        ( delIdsA
          |> List.append delIdsB
          |> ListExtra.unique
          |> \dst -> if List.isEmpty dst then Nothing else Just dst
        )
        ( a.deleted_ && b.deleted_ )
      )

    (Nothing, Nothing) ->
      (aid, TreeNode
        ( if a.content /= b.content then
            a.content ++ "\n=====CONFLICT=====\n" ++ b.content
          else
            a.content
        )
        ( a.children |> List.append b.children |> ListExtra.uniqueBy first )
        ( a.rev )
        ( Nothing )
        ( a.deleted_ && b.deleted_ )
      )




-- ====== NODE UPDATES ======

insertChild : String -> Int -> TreeNode -> TreeNode
insertChild idToInsert idx treeNode =
  let
    visIdx_ =
      treeNode.children
        |> List.filter second
        |> ListExtra.getAt idx -- Maybe (String, Bool)
        |> Maybe.andThen (\sb -> ListExtra.elemIndex sb treeNode.children) -- Maybe Int

    ins i cs = (List.take i cs) ++ [(idToInsert, True)] ++ (List.drop i cs)
  in
  case visIdx_ of
    Just visIdx ->
      { treeNode
        | children = ins visIdx treeNode.children
      }

    Nothing ->
      { treeNode
        | children = ins idx treeNode.children
      }


getParentNodeEntry : String -> Dict String TreeNode -> Maybe (String, TreeNode)
getParentNodeEntry id nodes =
  nodes
    |> Dict.filter
        (\nid n ->
          n.children
            |> List.map first
            |> List.member id
        )
    |> Dict.toList
    |> List.head




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
      in
      viewKeyedCard (isActive, isEditing, depth) t
  in
    Keyed.node "div"
      [ classList [ ("group", True)
                  , ("active-descendant", isActiveDescendant)
                  ]
      ]
      (List.map viewFunction xs)


viewKeyedCard : (Bool, Bool, Int) -> Tree -> (String, Html Msg)
viewKeyedCard tup tree =
  (tree.id, lazy2 viewCard tup tree)


viewCard : (Bool, Bool, Int) -> Tree -> Html Msg
viewCard (isActive, isEditing, depth) tree =
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
    div cardAttributes
      (
        buttons ++
        [ div
            [ class "view"
            , onClick (Activate tree.id)
            , onDoubleClick (OpenCard tree.id tree.content)
            ]
            [( lazy viewContent tree.content )]
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
  in
  Markdown.toHtmlWith options
    [] content

