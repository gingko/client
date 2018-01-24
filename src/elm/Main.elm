port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Json.Encode exposing (..)
import Regex exposing (Regex, replace, regex)
import Dom
import Task
--import Time
import Diff exposing (..)
import InlineHover exposing (hover)

import Types exposing (..)
import Trees exposing (..)
import TreeUtils exposing (..)
import Sha1 exposing (timestamp, timeJSON)
import Objects
import Coders exposing (..)

import Html5.DragDrop as DragDrop


main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port js : (String, Json.Value) -> Cmd msg
port activateCards : (Int, List (List String)) -> Cmd msg
port getText : String -> Cmd msg
port saveObjects : (Json.Value, Json.Value) -> Cmd msg
port saveLocal : Json.Value -> Cmd msg
port updateCommits : (Json.Value, Maybe String) -> Cmd msg




-- MODEL


type alias Model =
  { workingTree : Trees.Model
  , objects : Objects.Model
  , status : Status
  , uid : String
  , viewState : ViewState
  , startingWordcount : Int
  , online : Bool
  , filepath : Maybe String
  , changed : Bool
  }


defaultModel : Model
defaultModel =
  { workingTree = Trees.defaultModel
  , objects = Objects.defaultModel
  , status = Bare
  , uid = timeJSON ()
  , viewState =
      { active = "1"
      , activePast = []
      , activeFuture = []
      , descendants = []
      , editing = Nothing
      , dragModel = DragDrop.init
      , draggedTree = Nothing
      , collaborators = []
      }
  , startingWordcount = 0
  , online = True
  , filepath = Nothing
  , changed = False
  }


init : (Model, Cmd Msg)
init =
  defaultModel ! [focus "1"]
    |> activate "1"




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({objects, workingTree, status} as model) =
  let
    vs = model.viewState
  in
  case msg of
    -- === Card Activation ===

    Activate id ->
      case vs.editing of
        Just eid ->
          model ! [ getText eid ]
            |> cancelCard
            |> activate id

        Nothing ->
          model ! []
            |> activate id

    -- === Card Editing  ===

    OpenCard id str ->
      model ! []
        |> openCard id str

    UpdateContent (id, str) ->
      let
        newTree = Trees.update (Trees.Upd id str) model.workingTree
      in
      if newTree.tree /= model.workingTree.tree then
        { model
          | workingTree = newTree
        }
          ! []
          |> addToHistory
          |> sendCollabState (CollabState model.uid (Active id) "")
      else
        model
          ! []
          |> sendCollabState (CollabState model.uid (Active id) "")

    DeleteCard id ->
      model ! []
        |> deleteCard id

    CancelCard ->
      model ! []
        |> cancelCard

    -- === Card Insertion  ===

    InsertAbove id ->
      model ! []
        |> insertAbove id

    InsertBelow id ->
      model ! []
        |> insertBelow id

    InsertChild id ->
      model ! []
        |> insertChild id

    -- === Card Moving  ===

    DragDropMsg dragDropMsg ->
      let
        ( newDragModel, dragResult_ ) =
          DragDrop.update dragDropMsg vs.dragModel
      in
      case (vs.draggedTree, DragDrop.getDragId newDragModel, dragResult_ ) of
        -- Start drag
        ( Nothing, Just dragId, Nothing ) ->
          { model
            | workingTree = Trees.update (Trees.Rmv dragId) model.workingTree
            , viewState =
              { vs
                | dragModel = newDragModel
                , draggedTree = getTreeWithPosition dragId model.workingTree.tree
              }
          }
          ! []

        -- Successful drop
        ( Just (draggedTree, _, _), Nothing, Just (dragId, dropId) ) ->
          let
            moveOperation =
              case dropId of
                Into id ->
                  move draggedTree id 999999

                Above id ->
                  move draggedTree
                    ( ( getParent id model.workingTree.tree |> Maybe.map .id ) ? "0" )
                    ( ( getIndex id model.workingTree.tree ? 0 ) |> Basics.max 0 )

                Below id ->
                  move draggedTree
                    ( ( getParent id model.workingTree.tree |> Maybe.map .id ) ? "0" )
                    ( ( getIndex id model.workingTree.tree ? 0 ) + 1)
          in
          { model | viewState =
            { vs
              | dragModel = newDragModel
              , draggedTree = Nothing
            }
          } ! []
            |> moveOperation
            |> activate draggedTree.id

        -- Failed drop
        ( Just (draggedTree, parentId, idx), Nothing, Nothing ) ->
          { model | viewState =
            { vs
              | dragModel = newDragModel
              , draggedTree = Nothing
            }
          } ! []
            |> move draggedTree parentId idx
            |> activate draggedTree.id

        _ ->
          { model | viewState = { vs | dragModel = newDragModel } } ! []

    -- === History ===

    Undo ->
      model ! []

    Redo ->
      model ! []

    Pull ->
      case (model.status, model.online) of
        (Clean _, True) ->
          model ! [js ("pull", null)]

        (Bare, True) ->
          model ! [js ("pull", null)]

        _ ->
          model ! []

    SetSelection cid selection id ->
      let
        newStatus =
          case status of
            MergeConflict mTree oldHead newHead conflicts ->
              conflicts
                |> List.map (\c -> if c.id == cid then { c | selection = selection } else c)
                |> MergeConflict mTree oldHead newHead

            _ ->
              status

      in
      case newStatus of
        MergeConflict mTree oldHead newHead conflicts ->
          case selection of
            Manual ->
              { model
                | workingTree = Trees.setTreeWithConflicts conflicts mTree model.workingTree
                , status = newStatus
              }
                ! []

            _ ->
              { model
                | workingTree = Trees.setTreeWithConflicts conflicts mTree model.workingTree
                , status = newStatus
              }
                ! []
                |> cancelCard
                |> activate id

        _ ->
          model ! []

    Resolve cid ->
      case status of
        MergeConflict mTree shaA shaB conflicts ->
          { model
            | status = MergeConflict mTree shaA shaB (conflicts |> List.filter (\c -> c.id /= cid))
          }
            ! []
            |> addToHistory

        _ ->
          model ! []

    CheckoutCommit commitSha ->
      case status of
        MergeConflict _ _ _ _ ->
          model ! []

        _ ->
          let
            (newStatus, newTree_, newModel) =
              Objects.update (Objects.Checkout commitSha) objects
          in
          case newTree_ of
            Just newTree ->
              { model
                | workingTree = Trees.setTree newTree model.workingTree
                , status = newStatus
              }
                ! [ updateCommits (objects |> Objects.toValue, getHead newStatus) ]

            Nothing ->
              model ! []
                |> Debug.log "failed to load commit"



    -- === Ports ===

    Load (filepath, json) ->
      let
        (newStatus, newTree_, newObjects) =
            Objects.update (Objects.Init json) objects

        startingWordcount =
          newTree_
            |> Maybe.map (\t -> countWords (treeToMarkdownString t))
            |> Maybe.withDefault 0
      in
      case (newStatus, newTree_) of
        (Clean newHead, Nothing) -> -- no changes to Tree
          { model
            | status = newStatus
            , startingWordcount = startingWordcount
            , filepath = filepath
            , changed = False
          }
            ! [ updateCommits (newObjects |> Objects.toValue, getHead newStatus) ]
            |> activate model.viewState.active

        (Clean newHead, Just newTree) ->
          { model
            | workingTree = Trees.setTree newTree model.workingTree
            , objects = newObjects
            , status = newStatus
            , startingWordcount = startingWordcount
            , filepath = filepath
            , changed = False
          }
            ! [ updateCommits (newObjects |> Objects.toValue, getHead newStatus) ]
            |> activate model.viewState.active

        (MergeConflict mTree oldHead newHead [], Just newTree) ->
          { model
            | workingTree = Trees.setTree newTree model.workingTree
            , objects = newObjects
            , status = newStatus
            , startingWordcount = startingWordcount
            , filepath = filepath
            , changed = False
          }
            ! [ updateCommits (newObjects |> Objects.toValue, getHead newStatus) ]
            |> activate model.viewState.active

        (MergeConflict mTree oldHead newHead conflicts, Just newTree) ->
          { model
            | workingTree = Trees.setTreeWithConflicts conflicts mTree model.workingTree
            , objects = newObjects
            , status = newStatus
            , startingWordcount = startingWordcount
            , filepath = filepath
            , changed = False
          }
            ! [ updateCommits (newObjects |> Objects.toValue, getHead newStatus) ]
            |> activate model.viewState.active

        _ ->
          let _ = Debug.log "failed to load json" (newStatus, newTree_, newObjects, json) in
          model ! []

    MergeIn json ->
      let
        (newStatus, newTree_, newObjects) =
          Objects.update (Objects.Merge json workingTree.tree) objects
      in
      case (status, newStatus) of
        (Bare, Clean sha) ->
          { model
            | workingTree = Trees.setTree (newTree_ ? workingTree.tree) workingTree
            , objects = newObjects
            , status = newStatus
          }
            ! [ updateCommits (newObjects |> Objects.toValue, Just sha) ]
            |> activate vs.active

        (Clean oldHead, Clean newHead) ->
          if (oldHead /= newHead) then
            { model
              | workingTree = Trees.setTree (newTree_ ? workingTree.tree) workingTree
              , objects = newObjects
              , status = newStatus
            }
              ! [ updateCommits (newObjects |> Objects.toValue, Just newHead) ]
              |> activate vs.active
          else
            model ! []

        (Clean _, MergeConflict mTree oldHead newHead conflicts) ->
          { model
            | workingTree =
                if (List.isEmpty conflicts) then
                  Trees.setTree (newTree_ ? workingTree.tree) workingTree
                else
                  Trees.setTreeWithConflicts conflicts mTree model.workingTree
            , objects = newObjects
            , status = newStatus
          }
            ! [ updateCommits (newObjects |> Objects.toValue, Just newHead) ]
            |> addToHistory
            |> activate vs.active

        _ ->
          let _ = Debug.log "failed to merge json" json in
          model ! []

    ImportJson json ->
      case Json.decodeValue treeDecoder json of
        Ok newTree ->
          let
            (newStatus, _, newObjects) =
              Objects.update (Objects.Commit [] "Jane Doe <jane.doe@gmail.com>" newTree) model.objects
          in
          { defaultModel
            | workingTree = Trees.setTree newTree model.workingTree
            , objects = newObjects
            , status = newStatus
            , changed = True
          }
            ! [ saveObjects (newStatus |> statusToValue, newObjects |> Objects.toValue)
              , updateCommits ( newObjects |> Objects.toValue, getHead newStatus)
              ]

        Err err ->
          let _ = Debug.log "ImportJson error" err in
          model ! []

    SetHeadRev rev ->
      { model
        | objects = Objects.setHeadRev rev model.objects
      }
        ! []
        |> push

    RecvCollabState json ->
      case Json.decodeValue collabStateDecoder json of
        Ok collabState ->
          let
            newCollabs =
              if List.member collabState.uid (vs.collaborators |> List.map .uid) then
                vs.collaborators |> List.map (\c -> if c.uid == collabState.uid then collabState else c)
              else
                collabState :: vs.collaborators

            newTree =
              case collabState.mode of
                Editing editId ->
                  Trees.update (Trees.Upd editId collabState.field) model.workingTree

                _ -> model.workingTree
          in
          { model
            | workingTree = newTree
            , viewState = { vs | collaborators = newCollabs }
          }
            ! []

        Err err ->
          let
            _ = Debug.log "collabState ERROR" err
          in
          model ! []

    CollaboratorDisconnected uid ->
      { model
        | viewState =
            { vs | collaborators = vs.collaborators |> List.filter (\c -> c.uid /= uid)}
      }
        ! []

    HandleKey key ->
      case key of
        "mod+x" ->
          let _ = Debug.log "model" model in
          model ! []

        "mod+enter" ->
          case vs.editing of
            Nothing ->
              model ! []

            Just uid ->
              model ! [getText uid]
                |> cancelCard
                |> activate uid

        "enter" ->
          normalMode model (openCard vs.active (getContent vs.active model.workingTree.tree))

        "mod+backspace" ->
          normalMode model (deleteCard vs.active)

        "esc" ->
          model |> intentCancelCard

        "mod+j" ->
          model |> maybeSaveAndThen (insertBelow vs.active)

        "mod+down" ->
          normalMode model (insertBelow vs.active)

        "mod+k" ->
          model |> maybeSaveAndThen (insertAbove vs.active)

        "mod+up" ->
          normalMode model (insertAbove vs.active)

        "mod+l" ->
          model |> maybeSaveAndThen (insertChild vs.active)

        "mod+right" ->
          normalMode model (insertChild vs.active)

        "h" ->
          normalMode model (goLeft vs.active)

        "left" ->
          normalMode model (goLeft vs.active)

        "j" ->
          normalMode model (goDown vs.active)

        "down" ->
          normalMode model (goDown vs.active)

        "k" ->
          normalMode model (goUp vs.active)

        "up" ->
          normalMode model (goUp vs.active)

        "l" ->
          normalMode model (goRight vs.active)

        "right" ->
          normalMode model (goRight vs.active)

        "alt+up" ->
          normalMode model (moveWithin vs.active -1)

        "alt+k" ->
          normalMode model (moveWithin vs.active -1)

        "alt+down" ->
          normalMode model (moveWithin vs.active 1)

        "alt+j" ->
          normalMode model (moveWithin vs.active 1)

        "alt+left" ->
          normalMode model (moveLeft vs.active)

        "alt+h" ->
          normalMode model (moveLeft vs.active)

        "alt+right" ->
          normalMode model (moveRight vs.active)

        "alt+l" ->
          normalMode model (moveRight vs.active)

        "alt+shift+up" ->
          normalMode model (moveWithin vs.active -5)

        "alt+shift+down" ->
          normalMode model (moveWithin vs.active 5)

        "alt+home" ->
          normalMode model (moveWithin vs.active -999999)

        "alt+end" ->
          normalMode model (moveWithin vs.active 999999)

        "mod+z" ->
          model ! []

        "mod+r" ->
          model ! []

        "mod+n" ->
          model ! []
            |> intentNew

        "mod+s" ->
          model |> maybeSaveAndThen intentSave

        "mod+o" ->
          model ! []
            |> intentOpen

        _ ->
          model ! []

    ExternalMessage (cmd, arg) ->
      case cmd of
        "new" ->
          init

        "saved" ->
          { model
            | filepath = Just arg
            , changed = False
          }
            ! [js ("saved", arg |> string)]

        "changed" ->
          { model
            | changed = True
          }
            ! []

        "export-json" ->
          model
            ! [js ("export-json", model.workingTree.tree |> treeToJSON )]

        "export-txt" ->
          model
            ! [js ("export-txt", model.workingTree.tree |> treeToMarkdown )]

        _ ->
          let _ = Debug.log "Unknown external command" cmd in
          model ! []

    NoOp ->
      model ! []




-- === Card Activation ===

activate : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
activate id (model, prevCmd) =
  let vs = model.viewState in
  if id == "0" then
    model ! [ prevCmd ]
  else
    let
      activeTree_ = getTree id model.workingTree.tree
      newPast =
        if (id == vs.active) then
          vs.activePast
        else
          vs.active :: vs.activePast |> List.take 40
    in
    case activeTree_ of
      Just activeTree ->
        let
          desc =
            activeTree
              |> getDescendants
              |> List.map .id

          anc =
            getAncestors model.workingTree.tree activeTree []
              |> List.map .id

          flatCols =
            model.workingTree.columns
              |> List.map (\c -> List.map (\g -> List.map .id g) c)
              |> List.map List.concat

          allIds =
            anc
            ++ [id]
            ++ desc
        in
        { model
          | viewState =
              { vs
                | active = id
                , activePast = newPast
                , activeFuture = []
                , descendants = desc
              }
        }
          ! [ prevCmd
            , activateCards
                ( getDepth 0 model.workingTree.tree id
                , centerlineIds flatCols allIds newPast
                )
            ]
            |> sendCollabState (CollabState model.uid (Active id) "")

      Nothing ->
        model ! [ prevCmd ]


goLeft : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goLeft id (model, prevCmd) =
  let
    targetId =
      getParent id model.workingTree.tree ? defaultTree |> .id
  in
  model ! [prevCmd]
    |> activate targetId


goDown : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goDown id (model, prevCmd) =
  let
    targetId =
      case getNextInColumn id model.workingTree.tree of
        Nothing -> id
        Just ntree -> ntree.id
  in
  model ! [prevCmd]
    |> activate targetId


goUp : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goUp id (model, prevCmd) =
  let
    targetId =
      case getPrevInColumn id model.workingTree.tree of
        Nothing -> id
        Just ntree -> ntree.id
  in
  model ! [prevCmd]
    |> activate targetId


goRight : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goRight id (model, prevCmd) =
  let
    vs = model.viewState

    tree_ =
      getTree id model.workingTree.tree

    childrenIds =
      getChildren (tree_ ? defaultTree)
        |> List.map .id

    firstChildId =
      childrenIds
        |> List.head
        |> Maybe.withDefault id

    prevActiveOfChildren =
      vs.activePast
        |> List.filter (\a -> List.member a childrenIds)
        |> List.head
        |> Maybe.withDefault firstChildId
  in
  case tree_ of
    Nothing ->
      model ! [prevCmd]

    Just tree ->
      if List.length childrenIds == 0 then
        model ! [prevCmd]
      else
        model ! [prevCmd]
          |> activate prevActiveOfChildren


-- === Card Editing  ===

openCard : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
openCard id str (model, prevCmd) =
  let
    vs = model.viewState
    isLocked =
      vs.collaborators
        |> List.filter (\c -> c.mode == Editing id)
        |> (not << List.isEmpty)
  in
  if isLocked then
    model ! [prevCmd, js ("alert", string "Card is being edited by someone else.")]
  else
    { model
      | viewState = { vs | active = id, editing = Just id }
    }
      ! [ prevCmd, focus id ]
      |> sendCollabState (CollabState model.uid (Editing id) str)


deleteCard : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
deleteCard id (model, prevCmd) =
  let
    vs = model.viewState

    isLocked =
      vs.collaborators
        |> List.filter (\c -> c.mode == Editing id)
        |> (not << List.isEmpty)

    filteredActive =
      vs.activePast
        |> List.filter (\a -> a /= id)

    parent_ = getParent id model.workingTree.tree
    prev_ = getPrevInColumn id model.workingTree.tree
    next_ = getNextInColumn id model.workingTree.tree

    (nextToActivate, isLastChild) =
      case (parent_, prev_, next_) of
        (_, Just prev, _) ->
          (prev.id, False)

        (_, Nothing, Just next) ->
          (next.id, False)

        (Just parent, Nothing, Nothing) ->
          (parent.id, parent.id == "0")

        (Nothing, Nothing, Nothing) ->
          ("0", True)
  in
  if isLocked then
    model ! [js ("alert", string "Card is being edited by someone else.")]
  else if isLastChild then
    model ! [js ("alert", string "Cannot delete last card.")]
  else
    { model
      | workingTree = Trees.update (Trees.Rmv id) model.workingTree
    }
      ! []
      |> activate nextToActivate
      |> addToHistory


cancelCard : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
cancelCard (model, prevCmd) =
  let vs = model.viewState in
  { model
    | viewState = { vs | editing = Nothing }
  }
    ! [prevCmd]
    |> sendCollabState (CollabState model.uid (Active vs.active) "")


intentCancelCard : Model -> ( Model, Cmd Msg )
intentCancelCard model =
  let
    vs = model.viewState
    originalContent = getContent vs.active model.workingTree.tree
  in
  case vs.editing of
    Nothing ->
      model ! []

    Just id ->
      model ! [js ("confirm-cancel", Json.Encode.list [string vs.active, string originalContent])]


-- === Card Insertion  ===

insert : String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insert pid pos (model, prevCmd) =
  let
    newId = "node-" ++ (timestamp () |> toString)
  in
  { model
    | workingTree = Trees.update (Trees.Ins newId "" pid pos) model.workingTree
  }
    ! [prevCmd]
    |> openCard newId ""
    |> activate newId


insertRelative : String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertRelative id delta (model, prevCmd) =
  let
    idx =
      getIndex id model.workingTree.tree ? 999999

    pid_ =
      getParent id model.workingTree.tree |> Maybe.map .id
  in
  case pid_ of
    Just pid ->
      model ! [prevCmd]
        |> insert pid (idx + delta)

    Nothing ->
      model ! [prevCmd]


insertAbove : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertAbove id tup =
  insertRelative id 0 tup


insertBelow : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertBelow id tup =
  insertRelative id 1 tup


insertChild : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertChild id (model, prevCmd) =
  model ! [prevCmd]
    |> insert id 999999


-- === Card Moving  ===

move : Tree -> String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
move subtree pid pos (model, prevCmd) =
  { model
    | workingTree = Trees.update (Trees.Mov subtree pid pos) model.workingTree
  }
    ! []
    |> activate subtree.id
    |> addToHistory


moveWithin : String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
moveWithin id delta (model, prevCmd) =
  let
    tree_ =
      getTree id model.workingTree.tree

    pid_ =
      getParent id model.workingTree.tree
        |> Maybe.map .id

    refIdx_ =
      getIndex id model.workingTree.tree
  in
  case (tree_, pid_, refIdx_) of
    (Just tree, Just pid, Just refIdx) ->
      model ! [prevCmd]
        |> move tree pid (refIdx + delta |> Basics.max 0)
    _ -> model ! [prevCmd]


moveLeft : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
moveLeft id (model, prevCmd) =
  let
    tree_ =
      getTree id model.workingTree.tree

    parentId =
      getParent id model.workingTree.tree
        |> Maybe.map .id
        |> Maybe.withDefault "invalid"

    parentIdx_ =
      getIndex parentId model.workingTree.tree

    grandparentId_ =
      getParent parentId model.workingTree.tree
        |> Maybe.map .id
  in
  case (tree_, grandparentId_, parentIdx_) of
    (Just tree, Just gpId, Just refIdx) ->
      model ! [prevCmd]
        |> move tree gpId (refIdx+1)
    _ -> model ! [prevCmd]


moveRight : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
moveRight id (model, prevCmd) =
  let
    tree_ =
      getTree id model.workingTree.tree

    prev_ =
      getPrev id model.workingTree.tree
        |> Maybe.map .id
  in
  case (tree_, prev_) of
    (Just tree, Just prev) ->
      model ! [prevCmd]
        |> move tree prev 999999
    _ -> model ! [prevCmd]


-- === History ===

push : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
push (model, prevCmd) =
  if model.online then
    model ! [prevCmd, js ("push", null)]
  else
    model ! [prevCmd]


addToHistory : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToHistory ({workingTree} as model, prevCmd) =
  case model.status of
    Bare ->
      let
        (newStatus, _, newObjects) =
          Objects.update (Objects.Commit [] "Jane Doe <jane.doe@gmail.com>" workingTree.tree) model.objects
      in
      { model
        | objects = newObjects
        , status = newStatus
        , changed = True
      }
        ! [ saveObjects (newStatus |> statusToValue, newObjects |> Objects.toValue)
          , updateCommits (newObjects |> Objects.toValue, getHead newStatus)
          , js ("changed", null)
          ]

    Clean oldHead ->
      let
        (newStatus, _, newObjects) =
          Objects.update (Objects.Commit [oldHead] "Jane Doe <jane.doe@gmail.com>" workingTree.tree) model.objects
      in
      { model
        | objects = newObjects
        , status = newStatus
        , changed = True
      }
        ! [ saveObjects (newStatus |> statusToValue, newObjects |> Objects.toValue)
          , updateCommits (newObjects |> Objects.toValue, getHead newStatus)
          , js ("changed", null)
          ]

    MergeConflict _ oldHead newHead conflicts ->
      if (List.isEmpty conflicts || (conflicts |> List.filter (not << .resolved) |> List.isEmpty)) then
        let
          (newStatus, _, newObjects) =
            Objects.update (Objects.Commit [oldHead, newHead] "Jane Doe <jane.doe@gmail.com>" workingTree.tree) model.objects
        in
        { model
          | objects = newObjects
          , status = newStatus
          , changed = True
        }
          ! [ saveObjects (newStatus |> statusToValue, newObjects |> Objects.toValue)
            , updateCommits (newObjects |> Objects.toValue, getHead newStatus)
            , js ("changed", null)
            ]
      else
        model
          ! [saveLocal ( model.workingTree.tree |> treeToValue )]




-- === Files ===

intentSave : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
intentSave (model, prevCmd) =
  case (model.filepath, model.changed) of
    (Nothing, True) ->
      model ! [prevCmd, js ("save-as", null)]

    (Just filepath, True) ->
      model ! [prevCmd, js ("save", filepath |> string)]

    _ ->
      model ! [prevCmd]


intentNew : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
intentNew (model, prevCmd) =
  case model.filepath of
    Nothing ->
      model ! [prevCmd, js ("new", null)]

    Just filepath ->
      model ! [prevCmd, js ("new", filepath |> string)]


intentOpen : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
intentOpen (model, prevCmd) =
  case (model.filepath, model.changed) of
    (Just filepath, True) ->
      model ! [prevCmd, js ("open", filepath |> string)]

    _ ->
      model ! [prevCmd, js ("open", null)]


sendCollabState : CollabState -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
sendCollabState collabState (model, prevCmd) =
  case model.status of
    MergeConflict _ _ _ _ ->
      model ! [ prevCmd ]

    _ ->
      model ! [ prevCmd, js ("socket-send", collabState |> collabStateToValue) ]



-- VIEW


view : Model -> Html Msg
view model =
  case model.status of
    MergeConflict _ oldHead newHead conflicts ->
      let
        bgString = """
repeating-linear-gradient(-45deg
, rgba(255,255,255,0.02)
, rgba(255,255,255,0.02) 15px
, rgba(0,0,0,0.025) 15px
, rgba(0,0,0,0.06) 30px
)
          """
      in
      div
        [ id "root"
        , style  [ ("background", bgString)
                , ("position", "absolute")
                , ("width", "100%")
                , ("height", "100%")
                ]
        ]
        [ ul [class "conflicts-list"]
              (List.map viewConflict conflicts)
        , (lazy2 Trees.view model.viewState model.workingTree)
        ]

    _ ->
      div
        [ id "root" ]
        [ lazy2 Trees.view model.viewState model.workingTree
        , viewFooter model
        ]


viewFooter : Model -> Html Msg
viewFooter model =
  let
    current = countWords ( treeToMarkdownString model.workingTree.tree )
    session = current - model.startingWordcount
  in
  div
    [ class "footer" ]
    ( if model.viewState.editing == Nothing then
        if model.startingWordcount /= 0 then
          let
            hoverStyle = [("height","48px")]
          in
          [ hover hoverStyle div [ id "wordcount", class "inset" ]
            [ viewWordcountProgress current session
            , span [][ text ( "Total: " ++ ( current |> toWordsString ) ) ]
            , span [][ text ( "Session: " ++ ( session |> toWordsString ) ) ]
            ]
          ]
        else
          [ div [ id "wordcount", class "inset" ]
            [ span [][ text ( "Total: " ++ ( current |> toWordsString ) ) ] ]
          ]
      else
        []
    )


viewWordcountProgress : Int -> Int -> Html Msg
viewWordcountProgress current session =
  let
    currW =
      1/(1+(toFloat session)/(toFloat current))

    sessW =
      1-currW
  in
  div [ id "wc-progress" ]
    [ div [ id "wc-progress-wrap" ]
      [ span [ style [("flex", toString currW)], id "wc-progress-bar" ][]
      , span [ style [("flex", toString sessW)], id "wc-progress-bar-session" ][]
      ]
    ]


countWords : String -> Int
countWords str =
  let
    punctuation = regex "[!@#$%^&*():;\"',.]+"
  in
  str
    |> String.toLower
    |> replace Regex.All punctuation (\_ -> "")
    |> String.words
    |> List.filter ((/=) "")
    |> List.length


toWordsString : Int -> String
toWordsString num =
  case num of
    1 ->
      "1 word"
    n ->
      (toString n) ++ " words"


viewConflict: Conflict -> Html Msg
viewConflict {id, opA, opB, selection, resolved} =
  let
    withManual cardId oursElement theirsElement =
      li
        []
        [ fieldset []
            [ radio (SetSelection id Original cardId) (selection == Original) (text "Original")
            , radio (SetSelection id Ours cardId) (selection == Ours) (oursElement)
            , radio (SetSelection id Theirs cardId) (selection == Theirs) theirsElement
            , radio (SetSelection id Manual cardId) (selection == Manual) (text "Merged")
            , label []
               [ input [ checked resolved , type_ "checkbox" , onClick (Resolve id) ][]
               , text "Resolved"
               ]
            ]
        ]

    withoutManual cardIdA cardIdB =
      li
        []
        [ fieldset []
            [ radio (SetSelection id Original "") (selection == Original) (text "Original")
            , radio (SetSelection id Ours cardIdA) (selection == Ours) (text ("Ours:" ++ (toString opA |> String.left 3)))
            , radio (SetSelection id Theirs cardIdB) (selection == Theirs) (text ("Theirs:" ++ (toString opB |> String.left 3)))
            , label []
               [ input [ checked resolved , type_ "checkbox" , onClick (Resolve id) ][]
               , text "Resolved"
               ]
            ]
        ]

    newConflictView cardId ourChanges theirChanges =
      div [class "flex-row"]
        [ div [class "conflict-container flex-column"]
            [ div
                [ classList [("row option", True), ("selected", selection == Original)]
                , onClick (SetSelection id Original cardId)
                ]
                [ text "Original"]
            , div [class "row flex-row"]
                [ div
                    [ classList [("option", True), ("selected", selection == Ours)]
                    , onClick (SetSelection id Ours cardId)
                    ]
                    [ text "Ours"
                    , ul [class "changelist"] ourChanges
                    ]
                , div
                    [ classList [("option", True), ("selected", selection == Theirs)]
                    , onClick (SetSelection id Theirs cardId)
                    ]
                    [ text "Theirs"
                    , ul [class "changelist"] theirChanges
                    ]
                ]
            , div
                [ classList [("row option", True), ("selected", selection == Manual)]
                , onClick (SetSelection id Manual cardId)
                ]
                [ text "Merged"]
            ]
        , button [onClick (Resolve id)][text "Resolved"]
        ]
  in
  case (opA, opB) of
    (Mod idA _ strA orig, Mod _ _ strB _) ->
      let
        diffLinesString l r =
          diffLines l r
            |> List.filterMap
              (\c ->
                case c of
                  NoChange s -> Nothing
                  Added s -> Just (li [][ins [class "diff"][text s]])
                  Removed s -> Just (li [][del [class "diff"][text s]])
              )
      in
      newConflictView idA ([]) ([])

    (Types.Ins idA _ _ _, Del idB _) ->
      withoutManual idA idB

    (Del idA _, Types.Ins idB _ _ _) ->
      withoutManual idA idB

    _ ->
      withoutManual "" ""


radio : msg -> Bool -> Html msg -> Html msg
radio msg bool labelElement =
  label []
    [ input [ type_ "radio", checked bool, onClick msg ] []
    , labelElement
    ]




-- ENCODING/DECODING

modelToValue : Model -> Json.Value
modelToValue model =
  null




-- SUBSCRIPTIONS


port externals : ((String, String) -> msg) -> Sub msg
port load : ((Maybe String, Json.Value) -> msg) -> Sub msg
port merge : (Json.Value -> msg) -> Sub msg
port importJson : (Json.Value -> msg) -> Sub msg
port setHead : (String -> msg) -> Sub msg
port setHeadRev : (String -> msg) -> Sub msg
port keyboard : (String -> msg) -> Sub msg
port collabMsg : (Json.Value -> msg) -> Sub msg
port collabLeave : (String -> msg) -> Sub msg
port updateContent : ((String, String) -> msg) -> Sub msg
port cancelConfirmed : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ load Load
    , merge MergeIn
    , importJson ImportJson
    , setHead CheckoutCommit
    , setHeadRev SetHeadRev
    , keyboard HandleKey
    , collabMsg RecvCollabState
    , collabLeave CollaboratorDisconnected
    , updateContent UpdateContent
    , cancelConfirmed (\_ -> CancelCard)
    , externals ExternalMessage
    --, Time.every (1*Time.second) (\_ -> Pull)
    ]




-- HELPERS

getHead : Status -> Maybe String
getHead status =
  case status of
    Clean head ->
      Just head

    MergeConflict _ head _ [] ->
      Just head

    _ ->
      Nothing

focus : String -> Cmd Msg
focus id =
  Task.attempt (\_ -> NoOp) (Dom.focus ("card-edit-" ++ id))


run : Msg -> Cmd Msg
run msg =
  Task.attempt (\_ -> msg ) (Task.succeed msg)


maybeSaveAndThen : ( (Model, Cmd Msg) -> (Model, Cmd Msg) ) -> Model -> (Model, Cmd Msg)
maybeSaveAndThen operation model =
  case model.viewState.editing of
    Nothing ->
      model ! []
        |> operation

    Just uid ->
      model ! [getText uid]
        |> operation


normalMode : Model -> ( (Model, Cmd Msg) -> (Model, Cmd Msg) ) -> (Model, Cmd Msg)
normalMode model operation =
  model ! []
    |> if (model.viewState.editing == Nothing) then operation else identity
