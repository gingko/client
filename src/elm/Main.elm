port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Json.Encode exposing (..)
import Dom
import Task

import Types exposing (..)
import Trees exposing (..)
import TreeUtils exposing (..)
import UI exposing (viewFooter, viewVideo, viewConflict, countWords)
import Sha1 exposing (timestamp, timeJSON)
import Objects
import Ports exposing (..)
import Coders exposing (..)

import Html5.DragDrop as DragDrop


main : Program (Bool, Bool, Bool) Model Msg
main =
  programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { workingTree : Trees.Model
  , objects : Objects.Model
  , status : Status
  , uid : String
  , viewState : ViewState
  , field : String
  , isMac : Bool
  , shortcutTrayOpen : Bool
  , videoModalOpen : Bool
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
      , parent = "0"
      , editing = Nothing
      , dragModel = DragDrop.init
      , draggedTree = Nothing
      , collaborators = []
      }
  , field = ""
  , isMac = False
  , shortcutTrayOpen = True
  , videoModalOpen = False
  , startingWordcount = 0
  , online = False
  , filepath = Nothing
  , changed = False
  }


init : (Bool, Bool, Bool) -> (Model, Cmd Msg)
init (isMac, trayIsOpen, videoModalIsOpen) =
  { defaultModel
    | isMac = isMac
    , shortcutTrayOpen = trayIsOpen
    , videoModalOpen = videoModalIsOpen
  }
    ! [focus "1"]
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
      model ! []
        |> saveCardIfEditing
        |> activate id

    -- === Card Editing  ===

    OpenCard id str ->
      model ! []
        |> openCard id str

    DeleteCard id ->
      model ! []
        |> deleteCard id

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

    Sync ->
      case (model.status, model.online) of
        (Clean _, True) ->
          model ! [ sendOut Pull ]

        (Bare, True) ->
          model ! [ sendOut Pull ]

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

    -- === Help ===

    VideoModal shouldOpen ->
      model ! []
        |> toggleVideoModal shouldOpen

    ShortcutTrayToggle ->
      let newIsOpen = not model.shortcutTrayOpen in
      { model
        | shortcutTrayOpen = newIsOpen
      }
        ! [ sendOut ( SetShortcutTray newIsOpen ) ]


    -- === Ports ===


    Port incomingMsg ->
      case incomingMsg of
        -- === Dialogs, Menus, Window State ===

        IntentNew ->
          intentNew model

        IntentOpen ->
          intentOpen model

        IntentImport ->
          intentImport model

        IntentSave ->
          model ! []
            |> intentSave

        IntentSaveAs ->
          model ! []
            |> intentSaveAs

        IntentExport exportSettings ->
          case exportSettings.format of
            JSON ->
              case exportSettings.selection of
                All ->
                  model ! []
                    |> saveCardIfEditing
                    |> \(m, c) -> m ! [ c,  sendOut ( ExportJSON m.workingTree.tree m.filepath ) ]

                _ -> model ! []

            TXT ->
              case exportSettings.selection of
                All ->
                  model ! []
                    |> saveCardIfEditing
                    |> \(m, c) -> m ! [ c,  sendOut ( ExportTXT False m.workingTree.tree m.filepath ) ]

                CurrentSubtree ->
                  model ! []
                    |> saveCardIfEditing
                    |> \(m, c) -> m ! [ c,  sendOut ( ExportTXT True m.workingTree.tree m.filepath ) ]

                ColumnNumber col ->
                  model ! []
                    |> saveCardIfEditing
                    |> \(m, c) -> m ! [ c,  sendOut ( ExportTXTColumn col m.workingTree.tree m.filepath ) ]

        IntentExit ->
          intentExit model

        CancelCardConfirmed ->
          model ! []
          |> cancelCard

        -- === Database ===

        New ->
          actionNew model

        Open (filepath, json, lastActiveCard) ->
          let
            -- Reset model, while keeping flags intact
            baseModel =
              init (model.isMac, model.shortcutTrayOpen, model.videoModalOpen)
                |> Tuple.first

            (newStatus, newTree_, newObjects) =
                Objects.update (Objects.Init json) objects

            startingWordcount =
              newTree_
                |> Maybe.map (\t -> countWords (treeToMarkdownString False t))
                |> Maybe.withDefault 0
          in
          case (newStatus, newTree_) of
            (Clean newHead, Nothing) -> -- no changes to Tree
              { baseModel
                | status = newStatus
                , startingWordcount = startingWordcount
                , filepath = Just filepath
                , changed = False
              }
                ! [ sendOut ( UpdateCommits ( newObjects |> Objects.toValue, getHead newStatus ) ) ]
                |> maybeColumnsChanged model.workingTree.columns
                |> activate lastActiveCard

            (Clean newHead, Just newTree) ->
              { baseModel
                | workingTree = Trees.setTree newTree model.workingTree
                , objects = newObjects
                , status = newStatus
                , startingWordcount = startingWordcount
                , filepath = Just filepath
                , changed = False
              }
                ! [ sendOut ( UpdateCommits ( newObjects |> Objects.toValue, getHead newStatus ) ) ]
                |> maybeColumnsChanged model.workingTree.columns
                |> activate lastActiveCard

            (MergeConflict mTree oldHead newHead [], Just newTree) ->
              { baseModel
                | workingTree = Trees.setTree newTree model.workingTree
                , objects = newObjects
                , status = newStatus
                , startingWordcount = startingWordcount
                , filepath = Just filepath
                , changed = False
              }
                ! [ sendOut ( UpdateCommits ( newObjects |> Objects.toValue, getHead newStatus ) ) ]
                |> maybeColumnsChanged model.workingTree.columns
                |> activate lastActiveCard

            (MergeConflict mTree oldHead newHead conflicts, Just newTree) ->
              { baseModel
                | workingTree = Trees.setTreeWithConflicts conflicts mTree model.workingTree
                , objects = newObjects
                , status = newStatus
                , startingWordcount = startingWordcount
                , filepath = Just filepath
                , changed = False
              }
                ! [ sendOut ( UpdateCommits ( newObjects |> Objects.toValue, getHead newStatus ) ) ]
                |> maybeColumnsChanged model.workingTree.columns
                |> activate lastActiveCard

            _ ->
              let _ = Debug.log "failed to load json" (newStatus, newTree_, newObjects, json) in
              model ! []

        SetHeadRev rev ->
          { model
            | objects = Objects.setHeadRev rev model.objects
          }
            ! []
            |> push

        Merge json ->
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
                ! [ sendOut ( UpdateCommits ( Objects.toValue newObjects , Just sha ) ) ]
                |> activate vs.active

            (Clean oldHead, Clean newHead) ->
              if (oldHead /= newHead) then
                { model
                  | workingTree = Trees.setTree (newTree_ ? workingTree.tree) workingTree
                  , objects = newObjects
                  , status = newStatus
                }
                  ! [ sendOut ( UpdateCommits ( Objects.toValue newObjects , Just newHead ) ) ]
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
                ! [ sendOut ( UpdateCommits ( newObjects |> Objects.toValue, Just newHead ) ) ]
                |> addToHistory
                |> activate vs.active

            _ ->
              let _ = Debug.log "failed to merge json" json in
              model ! []

        ImportJSON json ->
          case Json.decodeValue treeDecoder json of
            Ok newTree ->
              let
                -- Reset model, while keeping flags intact
                baseModel =
                  init (model.isMac, model.shortcutTrayOpen, model.videoModalOpen)
                    |> Tuple.first

                (newStatus, _, newObjects) =
                  Objects.update (Objects.Commit [] "Jane Doe <jane.doe@gmail.com>" newTree) model.objects
              in
              { baseModel
                | workingTree = Trees.setTree newTree model.workingTree
                , objects = newObjects
                , status = newStatus
                , changed = True
              }
                ! [ sendOut ( SaveToDB ( statusToValue newStatus , Objects.toValue newObjects ) )
                  , sendOut ( UpdateCommits ( newObjects |> Objects.toValue, getHead newStatus ) )
                  ]
                  |> maybeColumnsChanged model.workingTree.columns
                  |> activate vs.active

            Err err ->
              let _ = Debug.log "ImportJson error" err in
              model ! []

        -- === File System ===

        FileState filepath_ changed ->
          { model
            | filepath = filepath_
            , changed = changed
          }
            ! []

        -- === DOM ===

        FieldChanged str ->
          { model
            | field = str
            , changed = True
          }
            ! []

        -- === UI ===

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
                    ! [ sendOut ( UpdateCommits ( Objects.toValue objects, getHead newStatus ) ) ]
                    |> maybeColumnsChanged model.workingTree.columns

                Nothing ->
                  model ! []
                    |> Debug.log "failed to load commit"

        ViewVideos ->
          model ! []
            |> toggleVideoModal True

        Keyboard shortcut ->
          case shortcut of
            "mod+x" ->
              let _ = Debug.log "model" model in
              model ! []

            "mod+enter" ->
              model ! []
                |> saveCardIfEditing
                |> activate vs.active

            "enter" ->
              normalMode model (openCard vs.active (getContent vs.active model.workingTree.tree))

            "mod+backspace" ->
              normalMode model (deleteCard vs.active)

            "esc" ->
              model |> intentCancelCard

            "mod+j" ->
              model ! []
                |> saveCardIfEditing
                |> insertBelow vs.active

            "mod+down" ->
              normalMode model (insertBelow vs.active)

            "mod+k" ->
              model ! []
                |> saveCardIfEditing
                |> insertAbove vs.active

            "mod+up" ->
              normalMode model (insertAbove vs.active)

            "mod+l" ->
              model ! []
                |> saveCardIfEditing
                |> insertChild vs.active

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
              intentNew model

            "mod+s" ->
              model ! []
                |> saveCardIfEditing
                |> intentSave

            "mod+o" ->
              intentOpen model

            "mod+b" ->
              case vs.editing of
                Nothing ->
                  model ! []

                Just uid ->
                  model ! [ sendOut ( TextSurround uid "**" ) ]

            "mod+i" ->
              case vs.editing of
                Nothing ->
                  model ! []

                Just uid ->
                  model ! [ sendOut ( TextSurround uid "*" ) ]

            _ ->
              let _ = Debug.log "unhandled shortcut" shortcut in
              model ! []

        -- === Misc ===

        RecvCollabState collabState ->
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

        CollaboratorDisconnected uid ->
          { model
            | viewState =
                { vs | collaborators = vs.collaborators |> List.filter (\c -> c.uid /= uid)}
          }
            ! []

    LogErr err ->
      model ! [ sendOut (ConsoleLogRequested err) ]

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

          parent =
            anc
              |> List.reverse
              |> List.head
              |> Maybe.withDefault "0"

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
                , parent = parent
              }
        }
          ! [ prevCmd
            , sendOut
              ( ActivateCards
                ( id
                , getDepth 0 model.workingTree.tree id
                , centerlineIds flatCols allIds newPast
                , model.filepath
                )
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

saveCardIfEditing : (Model, Cmd Msg) -> (Model, Cmd Msg)
saveCardIfEditing (model, prevCmd) =
  let vs = model.viewState in
  case vs.editing of
    Just id ->
      let
        newTree = Trees.update (Trees.Upd id model.field) model.workingTree
      in
      if newTree.tree /= model.workingTree.tree then
        { model
          | workingTree = newTree
          , viewState = { vs | editing = Nothing }
          , field = ""
        }
          ! [prevCmd]
          |> addToHistory
      else
        { model
          | viewState = { vs | editing = Nothing }
          , field = ""
        }
          ! [prevCmd]

    Nothing ->
      model ! [prevCmd]

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
    model ! [prevCmd, sendOut (Alert "Card is being edited by someone else.")]
  else
    { model
      | viewState = { vs | active = id, editing = Just id }
      , field = str
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
    model ! [ sendOut ( Alert "Card is being edited by someone else.") ]
  else if isLastChild then
    model ! [ sendOut ( Alert "Cannot delete last card.") ]
  else
    { model
      | workingTree = Trees.update (Trees.Rmv id) model.workingTree
    }
      ! [prevCmd]
      |> maybeColumnsChanged model.workingTree.columns
      |> activate nextToActivate
      |> addToHistory


cancelCard : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
cancelCard (model, prevCmd) =
  let vs = model.viewState in
  { model
    | viewState = { vs | editing = Nothing }
    , field = ""
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
      model ! [ sendOut (ConfirmCancelCard vs.active originalContent) ]


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
    |> maybeColumnsChanged model.workingTree.columns
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


maybeColumnsChanged : List Column -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
maybeColumnsChanged oldColumns ({workingTree} as model, prevCmd) =
  let
    oldColNumber = oldColumns |> List.length
    newColNumber = workingTree.columns |> List.length

    colsChangedCmd =
      if newColNumber /= oldColNumber then
        sendOut ( ColumnNumberChange ( newColNumber - 1 ) )
      else
        Cmd.none
  in
  model ! [prevCmd, colsChangedCmd]


-- === Card Moving  ===

move : Tree -> String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
move subtree pid pos (model, prevCmd) =
  { model
    | workingTree = Trees.update (Trees.Mov subtree pid pos) model.workingTree
  }
    ! [prevCmd]
    |> maybeColumnsChanged model.workingTree.columns
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
    model ! [ prevCmd, sendOut Push ]
  else
    model ! [ prevCmd ]


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
        ! [ prevCmd
          , sendOut ( SaveToDB ( statusToValue newStatus , Objects.toValue newObjects ) )
          , sendOut ( UpdateCommits ( Objects.toValue newObjects , getHead newStatus ) )
          , sendOut ( ChangeTitle model.filepath True )
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
        ! [ prevCmd
          , sendOut ( SaveToDB ( statusToValue newStatus , Objects.toValue newObjects ) )
          , sendOut ( UpdateCommits ( Objects.toValue newObjects , getHead newStatus ) )
          , sendOut ( ChangeTitle model.filepath True )
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
          ! [ prevCmd
            , sendOut ( SaveToDB ( statusToValue newStatus , Objects.toValue newObjects ) )
            , sendOut ( UpdateCommits ( Objects.toValue newObjects , getHead newStatus ) )
            , sendOut ( ChangeTitle model.filepath True )
            ]
      else
        model
          ! [ prevCmd, sendOut ( SaveLocal ( model.workingTree.tree ) ) ]




-- === Files ===


saveChangesDialog : String -> ( Model -> ( Model, Cmd Msg ) ) -> Model -> ( Model, Cmd Msg )
saveChangesDialog actionId actionFunction model =
  case (model.changed, model.viewState.editing) of
    ( False, _ ) ->
      actionFunction model

    ( True, Nothing ) ->
      let
        (status, objects) = ( statusToValue model.status, Objects.toValue model.objects )
      in
      model ! [ sendOut ( ConfirmClose actionId model.filepath (status, objects) ) ]

    ( True, Just eid ) ->
      let
        modelCardSaved =
          model ! []
            |> saveCardIfEditing
            |> Tuple.first

        (status, objects) = ( statusToValue modelCardSaved.status, Objects.toValue modelCardSaved.objects )
      in
      model ! [ sendOut ( ConfirmClose actionId modelCardSaved.filepath (status, objects) ) ]


intentNew : Model -> ( Model, Cmd Msg )
intentNew model =
  saveChangesDialog "New" actionNew model


intentOpen : Model -> ( Model, Cmd Msg )
intentOpen model =
  saveChangesDialog "Open" actionOpen model


intentImport : Model -> ( Model, Cmd Msg )
intentImport model =
  saveChangesDialog "Import" actionImport model


intentSave : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
intentSave (model, prevCmd) =
  model ! [ prevCmd, sendOut ( Save model.filepath ) ]


intentSaveAs : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
intentSaveAs (model, prevCmd) =
  model ! [ prevCmd, sendOut ( SaveAs model.filepath ) ]


intentExit : Model -> ( Model, Cmd Msg )
intentExit model =
  saveChangesDialog "Exit" actionExit model


actionNew : Model -> ( Model, Cmd Msg )
actionNew model =
  init (model.isMac, model.shortcutTrayOpen, model.videoModalOpen)
    |> maybeColumnsChanged model.workingTree.columns
    |> \(m, c) -> m ! [ sendOut ClearDB ]


actionOpen : Model -> ( Model, Cmd Msg )
actionOpen model =
  model ! [ sendOut ( OpenDialog model.filepath ) ]


actionImport : Model -> ( Model, Cmd Msg )
actionImport model =
  model ! [ sendOut ( ImportDialog model.filepath ) ]


actionExit : Model -> ( Model, Cmd Msg )
actionExit model =
  model ! [ sendOut Exit ]


sendCollabState : CollabState -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
sendCollabState collabState (model, prevCmd) =
  case model.status of
    MergeConflict _ _ _ _ ->
      model ! [ prevCmd ]

    _ ->
      model ! [ prevCmd, sendOut ( SocketSend collabState ) ]


toggleVideoModal : Bool -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
toggleVideoModal shouldOpen (model, prevCmd) =
  { model
    | videoModalOpen = shouldOpen
  }
    ! [ prevCmd, sendOut ( SetVideoModal shouldOpen ) ]




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
        , viewVideo model
        ]




-- ENCODING/DECODING

modelToValue : Model -> Json.Value
modelToValue model =
  null




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  receiveMsg Port LogErr




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


normalMode : Model -> ( (Model, Cmd Msg) -> (Model, Cmd Msg) ) -> (Model, Cmd Msg)
normalMode model operation =
  model ! []
    |> if (model.viewState.editing == Nothing) then operation else identity
