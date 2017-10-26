port module Main exposing (..)


import Tuple exposing (first, second)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Json.Encode exposing (..)
import Dom
import Task
--import Time
import Diff exposing (..)

import Types exposing (..)
import Trees exposing (..)
import TreeUtils exposing (..)
import Sha1 exposing (timestamp, timeJSON)
import Objects
import Coders exposing (..)


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
      , collaborators = []
      }
  , online = True
  , filepath = Nothing
  , changed = False
  }


init : (Model, Cmd Msg)
init =
  defaultModel
    ! [focus "1"]
    |> andThen (Activate "1")




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({objects, workingTree, status} as model) =
  let
    vs = model.viewState
  in
  case msg of
    -- === Card Activation ===

    Activate id ->
      if id == "0" then
        model ! []
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
              ! [ activateCards
                    ( getDepth 0 model.workingTree.tree id
                    , centerlineIds flatCols allIds newPast
                    )
                ]
                |> andThen (SendCollabState (CollabState model.uid (Active id) ""))

          Nothing ->
            model ! []

    GoLeft id ->
      let
        targetId =
          getParent id model.workingTree.tree ? defaultTree |> .id
      in
      update (Activate targetId) model

    GoDown id ->
      let
        targetId =
          case getNextInColumn id model.workingTree.tree of
            Nothing -> id
            Just ntree -> ntree.id
      in
      update (Activate targetId) model

    GoUp id ->
      let
        targetId =
          case getPrevInColumn id model.workingTree.tree of
            Nothing -> id
            Just ntree -> ntree.id
      in
      update (Activate targetId) model

    GoRight id ->
      let
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
          model ! []

        Just tree ->
          if List.length childrenIds == 0 then
            model ! []
          else
            update (Activate prevActiveOfChildren) model


    -- === Card Editing  ===

    OpenCard id str ->
      let
        isLocked =
          vs.collaborators
            |> List.filter (\c -> c.mode == Editing id)
            |> (not << List.isEmpty)
      in
      if isLocked then
        model ! [js ("alert", string "Card is being edited by someone else.")]
      else
        { model
          | viewState = { vs | active = id, editing = Just id }
        }
          ! [focus id]
          |> andThen (SendCollabState (CollabState model.uid (Editing id) str))

    GetContentToSave id ->
      model ! [getText id]

    UpdateContent (id, str) ->
      let
        newTree = Trees.update (Trees.Upd id str) model.workingTree
      in
      if newTree.tree /= model.workingTree.tree then
        { model
          | workingTree = newTree
        }
          ! []
          |> andThen Save
          |> andThen (SendCollabState (CollabState model.uid (Active id) ""))
      else
        model
          ! []
          |> andThen (SendCollabState (CollabState model.uid (Active id) ""))

    DeleteCard id ->
      let
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
          |> andThen (Activate nextToActivate)
          |> andThen Save

    IntentCancelCard ->
      let
        originalContent = getContent vs.active model.workingTree.tree
      in
      model ! [js ("confirm-cancel", Json.Encode.list [string vs.active, string originalContent])]

    CancelCard ->
      { model
        | viewState = { vs | editing = Nothing }
      }
        ! []
        |> andThen (SendCollabState (CollabState model.uid (Active vs.active) ""))

    -- === Card Insertion  ===

    Insert pid pos ->
      let
        newId = "node-" ++ (timestamp () |> toString)
      in
      { model
        | workingTree = Trees.update (Trees.Ins newId "" pid pos) model.workingTree
      }
        ! []
        |> andThen (OpenCard newId "")
        |> andThen (Activate newId)

    InsertAbove id ->
      let
        idx =
          getIndex id model.workingTree.tree ? 999999

        pid_ =
          getParent id model.workingTree.tree |> Maybe.map .id

        insertMsg =
          case pid_ of
            Just pid ->
              Insert pid idx

            Nothing ->
              NoOp
      in
      update insertMsg model

    InsertBelow id ->
      let
        idx =
          getIndex id model.workingTree.tree ? 999999

        pid_ =
          getParent id model.workingTree.tree |> Maybe.map .id

        insertMsg =
          case pid_ of
            Just pid ->
              Insert pid (idx+1)

            Nothing ->
              NoOp
      in
      update insertMsg model

    InsertChild id ->
      update (Insert id 999999) model

    -- === Card Moving  ===

    Move subtree pid pos ->
      { model
        | workingTree = Trees.update (Trees.Mov subtree pid pos) model.workingTree
      }
        ! []
        |> andThen (Activate subtree.id)
        |> andThen Save

    MoveWithin id delta ->
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
          update (Move tree pid (refIdx + delta |> Basics.max 0)) model
        _ -> model ! []

    MoveLeft id ->
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
          update (Move tree gpId (refIdx+1)) model
        _ -> model ! []

    MoveRight id ->
      let
        tree_ =
          getTree id model.workingTree.tree

        prev_ =
          getPrev id model.workingTree.tree
            |> Maybe.map .id
      in
      case (tree_, prev_) of
        (Just tree, Just prev) ->
          update (Move tree prev 999999) model
        _ -> model ! []

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


    Push ->
      if model.online then
        model ! [js ("push", null)]
      else
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
                |> andThen CancelCard
                |> andThen (Activate id)

        _ ->
          model ! []

    Resolve cid ->
      case status of
        MergeConflict mTree shaA shaB conflicts ->
          { model
            | status = MergeConflict mTree shaA shaB (conflicts |> List.filter (\c -> c.id /= cid))
          }
            ! []
            |> andThen Save

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
                ! []
                |> andThen (UpdateCommits (objects |> Objects.toValue, getHead newStatus))

            Nothing ->
              model ! []
                |> Debug.log "failed to load commit"

    -- === Files ===

    IntentNew ->
      case model.filepath of
        Nothing ->
          model ! [js ("new", null)]

        Just filepath ->
          model ! [js ("new", filepath |> string)]

    IntentSave ->
      case (model.filepath, model.changed) of
        (Nothing, True) ->
          model ! [js ("save-as", null)]

        (Just filepath, True) ->
          model ! [js ("save", filepath |> string)]

        _ ->
          model ! []

    IntentOpen ->
      case (model.filepath, model.changed) of
        (Just filepath, True) ->
          model ! [js ("open", filepath |> string)]

        _ ->
          model ! [js ("open", null)]


    -- === Ports ===

    Load (filepath, json) ->
      let
        (newStatus, newTree_, newObjects) =
            Objects.update (Objects.Init json) objects
      in
      case (newStatus, newTree_) of
        (Clean newHead, Nothing) -> -- no changes to Tree
          { model
            | status = newStatus
            , filepath = filepath
            , changed = False
          }
            ! []
            |> andThen (UpdateCommits (newObjects |> Objects.toValue, getHead newStatus))

        (Clean newHead, Just newTree) ->
          { model
            | workingTree = Trees.setTree newTree model.workingTree
            , objects = newObjects
            , status = newStatus
            , filepath = filepath
            , changed = False
          }
            ! []
            |> andThen (UpdateCommits (newObjects |> Objects.toValue, getHead newStatus))

        (MergeConflict mTree oldHead newHead [], Just newTree) ->
          { model
            | workingTree = Trees.setTree newTree model.workingTree
            , objects = newObjects
            , status = newStatus
            , filepath = filepath
            , changed = False
          }
            ! []
            |> andThen (UpdateCommits (newObjects |> Objects.toValue, getHead newStatus))

        (MergeConflict mTree oldHead newHead conflicts, Just newTree) ->
          { model
            | workingTree = Trees.setTreeWithConflicts conflicts mTree model.workingTree
            , objects = newObjects
            , status = newStatus
            , filepath = filepath
            , changed = False
          }
            ! []
            |> andThen (UpdateCommits (newObjects |> Objects.toValue, getHead newStatus))

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
            ! []
            |> andThen (UpdateCommits (newObjects |> Objects.toValue, Just sha))
            |> andThen (Activate vs.active)

        (Clean oldHead, Clean newHead) ->
          if (oldHead /= newHead) then
            { model
              | workingTree = Trees.setTree (newTree_ ? workingTree.tree) workingTree
              , objects = newObjects
              , status = newStatus
            }
              ! []
              |> andThen (UpdateCommits (newObjects |> Objects.toValue, Just newHead))
              |> andThen (Activate vs.active)
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
            ! []
            |> andThen Save
            |> andThen (UpdateCommits (newObjects |> Objects.toValue, Just newHead))
            |> andThen (Activate vs.active)

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
            ! [saveObjects (newStatus |> statusToValue, newObjects |> Objects.toValue)]
            |> andThen (UpdateCommits ( newObjects |> Objects.toValue, getHead newStatus))

        Err err ->
          let _ = Debug.log "ImportJson error" err in
          model ! []

    SetHeadRev rev ->
      { model
        | objects = Objects.setHeadRev rev model.objects
      }
        ! []
        |> andThen Push

    UpdateCommits (json, sha_) ->
      model ! [updateCommits (json, sha_)]

    Save ->
      case status of
        Bare ->
          let
            (newStatus, _, newObjects) =
              Objects.update (Objects.Commit [] "Jane Doe <jane.doe@gmail.com>" workingTree.tree) model.objects
          in
          { model
            | objects = newObjects
            , status = newStatus
          }
            ! [saveObjects (newStatus |> statusToValue, newObjects |> Objects.toValue)]
            |> andThen (UpdateCommits (newObjects |> Objects.toValue, getHead newStatus))

        Clean oldHead ->
          let
            (newStatus, _, newObjects) =
              Objects.update (Objects.Commit [oldHead] "Jane Doe <jane.doe@gmail.com>" workingTree.tree) model.objects
          in
          { model
            | objects = newObjects
            , status = newStatus
          }
            ! [saveObjects (newStatus |> statusToValue, newObjects |> Objects.toValue)]
            |> andThen (UpdateCommits (newObjects |> Objects.toValue, getHead newStatus))

        MergeConflict _ oldHead newHead conflicts ->
          if (List.isEmpty conflicts || (conflicts |> List.filter (not << .resolved) |> List.isEmpty)) then
            let
              (newStatus, _, newObjects) =
                Objects.update (Objects.Commit [oldHead, newHead] "Jane Doe <jane.doe@gmail.com>" workingTree.tree) model.objects
            in
            { model
              | objects = newObjects
              , status = newStatus
            }
              ! [saveObjects (newStatus |> statusToValue, newObjects |> Objects.toValue)]
              |> andThen (UpdateCommits (newObjects |> Objects.toValue, getHead newStatus))
          else
            model
              ! [saveLocal ( model.workingTree.tree |> treeToValue )]

    SendCollabState collabState ->
      case model.status of
        MergeConflict _ _ _ _ ->
          model ! []

        _ ->
          model ! [js ("socket-send", collabState |> collabStateToValue)]

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
              update (GetContentToSave uid) model
                |> andThen CancelCard
                |> andThen (Activate uid)

        "enter" ->
          normalMode model (OpenCard vs.active (getContent vs.active model.workingTree.tree))

        "mod+backspace" ->
          normalMode model (DeleteCard vs.active)

        "esc" ->
          case model.viewState.editing of
            Nothing ->
              model ! []

            Just id ->
              update IntentCancelCard model

        "mod+j" ->
          model |> maybeSaveAndThen (InsertBelow vs.active)

        "mod+down" ->
          model |> maybeSaveAndThen (InsertBelow vs.active)

        "mod+k" ->
          model |> maybeSaveAndThen (InsertAbove vs.active)

        "mod+up" ->
          model |> maybeSaveAndThen (InsertAbove vs.active)

        "mod+l" ->
          model |> maybeSaveAndThen (InsertChild vs.active)

        "mod+right" ->
          normalMode model (InsertChild vs.active)

        "h" ->
          normalMode model (GoLeft vs.active)

        "left" ->
          normalMode model (GoLeft vs.active)

        "j" ->
          normalMode model (GoDown vs.active)

        "down" ->
          normalMode model (GoDown vs.active)

        "k" ->
          normalMode model (GoUp vs.active)

        "up" ->
          normalMode model (GoUp vs.active)

        "l" ->
          normalMode model (GoRight vs.active)

        "right" ->
          normalMode model (GoRight vs.active)

        "alt+up" ->
          normalMode model (MoveWithin vs.active -1)

        "alt+down" ->
          normalMode model (MoveWithin vs.active 1)

        "alt+left" ->
          normalMode model (MoveLeft vs.active)

        "alt+right" ->
          normalMode model (MoveRight vs.active)

        "alt+shift+up" ->
          normalMode model (MoveWithin vs.active -5)

        "alt+shift+down" ->
          normalMode model (MoveWithin vs.active 5)

        "alt+home" ->
          normalMode model (MoveWithin vs.active -999999)

        "alt+end" ->
          normalMode model (MoveWithin vs.active 999999)

        "mod+z" ->
          normalMode model Undo

        "mod+r" ->
          normalMode model Redo

        "mod+n" ->
          update IntentNew model

        "mod+s" ->
          case model.viewState.editing of
            Nothing ->
              update IntentSave model

            Just id ->
              update (GetContentToSave id) model
                |> andThen IntentSave

        "mod+o" ->
          update IntentOpen model

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


        _ ->
          let _ = Debug.log "Unknown external command" cmd in
          model ! []

    _ ->
      model ! []


andThen : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen msg (model, prevMsg) =
  let
    newStep =
      update msg model
  in
  ( first newStep, Cmd.batch [prevMsg, second newStep] )


onlyIf : Bool -> Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
onlyIf cond msg prevStep =
  if cond then
    prevStep
      |> andThen msg
  else
    prevStep


getHead : Status -> Maybe String
getHead status =
  case status of
    Clean head ->
      Just head

    MergeConflict _ head _ [] ->
      Just head

    _ ->
      Nothing




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
        [style  [ ("background", bgString)
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
        []
        [ lazy2 Trees.view model.viewState model.workingTree ]


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

focus : String -> Cmd Msg
focus id =
  Task.attempt (\_ -> NoOp) (Dom.focus ("card-edit-" ++ id))


run : Msg -> Cmd Msg
run msg =
  Task.attempt (\_ -> msg ) (Task.succeed msg)


maybeSaveAndThen : Msg -> Model -> (Model, Cmd Msg)
maybeSaveAndThen msg model =
  case model.viewState.editing of
    Nothing ->
      update msg model

    Just uid ->
      update (GetContentToSave uid) model
        |> andThen msg


normalMode : Model -> Msg -> (Model, Cmd Msg)
normalMode model msg =
  case model.viewState.editing of
    Nothing ->
      update msg model

    Just _ ->
      model ! []
