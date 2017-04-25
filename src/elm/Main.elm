port module Main exposing (..)


import Html exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Tuple exposing (first, second)
import Dict
import Json.Decode as Json
import Dom
import Task
import List.Extra as ListExtra

import Types exposing (..)
import Trees exposing (..)
import TreeUtils exposing (..)
import Coders exposing (nodeListDecoder, nodeObjectDecoder, nodeListToValue)
import Sha1 exposing (timeJSON)


main : Program Json.Value Model Msg
main =
  programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port activateCards : (Int, List (List String)) -> Cmd msg
port getText : String -> Cmd msg
port saveNodes : Json.Value -> Cmd msg




-- MODEL


type alias Model =
  { data : Trees.Model
  , viewState : ViewState
  }


defaultModel : Model
defaultModel =
  { data = Trees.defaultModel
  , viewState =
      { active = "0"
      , activePast = []
      , activeFuture = []
      , descendants = []
      , editing = Just "0"
      }
  }


init : Json.Value -> (Model, Cmd Msg)
init savedState =
  case Json.decodeValue nodeListDecoder savedState of
    Ok nodeList ->
      { defaultModel 
        | data = Trees.Model defaultTree [] (nodeList |> Dict.fromList) []
            |> Trees.updateData
      }
        ! []

    Err err ->
      let _ = Debug.log "nodes in err" err in
      defaultModel ! []




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    vs = model.viewState
    data = model.data
  in
  case msg of
    -- === Card Activation ===

    Activate id ->
      let
        activeTree_ = getTree id model.data.tree
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
              getAncestors model.data.tree activeTree []
                |> List.map .id

            flatCols =
              model.data.columns
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
                  ( getDepth 0 model.data.tree id
                  , centerlineIds flatCols allIds newPast
                  )
              ]

        Nothing ->
          model ! []

    GoLeft id ->
      let
        targetId =
          getParent id model.data.tree ? defaultTree |> .id
      in
      update (Activate targetId) model

    GoDown id ->
      let
        targetId =
          case getNextInColumn id model.data.tree of
            Nothing -> id
            Just ntree -> ntree.id
      in
      update (Activate targetId) model

    GoUp id ->
      let
        targetId =
          case getPrevInColumn id model.data.tree of
            Nothing -> id
            Just ntree -> ntree.id
      in
      update (Activate targetId) model

    GoRight id ->
      let
        tree_ =
          getTree id model.data.tree

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
      { model
        | viewState = { vs | active = id, editing = Just id }
      }
        ! [focus id]

    GetContentToSave id ->
      model ! [getText id]

    UpdateContent (id, str) ->
      { model
        | data = Trees.updatePending (Trees.Mod id str) model.data
        , viewState = { vs | active = id, editing = Nothing }
      }
        ! []
        |> andThen AttemptSave

    DeleteCard id ->
      let
        filteredActive =
          vs.activePast
            |> List.filter (\a -> a /= id)

        parent_ = getParent id model.data.tree
        prev_ = getPrevInColumn id model.data.tree
        next_ = getNextInColumn id model.data.tree

        desc =
          getTree id model.data.tree
            |> Maybe.map getDescendants
            |> Maybe.map (List.map .id)
            |> Maybe.withDefault []

        nextToActivate =
          case (parent_, prev_, next_) of
            (_, Just prev, _) ->
              prev.id

            (_, Nothing, Just next) ->
              next.id

            (Just parent, Nothing, Nothing) ->
              parent.id

            (Nothing, Nothing, Nothing) ->
              "0"
      in
      { model
        | data = Trees.updatePending (Trees.Rmv id desc) model.data
      }
        ! []
        |> andThen (Activate nextToActivate)
        |> andThen AttemptSave

    CancelCard ->
      { model
        | viewState = { vs | editing = Nothing }
      }
        ! []

    -- === Card Insertion  ===

    Insert pid pos ->
      let
        newId = "node-" ++ (timeJSON ())
      in
      { model
        | data = Trees.updatePending (Trees.Add newId pid pos) model.data
      }
        ! []
        |> andThen (OpenCard newId "")
        |> andThen (Activate newId)
        |> andThen AttemptSave

    InsertAbove id ->
      let
        idx =
          getIndex id model.data.tree ? 999999

        pid_ =
          getParent id model.data.tree |> Maybe.map .id

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
          getIndex id model.data.tree ? 999999

        pid_ =
          getParent id model.data.tree |> Maybe.map .id

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

    Move id pid pos ->
      { model
        | data = Trees.updatePending (Trees.Mv id pid pos) model.data
      }
        ! []
        |> andThen (Activate id)
        |> andThen AttemptSave

    MoveWithin id delta ->
      let
        tree_ =
          getTree id model.data.tree

        pid_ =
          getParent id model.data.tree
            |> Maybe.map .id

        refIdx_ =
          getIndex id model.data.tree
      in
      case (tree_, pid_, refIdx_) of
        (Just tree, Just pid, Just refIdx) ->
          update (Move tree.id pid (refIdx + delta |> max 0)) model
        _ -> model ! []

    MoveLeft id ->
      let
        tree_ =
          getTree id model.data.tree

        parentId =
          getParent id model.data.tree
            |> Maybe.map .id
            |> Maybe.withDefault "invalid"

        parentIdx_ =
          getIndex parentId model.data.tree

        grandparentId_ =
          getParent parentId model.data.tree
            |> Maybe.map .id
      in
      case (tree_, grandparentId_, parentIdx_) of
        (Just tree, Just gpId, Just refIdx) ->
          update (Move tree.id gpId (refIdx+1)) model
        _ -> model ! []

    MoveRight id ->
      let
        tree_ =
          getTree id model.data.tree

        prev_ =
          getPrev id model.data.tree
            |> Maybe.map .id
      in
      case (tree_, prev_) of
        (Just tree, Just prev) ->
          update (Move tree.id prev 999999) model
        _ -> model ! []

    -- === Ports ===

    NodesIn json ->
      let _ = Debug.log "json" json in
      init json

    ChangeIn json ->
      let _ = Debug.log "ChangeIn" json in
      case Json.decodeValue nodeObjectDecoder json of
        Ok (id, node) ->
          { model
            | data =
                { data
                  | nodes = Dict.insert id node data.nodes
                  , pending = data.pending
                      |> List.filter (\(i, _) -> i /= id)
                }
                  |> Trees.updateData
          }
            ! []

        Err err ->
          let _ = Debug.log "ChangeIn decode error:" err in
          model ! []

    ConflictsIn json ->
      let _ = Debug.log "ConflictsIn" json in
      case Json.decodeValue nodeListDecoder json of
        Ok nodeList ->
          let
            resolvedConflicts =
              nodeList
                |> Trees.resolve data.nodes
          in
          { model
            | data =
                { data
                  | pending =
                      data.pending ++ resolvedConflicts
                }
                  |> Trees.updateData
          }
            ! []
            |> andThen AttemptSave

        Err err ->
          let _ = Debug.log "ConflictsIn decode error:" err in
          model ! []

    AttemptSave ->
      model ! [saveNodes (model.data.pending |> nodeListToValue)]

    SaveResponses res ->
      let _ = Debug.log "SaveResponses" res in
      let
        okIds =
          res
            |> List.filter .ok
            |> List.map .id

        newPending =
          data.pending
            |> Debug.log "oldPending"
            |> List.filter (\(id, ptn) -> not <| List.member id okIds)
            |> Debug.log "newPending"

        modifiedNodes =
          res
            |> List.filter .ok
            |> List.filterMap
                (\r ->
                  data.pending
                    |> ListExtra.find (\(id, ptn) -> id == r.id)
                    |> Maybe.andThen (\(id, tn) -> Just (id, {tn | rev = Just r.rev }))
                )
            |> Dict.fromList
            |> Debug.log "modifiedNodes"

        newNodes =
          Dict.union modifiedNodes data.nodes
            |> Dict.filter (\id tn -> tn.deletedWith == Nothing )
            |> Debug.log "newNodes"
      in
      { model
        | data =
            { data
              | nodes = newNodes
              , pending = newPending
              }
                |> Trees.updateData
      }
        ! []

    HandleKey key ->
      case key of
        "mod+x" ->
          let _ = Debug.log "model" model in
          model ! []

        "mod+enter" ->
          editMode model (\id -> GetContentToSave id)

        "enter" ->
          normalMode model (OpenCard vs.active (getContent vs.active model.data.tree))

        "mod+backspace" ->
          normalMode model (DeleteCard vs.active)

        "esc" ->
          update CancelCard model

        "mod+j" ->
          update (InsertBelow vs.active) model

        "mod+down" ->
          update (InsertBelow vs.active) model

        "mod+k" ->
          update (InsertAbove vs.active) model

        "mod+up" ->
          update (InsertAbove vs.active) model

        "mod+l" ->
          update (InsertChild vs.active) model

        "mod+right" ->
          update (InsertChild vs.active) model

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

        _ ->
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




-- VIEW


view : Model -> Html Msg
view model =
  (lazy2 Trees.view model.viewState model.data)




-- SUBSCRIPTIONS


port nodes : (Json.Value -> msg) -> Sub msg
port change : (Json.Value -> msg) -> Sub msg
port conflicts : (Json.Value -> msg) -> Sub msg
port keyboard : (String -> msg) -> Sub msg
port updateContent : ((String, String) -> msg) -> Sub msg
port saveResponses : (List Response -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ nodes NodesIn
    , change ChangeIn
    , conflicts ConflictsIn
    , keyboard HandleKey
    , updateContent UpdateContent
    , saveResponses SaveResponses
    ]




-- HELPERS

focus : String -> Cmd Msg
focus id =
  Task.attempt (\_ -> NoOp) (Dom.focus ("card-edit-" ++ id))


run : Msg -> Cmd Msg
run msg =
  Task.attempt (\_ -> msg ) (Task.succeed msg)


editMode : Model -> (String -> Msg) -> (Model, Cmd Msg)
editMode model editing = 
  case model.viewState.editing of
    Nothing ->
      model ! []

    Just uid ->
      update (editing uid) model


normalMode : Model -> Msg -> (Model, Cmd Msg)
normalMode model msg = 
  case model.viewState.editing of
    Nothing ->
      update msg model

    Just _ ->
      model ! []
