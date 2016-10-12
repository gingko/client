port module Main exposing (..)


import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Json.Encode
import Json.Decode as Json
import Dom
import Task
import Markdown
import List.Extra as ListExtra

import Types exposing (..)
import Coders exposing (..)
import Tree exposing (update, viewColumn, blankTree)
import TreeUtils exposing (..)


main : Program Json.Value
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port saveModel : Json.Encode.Value -> Cmd msg
port activateCards : List (List String) -> Cmd msg


-- MODEL


type alias Model =
  { contents : List Content
  , nodes : List Node
  , commits : List Commit
  , operations : List Op
  , tree : Tree
  , commit : String
  , floating : List Op
  , viewState : ViewState
  }


defaultModel : Model
defaultModel =
  { contents = [defaultContent]
  , nodes = [defaultNode]
  , commits = [defaultCommit]
  , operations = []
  , tree = Tree.default
  , floating = []
  , commit = defaultCommit.id
  , viewState = 
      { active = "0"
      , activePast = []
      , activeFuture = []
      , descendants = []
      , editing = Nothing
      , field = ""
      }
  }


init : Json.Value -> ( Model, Cmd Msg )
init savedState =
  case Json.decodeValue modelDecoder savedState of
    Ok model ->
      model ! []
    Err err ->
      let
        deb = Debug.log "err" err
      in
      defaultModel ! []




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    vs = model.viewState
  in
  case msg of
    NoOp ->
      model ! []

    CommitChanges ts ->
      let
        newContents =
          getContents model.tree
            |> List.filter (\c -> not (List.member c model.contents))

        newNodes =
          (treeToNodes []) model.tree
            |> List.filter (\n -> not (List.member n model.nodes))

        newCommit = 
          { id = "id"
          , rootNode = treeUid model.tree
          , timestamp = ts
          , authors = ["Adriano Ferrari <adriano.ferrari@gmail.com>"]
          , committer = "Adriano Ferrari <adriano.ferrari@gmail.com>"
          , parents = [model.commit]
          , message = "Default Commit message"
          }
            |> withCommitId

        newModel =
          { model
            | nodes = model.nodes ++ newNodes
            , contents = model.contents ++ newContents
            , commits = model.commits ++ [newCommit]
            , operations = model.operations ++ model.floating
            , floating = []
            , commit = newCommit.id
          }
      in
        newModel ! [ saveModel (modelToValue newModel) ]

    CheckoutCommit cid ->
      let
        nodeId =
          model.commits
            |> ListExtra.find (\c -> c.id == cid)
            |> Maybe.withDefault defaultCommit
            |> .rootNode

        newTree =
          buildStructure 
          nodeId 
          (Objects model.contents model.nodes model.commits model.operations)
          |> Tree.applyOperations model.floating

        newModel =
          { model
            | commit = cid
            , tree = newTree
          }
      in
      newModel ! [ saveModel (modelToValue newModel) ]

    Activate uid ->
      let
        desc =
          model.tree
            |> flip getTree uid
            |> Maybe.withDefault Tree.default
            |> getDescendants
            |> List.map (\t -> t.uid)
      in
      { model
        | viewState = 
            { active = uid
            , activePast = vs.active :: vs.activePast
            , activeFuture = vs.activeFuture
            , descendants = desc
            , editing = vs.editing 
            , field = vs.field
            }
      }
        ! [ activateCards (centerlineIds model.tree (getTree model.tree uid |> Maybe.withDefault Tree.default) ) ]

    ActivatePast ->
      if List.isEmpty vs.activePast then
        model ! []
      else
      let
        targetId =
          vs.activePast
            |> List.head
            |> Maybe.withDefault vs.active

        newPast =
          List.drop 1 vs.activePast

        newFuture = vs.active :: vs.activeFuture

        newViewState v =
          { v
            | active = targetId
            , activePast = newPast
            , activeFuture = newFuture
          }
      in
      { model | viewState = newViewState vs } ! []
      
        
    ActivateFuture ->
      if List.isEmpty vs.activeFuture then
        model ! []
      else
      let
        targetId =
          vs.activeFuture
            |> List.head
            |> Maybe.withDefault vs.active

        newFuture =
          List.drop 1 vs.activeFuture

        newPast = vs.active :: vs.activePast

        newViewState v =
          { v
            | active = targetId
            , activePast = newPast
            , activeFuture = newFuture
          }
      in
      { model | viewState = newViewState vs } ! []

    OpenCard uid str ->
      { model
        | viewState =
            { active = vs.active
            , activePast = []
            , activeFuture = []
            , descendants = []
            , editing = (Just uid)
            , field = str
            }
      }
        ! [focus uid] 

    UpdateField str ->
      { model
        | viewState =
            { active = vs.active
            , activePast = []
            , activeFuture = []
            , descendants = []
            , editing = vs.editing
            , field = str
            }
      }
        ! []

    UpdateCard uid str ->
      let
        newOp = Upd "" uid str |> withOpId
        newViewState vs =
          { vs
            | active = uid
            , editing = Nothing
            , field = ""
          }
      in
      sequence model newViewState newOp []

    DeleteCard uid ->
      let
        newOp = Del "" uid |> withOpId
      in
      sequence model identity newOp []

    CancelCard ->
      { model
        | viewState =
            { active = vs.active
            , activePast = []
            , activeFuture = []
            , descendants = []
            , editing = Nothing
            , field = ""
            }
      }
        ! []

    InsertAbove uid ->
      let
        parentId_ = getParentId model.tree uid
        prevId_ = getPrev model.tree uid
        nextId_ = Just uid
        newId = newUid parentId_ prevId_ nextId_
        newOp = Ins newId parentId_ prevId_ nextId_
        newViewState vs =
          { vs 
            | active = newId
            , editing = Just newId
            , field = ""
          }
            
      in
        sequence model newViewState newOp [focus newId]

    InsertBelow uid ->
      let
        parentId_ = getParentId model.tree uid
        prevId_ = Just uid
        nextId_ = getNext model.tree uid
        newId = newUid parentId_ prevId_ nextId_
        newOp = Ins newId parentId_ prevId_ nextId_
        newViewState vs =
          { vs
            | active = newId
            , editing = Just newId
            , field = ""
          }
      in
        sequence model newViewState newOp [focus newId]

    InsertChild uid ->
      let
        oldTree = model.tree
        parentId_ = Just uid
        prevId_ = getLastChild model.tree uid
        nextId_ = Nothing
        newId = newUid parentId_ prevId_ nextId_
        newOp = Ins newId parentId_ prevId_ nextId_
        newViewState vs =
          { vs
            | active = newId
            , editing = Just newId
            , field = ""
          }
      in
        sequence model newViewState newOp [focus newId]

    GoLeft uid ->
      let
        targetId =
          getParent model.tree uid
            |> Maybe.withDefault (blankTree uid)
            |> .uid
      in
      update (Activate targetId) model

    GoDown uid ->
      let
        targetId =
          getNext model.tree uid
            |> Maybe.withDefault uid
      in
      update (Activate targetId) model

    GoUp uid ->
      let
        targetId =
          getPrev model.tree uid
            |> Maybe.withDefault uid
      in
      update (Activate targetId) model

    GoRight uid ->
      let
        tree =
          getTree model.tree uid -- Maybe Tree
            |> Maybe.withDefault (blankTree uid) -- Tree

        childrenIds =
          tree
            |> getChildren -- List Tree
            |> List.map .uid -- List String

        firstChild = 
          getFirstChild model.tree uid |> Maybe.withDefault uid

        prevActiveOfChildren =
          vs.activePast
            |> List.filter (\a -> List.member a childrenIds) -- children in activePast
            |> List.head
            |> Maybe.withDefault firstChild
      in
      update (Activate prevActiveOfChildren) model

    OpIn json ->
      case (Json.decodeValue opDecoder json) of
        Ok op ->
          { model
            | tree = Tree.update (Tree.Apply op) model.tree
          }
            ! []

        Err err ->
          let
            deb = Debug.log "err" err
          in
          model ! []

    ExternalCommand (cmd, arg) ->
      case cmd of
        "commit-changes" ->
          model ! [run (CommitChanges (arg |> String.toInt |> Result.withDefault 0))]
        "checkout-commit" ->
          model ! [run (CheckoutCommit arg)]
        "keyboard" ->
          model ! [run (HandleKey arg)]
        _ ->
          model ! []
    
    HandleKey str ->
      let
        vs = model.viewState
      in
      case str of
        "mod+x" ->
          let
            deb = Debug.log "model: " model
          in
          model ! [saveModel (modelToValue model)]

        "mod+enter" ->
          editMode model
            (\uid -> UpdateCard uid vs.field)

        "enter" ->
          normalMode model
            (OpenCard vs.active 
              (getContent model.tree vs.active
                |> Maybe.withDefault defaultContent 
                |> .content
              )
            )

        "esc" ->
          editMode model (\_ -> CancelCard )

        "mod+backspace" ->
          normalMode model
            (DeleteCard vs.active)

        "mod+j" ->
          normalMode model
            (InsertBelow vs.active)

        "mod+k" ->
          normalMode model
            (InsertAbove vs.active)

        "mod+l" ->
          normalMode model
            (InsertChild vs.active)

        "h" ->
          normalMode model
            (GoLeft vs.active)

        "j" ->
          normalMode model
            (GoDown vs.active)

        "k" ->
          normalMode model
            (GoUp vs.active)
  
        "l" ->
          normalMode model
            (GoRight vs.active)

        "[" ->
          normalMode model ActivatePast

        "]" ->
          normalMode model ActivateFuture

        "mod+s" ->
          normalMode model
            (CommitChanges 0)

        other ->
          let
            deb = Debug.log "keyboard" other
          in
          model ! []





-- VIEW


view : Model -> Html Msg
view model =
  let
    columns =
      getColumns([[[ model.tree ]]])
        |> List.map (viewColumn model.viewState)
  in
  div [id "app" ]
    ( columns
    ++ [ viewHistory (List.reverse model.floating) ]
    )


viewHistory : List Op -> Html Msg
viewHistory ops =
  div [id "history"]
      ( List.map viewOp ops
      ++ [ div [id "graph"][]]
      )


viewOp : Op -> Html Msg
viewOp op =
  case op of
    Ins oid parentId_ prevId_ nextId_ ->
      li [id ("op-" ++ oid)][text ("Insert:" ++ oid)]

    Upd oid uid str ->
      li [id ("op-" ++ oid)][text ("Update:" ++ str)]

    _ ->
      li [][]




-- SUBSCRIPTIONS

port externals : ((String, String) -> msg) -> Sub msg -- ~ Sub (String, String)
port opsIn : (Json.Encode.Value -> msg) -> Sub msg -- ~ Sub Op

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ externals ExternalCommand
    , opsIn OpIn
    ]




-- HELPERS

focus uid =
  Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus ("card-edit-" ++ uid)) 


run : Msg -> Cmd Msg
run msg =
  Task.perform (\_ -> NoOp) (\_ -> msg ) (Task.succeed msg)


sequence : Model -> (ViewState -> ViewState) -> Op -> List (Cmd Msg) -> (Model, Cmd Msg)
sequence model vsf op cmds =
  let
    newModel =
      { model
        | viewState = vsf model.viewState
        , tree = Tree.update (Tree.Apply op) model.tree
        , floating = model.floating ++ [op]
      }
  in
    newModel ! ([saveModel (modelToValue newModel)] ++ cmds)


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
